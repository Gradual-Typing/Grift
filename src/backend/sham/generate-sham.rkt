#lang typed/racket/no-check
#|------------------------------------------------------------------------------+
|pass: src/generate-sham
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
| Description: This pass generates two components one is the custom c-code that
| initializes the runtime correctly and contains the main function for the
| the program. The second is a sham IR which represents the user program.
| The main function calls into the sham IR.
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require
 "../../unique-identifiers.rkt"
 "../../language/primitives.rkt"
 "../../language/form-map.rkt"
 "../../errors.rkt"
 "../../casts/cast-profiler.rkt"
 "../../casts/constants-and-codes.rkt"
 "../../configuration.rkt"
 "../../language/forms.rkt"
 "../../macros.rkt"
 "../runtime-location.rkt"
 "../../logging.rkt"
 "../../lib/mutable-set.rkt"
 sham/ast
 sham/env)

(module+ test
  (require rackunit))

;; Only the pass is provided by this module
(provide generate-sham)

;; Build a sham AST from our IR language post specify representation
(define (generate-sham prgm)
  (debug generate-sham prgm)
  (match-let ([(Prog (list name count type) (GlobDecs d* expr)) prgm])
    (define (dglob d) (dglobal #f (uid->symbol d) imdt-type))
    (define defs (box (map dglob d*)))
    (define expr^ (uncover-locals (list->mset d*) expr))
    (debug off generate-sham expr^)
    (define stmt (generate-program defs expr^))
    (define result
      (dmodule
       (hash)
       'grift_module
       ;; Note: grift_main is run for it's effects not return value
       (cons (dfunction #f 'grift_main '() '() tvoid (block^ stmt (ret-void)))
             (unbox defs))))
    (debug generate-sham result)))


;; Our IR currently uses Assign to introduce new variable
;; Sham requires variable to be introduced with a let expression first
;; This function introduces let binding for each assigned variable at
;; the beginning of each function body. We may want to move our IR to
;; require let binding before assignment so that we can hand off better
;; scoping information to LLVM.
(define (uncover-locals gd-set expr)
  (define null (Quote 0))
  (define (ul-scope expr)
    (define vars : (MSet Uid) (mset))
    (define (rec x)
      (match x
        [(Assign (? Uid? uid) e)
         (mset-add! vars uid)
         (Assign uid (rec e))]
        [(Repeat i e1 e2 a e3 e4) 
         (mset-add! vars i)
         (Repeat i (rec e1) (rec e2)
                 (and a (mset-add! vars a) a)
                 (and e3 (rec e3))
                 (rec e4))]
        ;; Start a new scope when entering local code bindings
        [(Labels (list (cons l* c*) ...) e)
         (Labels
          (for/list ([l l*] [c c*])
            (match-define (Code u* e) c)
            (cons l (Code u* (ul-scope e))))
          (rec e))]
        [other (form-map other rec)]))
    (define expr^ (rec expr))
    (mset-subtract! vars gd-set)
    (Let (for/list ([v (mset->list vars)]) (cons v null)) expr^))
  (ul-scope expr))

;; Generates the sham ast for the main statement of a gift program
;; lifted-code* = return parameter box containing a list of sham
;; function definitions that need to be present at the top of this
;; sham module. The function adds to this list.
;; stmt = the main statement of the grift module
(define (generate-program lifted-code* stmt [call-conv (calling-convention)])  
  (define (set-call-conv! rator)
    (set-calling-conv! rator call-conv)
    rator)
  ;; lift-code! is used to add function definitions to the lifted-code* parameter
  (define (lift-code! x)
    (set-box! lifted-code* (cons x (unbox lifted-code*))))

  ;; map over a list of code bindings and add them as function
  ;; definitions to the sham module we generate. 
  (define (generate-bnd-code* bnd*)
    (debug (map car bnd*))
    (for ([b bnd*])
      (match-define (cons u (Code u* t)) b)
      (debug u (Code u* t))
      (lift-code!
       (dfunction
        (function-info-set-call-conv (empty-function-info) call-conv)
        (uid->symbol u)
        (map uid->symbol u*)
        (for/list ([_ u*]) imdt-type)
        imdt-type
        (generate-tail t)))))

  ;; create a sham let expressions that uses a while loop to mimics
  ;; the semantics of a repeat loop.
  (define (generate-repeat index start stop acc init body)
    (define index-id (uid->symbol index))
    (define index-var (var index-id))
    (define stop-id (gensym 'stop))
    (define stop-var (var stop-id))
    (define acc-id (uid->symbol acc))
    (define acc-var (var acc-id))
    (sham:ast:expr:let
     (list index-id stop-id acc-id)
     (list imdt-type imdt-type imdt-type)
     (list (generate-value start) (generate-value stop) (generate-value init))
     (while^
       (icmp-slt index-var stop-var)
       (set!^ acc-var (generate-value body))
       (set!^ index-var (add index-var (si64 1))))
     acc-var))

  ;; Convert grift constants to sham constants
  (define (generate-constant k)
    (cond
      [(inexact-real? k) (fp->ui (cfl k f64) int-expr)]
      [(exact-integer? k) (csi k i64)]
      [(char? k)
       (define i (char->integer k))
       (if (<= 0 i 255)
           (cui i i64)
           (error 'generate-c/quote-char "currently only supports ASCII"))]
      [(string? k) (ptr->imdt (cstring k))]))
  
  ;; Take an grift expression in tail position and map it to a sham
  ;; statement in tail position.
  (define (generate-tail exp)
    (debug generate-tail exp)
    (match exp
      [(Labels bndc* exp)
       (generate-bnd-code* bndc*)
       (generate-tail exp)]
      [(Let (list (cons id* rhs*) ...) tail)
       (se
        (sham:ast:expr:let
         (map uid->symbol id*)
         (map (const imdt-type) id*)
         (map generate-value rhs*)
         (generate-tail tail)
         (evoid)))]
      [(If test consequence alternative)
       (sham:ast:stmt:if
        (imdt->bool (generate-value test))
        (generate-tail consequence)
        (generate-tail alternative))]
      [(Begin eff* exp)
       ;; should be equivalent to this
       ;; (block (append (map generate-effect eff*) (generate-tail exp)))
       (block
        (foldr
         (λ (e a) (cons (generate-effect e) a))
         (list (generate-tail exp))
         eff*))]
      [(Repeat i e1 e2 a e3 e4)
       (define acc-sym (gensym 'repeat-acc))
       (se
        (sham:ast:expr:let
         (list acc-sym)
         (list imdt-type)
         (list (generate-repeat i e1 e2 a e3 e4))
         (ret (var acc-sym))
         (evoid)))]
      [(Switch v (list (cons (list lhs** ...) rhs*) ...) default)
       (switch
        (generate-value v)
        (map (curry map (curryr csi i64)) lhs**)
        (map generate-tail rhs*)
        (generate-tail default))]
      [(and value-expr (or (Halt) (App-Code _ _) (Op _ _) (Var _) (Global _) (Quote _)))
       ;; we don't explicitly mark tail calls in sham
       (return (generate-value value-expr))]
      [(and other
            (or
             (While _ _)
             (Assign _ _)
             (No-Op)
             (Stack-Alloc _)))
       (error 'generate-tail "invalid return value: ~a" other)]))

  ;; takes a grift expression and maps it to sham expression
  (define (generate-value exp)
    (debug generate-value exp)
    (match exp
      [(Labels bndc* exp)
       (generate-bnd-code* bndc*)
       (generate-value exp)]
      [(Let (list (cons id* rhs*) ...) exp)
       (sham:ast:expr:let
        (map uid->symbol id*)
        (map (const imdt-type) id*)
        (map generate-value rhs*)
        (svoid)
        (generate-value exp))]
      [(If t c a)
       (define result (gensym 'if-result))
       (sham:ast:expr:let
        (list result)
        (list imdt-type)
        (list (si64 0))
        (if^ (imdt->bool (generate-value t))
             (set!^ (var result) (generate-value c))
             (set!^ (var result) (generate-value a)))
        (var result))]
      [(Begin eff* exp)
       (sham:ast:expr:let
        '()
        '()
        '()
        (block (map generate-effect eff*))
        (generate-value exp))]
      [(Repeat i e1 e2 a e3 e4)
       (generate-repeat i e1 e2 a e3 e4)]
      [(Break-Repeat)
       (sham:ast:expr:let '() '() '() (break) UNIT-EXPR)]
      [(App-Code (Code-Label l) exp*)
       (app (set-call-conv! (rs (uid->symbol l))) (map generate-value exp*))]
      [(App-Code exp exp*)
       (define rator (gensym 'rator))
       (define rator-type (tptr (tfun (map (const imdt-type) exp*) imdt-type)))
       (sham:ast:expr:let
        (list rator)
        (list rator-type)
        (list (int->ptr (generate-value exp) (etype rator-type)))
        (svoid)
        (app (set-call-conv! (rs rator)) (map generate-value exp*)))]
      [(Op p e*)
       (define ret (Fn-ret (grift-primitive->type p)))
       (define e*^ (map generate-value e*))
       (cond
         [(and (grift-primitive-effect? p)
               (or (Unit? ret) (Void? ret) (Bot? ret)))
          (sham:ast:expr:let
           '() '() '()
           (generate-stmt-op p e*^)
           UNIT-EXPR)]
         [else (generate-op p e*^)])]
      [(Stack-Alloc n)
       (ptr->imdt (arr-alloca imdt-expr (cui n imdt-type)))]
      [(Var i) (var (uid->symbol i))]
      [(Global s)
       (global s)]
      [(Code-Label i)
       (ptr->imdt (var (uid->symbol i)))]
      [(Quote k) (generate-constant k)]
      [(Halt)
       ;; The return type here is false, but this function doesn't return
       (app (re #f 'exit imdt-type) (csi -1 i32))]
      [(Switch v (list (cons (list lhs** ...) rhs*) ...) default)   
       (define result (gensym 'if-result))
       (sham:ast:expr:let
        (list result)
        (list imdt-type)
        (list (si64 0))
        (switch
         (generate-value v)
         (map (curry map (curryr csi i64)) lhs**)
         (for/list ([rhs rhs*]) (set!^ (var result) (generate-value rhs)))
         (set!^ (var result) (generate-value default)))
        (var result))]
      [(and other (or (While _ _) (Assign _ _) (No-Op)))
       (error 'generate-value "Invalid Value: ~a" other)]))

  ;; like generate-op but always returns a grift statement
  (define (generate-stmt-op p exp*)
    (match (generate-op p exp*)
      [(? sham:ast:expr? expr) (se expr)]
      [(? sham:ast:stmt? stmt) stmt]
      [other (error 'grift/src/backend/sham/generate-sham/generate-stmt-op)]))

  ;; Convert a grift expression in an effect context to a sham statement
  (define (generate-effect exp)
    (debug generate-effect exp)
    (match exp
      [(Assign u rhs)
       (set!^ (var (uid->symbol u)) (generate-value rhs))]
      [(Labels bndc* exp)
       (generate-bnd-code* bndc*)
       (generate-effect exp)]
      [(Let (list (cons id* rhs*) ...) body)
       (se
        (sham:ast:expr:let
         (map uid->symbol id*)
         (map (const imdt-type) id*)
         (map generate-value rhs*)
         (generate-effect body)
         (evoid)))]
      [(If test consequence alternative)
       (if^ (imdt->bool (generate-value test)) 
            (generate-effect consequence)
            (generate-effect alternative))]
      [(Begin eff* exp) 
       (block
        (foldr (λ (e a) (cons (generate-effect e) a))
               (list (generate-effect exp))
               eff*))]
      [(Repeat i e1 e2 a e3 e4)
       (se
        (generate-repeat i e1 e2 a e3 e4))]
      [(While e1 e2)
       (while (imdt->bool (generate-value e1))
         (generate-effect e2))]
      [(Break-Repeat) (break)]
      [(App-Code (Code-Label l) exp*)
       (app (set-call-conv! (rs (uid->symbol l))) (map generate-value exp*))]
      [(App-Code exp exp*)
       ;; the application form doesn't expect an expression for the operator
       (define rator (gensym 'rator))
       (define rator-type (tptr (tfun (map (const imdt-type) exp*) imdt-type)))
       (se
        (sham:ast:expr:let
         (list rator)
         (list rator-type)
         (list (int->ptr (generate-value exp) (etype rator-type)))
         (svoid)
         (app (set-call-conv! (rs rator)) (map generate-value exp*))))]
      [(Op p exp*) 
       (cond
         [(grift-primitive-effect? p)
          (generate-stmt-op p (map generate-value exp*))]
         [else ;; evaluate values for their effects
          (block (map generate-effect exp*))])]
      [(or
        (Stack-Alloc _)
        (Var _)
        (Global _)
        (Code-Label _)
        (Quote _)
        (No-Op))
       (svoid)]
      [(Switch v (list (cons (list lhs** ...) rhs*) ...) default)
       (switch
        (generate-value v)
        (map (curry map (curryr csi i64)) lhs**)
        (map generate-effect rhs*)
        (generate-effect default))]))

  ;; build a sham expression that allocates the argument size
  (define (alloc size) (app^ (re #f 'GC_malloc i64) size))
  
  ;; convert a grift op to the equivalent sham expression or statement
  (define (generate-op p exp*)
    (match* (p exp*)
      [('Array-set! (list a o v))
       (store! v (gep (->arr-ptr a) (list o)))]
      [('Array-ref (list a o))
       (load (gep (->arr-ptr a) (list o)))]
      [('print-float (list f p))
       (app^ (re #f 'printf tvoid (list str-ptr-type)) (cstring "%.*.lf") p (->float f))]
      [('print-int (list d))
       (app^ (re #f 'printf tvoid (list str-ptr-type)) (cstring "%ld") d)]
      [('print-bool (list b))
       (if^ (imdt->bool b)
            (se (app^ (re #f 'printf tvoid (list str-ptr-type)) (cstring "#t")))
            (se (app^ (re #f 'printf tvoid (list str-ptr-type)) (cstring "#f"))))]
      [('Printf (cons fmt exp*))
       (app (re #f 'printf tvoid (list str-ptr-type)) (cons (->string fmt) exp*))]
      [('Alloc (list exp)) (alloc (mul size-of-imdt exp))]
      [('Types-hashcons! (and args (list e hcode)))
       (app (re #f 'hashcons i64) (cons (load types_ht) args))]
      [('Types-gen-index! (list))
       (store! (add (load types_unique_index_counter) (cui 1 i64)) types_unique_index_counter)]
      [('mref-cast-queue-enqueue (and args (list addr ty)))
       (app (re #f 'cast_queue_enqueue tvoid) (cons (load mref-cast-q) args))]
      [('mref-cast-queue-dequeue (list))
       ;; Since the rest of the code expects a imdt-type value we build a lhs
       ;; that expects {i64,i64} struct here.
       (sham:ast:expr:let
        (list (gensym '_))
        (list suspended-cast-type)
        (list (app (re #f 'cast_queue_dequeue suspended-cast-type) (list (load mref-cast-q))))
        (svoid)
        (evoid))]
      [('mref-cast-queue-not-empty? (list))
       (app (re #f 'cast_queue_is_not_empty i64) (list (load mref-cast-q)))]
      [('mref-cast-queue-peek-address (list))
       (app (re #f 'cast_queue_peek_address i64) (list (load mref-cast-q)))]
      [('mref-cast-queue-peek-type (list))
       (app (re #f 'cast_queue_peek_type i64) (list (load mref-cast-q)))]
      [('mvect-cast-queue-enqueue (and args (list addr ty)))
       (app (re #f 'cast_queue_enqueue tvoid) (cons (load mvect-cast-q) args))]
      [('mvect-cast-queue-dequeue (list))
       ;; Since the rest of the code expects a imdt-type value we build a lhs
       ;; that expects {i64,i64} struct here.
       (sham:ast:expr:let
        (list (gensym '_))
        (list suspended-cast-type)
        (list (app (re #f 'cast_queue_dequeue suspended-cast-type) (list (load mvect-cast-q))))
        (svoid)
        (evoid))]
      [('mvect-cast-queue-not-empty? (list))
       (app (re #f 'cast_queue_is_not_empty i64) (list (load mvect-cast-q)))]
      [('mvect-cast-queue-peek-address (list))
       (app (re #f 'cast_queue_peek_address i64) (list (load mvect-cast-q)))]
      [('mvect-cast-queue-peek-type (list))
       (app (re #f 'cast_queue_peek_type i64) (list (load mvect-cast-q)))]
      [('timer-start (list))
       (store!
        (app^ (re #f 'gettimeofday i32) (load (external #f 'timer_start_time i8)) (ui64 0))
        (external #f 'timer_started i32))]
      [('timer-stop  (list))
       (store!
        (app^ (re #f 'gettimeofday i32) (load (external #f 'timer_stop_time i8)) (ui64 0))
        (external #f 'timer_stopped i32))]
      [('timer-report (list)) (app^ (re #f 'timer_report tvoid))]
      [((app easy-p->impl (list f a* (list r))) e*)
       (define (simple-apply f x) (f x))
       (unless (and (length a*) (length e*))
         (error 'backend/c/generate-sham/generate-op "wrong number of args: ~a" e*))
       (debug off (r (apply f (map simple-apply a* e*))))
       ]))

  ;; The program is run for it's effect
  (generate-effect stmt))


;; i8* is used here as void* which doesn't exist in llvm
;; Note: This is one place where sham differs from C. In C references to
;; top level variables are implicitly turned into loads and stores from
;; the binary file. In sham and LLVM, external variables are implicitly
;; a pointer to the object of their, but their is no explicit loads or store.
;; Example:
;; (store! (add (load types_unique_index_counter) (ui64 1)) types_unique_index_counter)
;; This means a regular occurrence of types_unique_index_counter would
;; be equivalent to &types_unique_index_counter in the C language.
(define types_ht
  (external 'runtime.o 'types_ht i8*))
(define types_unique_index_counter
  (external 'runtime.o 'types_unique_index_counter i64))
(define mref-cast-q
  (external 'runtime.o 'mref_cast_q i8*))
(define mvect-cast-q
  (external 'runtime.o 'mvect_cast_q i8*))

;; Casts require types to be in their expression form
;; So here are all the common types we use and a bunch expressions for them.
(define float-type f64)
(define float-expr (sham:ast:expr:etype float-type))
(define int-type i64)
(define int-expr (sham:ast:expr:etype int-type))
(define imdt-type int-type)
(define imdt-expr int-expr)
;; In value contexts this needs to be returned instead of evoid
(define UNIT-EXPR (ui64 data:UNIT-IMDT))
(define size-of-imdt (ui64 8))
(define bool-type i1)
(define bool-expr (sham:ast:expr:etype bool-type))
(define arr-ptr-type i64*)
(define arr-ptr-expr (sham:ast:expr:etype arr-ptr-type))
(define str-ptr-type i8*)
(define str-ptr-expr (sham:ast:expr:etype str-ptr-type))
(define suspended-cast-type (tstruct '(address type) `(,i64 ,i64)))

;; LLVMs casts are complicated
;; Bitcast to switch between integer and float
(define (->imdt v)
  (bitcast v imdt-expr))
(define (->float v)
  (bitcast v float-expr))
(define (->int v)
  (bitcast v int-expr))

;; pointer conversion requires an particular cast
(define (ptr->imdt v)
  (ptr->int v imdt-expr))
(define (->string v)
  (int->ptr v str-ptr-expr))
(define (->arr-ptr v)
  (int->ptr v arr-ptr-expr))

;; truncate to integer
(define (float->int v)
  (fp->si v int-expr))

;; extend to floating point
(define (int->float v)
  (si->fp v float-expr))

;; intcast to switch between int sizes
(define (bool->imdt v)
  (intcast v imdt-expr))
(define (imdt->bool v)
  (intcast v bool-expr))



;; These ops didn't have counterparts in the llvm
(define (build-and x y) (if^ x y (ui64 0)))
(define (build-or x y) (if^ x (ui64 1) y))
(define (build-negate n) (fsub (cfl 0.0 f64) n))

(define ((call f) . args)
  (app f args))


;; Map or grift primitive symbol to 3 things:
;; a function that builds the corresponding sham ast nodes,
;; a list of functions that build sham cast ast nodes on the arguments,
;; and a list containing the cast on the return ast nodes.
(define prim-impl : (HashTable Symbol IMPL)
  (make-immutable-hash
   `((+          ,add    (,identity ,identity) (,identity))
     (-          ,sub    (,identity ,identity) (,identity))
     (*          ,mul    (,identity ,identity) (,identity))
     (%<<        ,shl    (,identity ,identity) (,identity))
     (%>>        ,lshr   (,identity ,identity) (,identity))
     (%/         ,sdiv   (,identity ,identity) (,identity))
     (%%         ,srem   (,identity ,identity) (,identity))
     (binary-and ,and^   (,identity ,identity) (,identity))
     (binary-or  ,or^    (,identity ,identity) (,identity))
     (binary-xor ,xor^   (,identity ,identity) (,identity))
     (binary-not ,not^   (,identity) (,identity))
     (<          ,icmp-slt (,identity ,identity) (,bool->imdt))
     (<=         ,icmp-sle (,identity ,identity) (,bool->imdt))
     (=          ,icmp-eq  (,identity ,identity) (,bool->imdt))
     (>          ,icmp-sgt (,identity ,identity) (,bool->imdt))
     (>=         ,icmp-sge (,identity ,identity) (,bool->imdt))
     (and        ,build-and (,imdt->bool ,imdt->bool) (,bool->imdt))
     (or         ,build-or (,imdt->bool ,imdt->bool) (,bool->imdt))
     (not        ,not^     (,imdt->bool) (,bool->imdt))
     (quotient   ,sdiv     (,identity ,identity) (,identity))
     (fl+        ,fadd     (,->float ,->float) (,->imdt))
     (fl-        ,fsub     (,->float ,->float) (,->imdt))
     (fl*        ,fmul     (,->float ,->float) (,->imdt))
     (fl/        ,fdiv     (,->float ,->float) (,->imdt))
     (flmodulo   ,frem  (,->float ,->float) (,->imdt))
     (flmin      ,(call (ri "llvm.minnum.f64" f64)) (,->float ,->float) (,->imdt))
     (flmax      ,(call (ri "llvm.maxnum.f64" f64)) (,->float ,->float) (,->imdt))
     (fl<        ,fcmp-olt     (,->float ,->float) (,bool->imdt))
     (fl<=       ,fcmp-ole   (,->float ,->float) (,bool->imdt))
     (fl=        ,fcmp-oeq    (,->float ,->float) (,bool->imdt))
     (fl>        ,fcmp-ogt     (,->float ,->float) (,bool->imdt))
     (fl>=       ,fcmp-oge    (,->float ,->float) (,bool->imdt))
     (flquotient ,fdiv     (,->float ,->float) (,identity))
     (flround    ,(call (ri "llvm.round.f64" f64)) (,->float) (,->imdt))
     (flnegate   ,build-negate    (,->float) (,->imdt))
     (flabs      ,(call (ri "llvm.abs.f64" f64)) (,->float) (,->imdt))
     (flfloor    ,(call (ri "llvm.floor.f64" f64)) (,->float) (,->imdt))
     (flceiling  ,(call (ri "llvm.ceil.f64" f64)) (,->float) (,->imdt))
     (flcos      ,(call (ri "llmv.cos.f64" f64)) (,->float) (,->imdt))
     (fltan      ,(call (ri "llvm.tan.f64" f64)) (,->float) (,->imdt))
     (flsin      ,(call (ri "llvm.sin.f64" f64)) (,->float) (,->imdt))
     (flasin     ,(call (re #f 'asin f64)) (,->float) (,->imdt))
     (flacos     ,(call (re #f 'acos f64)) (,->float) (,->imdt))
     (flatan     ,(call (re #f 'atan f64)) (,->float) (,->imdt))
     (flatan2    ,(call (re #f 'atan2 f64)) (,->float ,->float) (,->imdt)) 
     (fllog      ,(call (ri "llvm.log.f64" f64)) (,->float) (,->imdt))
     (flexp      ,(call (ri "llvm.exp.f64" f64)) (,->float) (,->imdt))
     (flsqrt     ,(call (ri "llvm.sqrt.f64" f64)) (,->float) (,->imdt))
     (Print      ,(call (re #f 'printf tvoid (list str-ptr-type)))  (,->string) (,identity))
     (Exit       ,(call (re #f 'exit tvoid)) (,identity) (,identity))
     (int->float ,int->float (,->int) (,->imdt))
     (float->int ,float->int (,->float) (,identity))
     (read-bool  ,(call (re #f 'read_bool i64)) () (,identity))
     (read-int   ,(call (re #f 'read_int i64)) () (,identity))
     (read-float ,(call (re #f 'read_float f64)) () (,->imdt))
     (read-char  ,(call (re #f 'read_ascii_char i64)) () (,identity))
     (print-char ,(call (re #f 'print_ascii_char tvoid)) (,identity) (,identity))
     (display-char ,(call (re #f 'display_ascii_char tvoid)) (,identity) (,identity))
     (int->char  ,identity (,identity) (,identity))
     (char->int  ,identity (,identity) (,identity))
     (make-assoc-stack ,(call (re #f 'grift_make_assoc_stack i8*)) () (,ptr->imdt))
     (assoc-stack-push!
      ,(call (re #f 'grift_assoc_stack_push i8*))
      (,->arr-ptr ,identity ,identity ,identity)
      (,identity))
     (assoc-stack-pop! ,(call (re #f 'grift_assoc_stack_pop i64)) (,->arr-ptr) (,identity))
     (assoc-stack-find
      ,(call (re #f 'grift_assoc_stack_find i64))
      (,->arr-ptr ,identity ,identity)
      (,identity))
     (assoc-stack-set!
      ,(call (re #f 'grift_assoc_stack_set tvoid))
      (,->arr-ptr ,identity ,identity)
      (,identity))
     (assoc-stack-ref ,(call (re #f 'grift_assoc_stack_ref i64))
      (,->arr-ptr ,identity)
      (,identity)))))

(define (easy-p->impl [s : Symbol]) : (Option IMPL)
  (hash-ref prim-impl s #f))



