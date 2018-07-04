#lang typed/racket/base
#|------------------------------------------------------------------------------+
|Pass: src/casts/convert-closures                                               |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
Description: This pass performs closure conversion and optimization based off
of the paper Optimizing closures in O(n) time.

- Closure Conversion is performed by the function `uncover-closure-ops`:
 - Records the free varibales of each closure in the `Closure` form.
 - Reduces the number of free variables by eliminating free variables that are
   known to be :
   - bound to constants (constant propagation)
   - aliases of other free variables (copy propagation)
   - references to the current closure (self reference)
 - Generates names for the closure self-parameter and closure code
 - Calls function code directly when a closure is known (direct call optimization)
- Closure optimization is performed by the function `optimize-closures`:
  - Identifies closures with the same lifetime 
    - uses tarjan's algorithm to identify strongly connected components formed by
      variable capture.
  - When one or more well-known closures exist with the same life 
    we share a single closure for the well-known closures.
  - When a closure is not well known but has no free variables
    then we statically allocate the closure and eliminate free
    variables that reference it.
  - Take into account all free-variables eliminated by sharing,
    statically allocating closures, and eliminating closure
    allocations in the next step.
  - Select representations for closures that minimize space, 
    maximize sharing, without extending the lifetime of any
    closure.
    - *Well-Known w/ No Free Variable* No Closure
    - *Well-Known w/ 1 Free Variable* Lambda Lift the one variable
    - *Well-Known w/ more than 1 Free Variables* allocate free variable in a tuple
    - *Not Well-Known w/ No Free Variable* Static Allocated Closure
    - *Otherwise* allocate closure normally

Terminology:
- a closure is *well-known* if we know where all of its call sites are.
  - we conservatively estimate this property by checking to see if the
    closure is found in any argument possition.
- a closure is *known* if we know that a variable binds it at a particular
  location. 

+-------------------------------------------------------------------------------+
TODO Fix Recursive Types
TODO Integrate Closure Optimization
TODO Start optimize closures
TODO introduce static procedure form

Notes to be deleted after implementation
;;   (define select-share (optimize-shared-closures prgm))
;;   ;;4. Determine the required free variables for each closure, leaving
;;   ;;out those that are unnecessary.

;;   ;; Constant propogate statically allocated closure and 
;;   ;; Constant propogate small constants
;;   ;; Take into account free variables eliminated by closure conversion
;;   ;; Copy propogation (Alias Propagation)
;;   ;; 
  
;;   ;;5. Select the appropriate representation for each closure and
;;   ;;whether it can share space with a closure from some outer strongly
;;   ;;connected set of bindings.
  
  
;;   ;;6. Rebuild the code based on these selections.

+------------------------------------------------------------------------------|#

(require 
 racket/match
 racket/hash
 "../configuration.rkt"
 "../language/cast-or-coerce3.1.rkt"
 "../language/cast-or-coerce4.rkt"
 "../language/syntax.rkt"
 "../logging.rkt"
 #;"../language/cast-or-coerce5.rkt"
 #;"../language/cast-or-coerce6.rkt"
 "../type-equality.rkt")

(provide
 convert-closures
 #;
 (all-from-out
  "../language/cast-or-coerce6.rkt"))

(: convert-closures (Cast-or-Coerce4-Lang -> Any))
(define (convert-closures prgm)
  (match prgm
    [(Prog (list name next type)
       (Let-Static* bnd-mu-type*
                    bnd-type*
                    bnd-mu-crcn*
                    bnd-crcn*
                    main-expr))
     
     (define uc (make-unique-counter next))

     (define main-expr^
       (parameterize ([current-unique-counter uc])
         (define cast-rep (cast-representation))
         (define fn-proxy-rep (fn-proxy-representation))
         (uncover-closure-ops main-expr)))

     (Prog (list name (unique-counter-next! uc) type)
       (Let-Static* bnd-mu-type*
                    bnd-type*
                    bnd-mu-crcn*
                    bnd-crcn*
                    main-expr^))]))

(define (unbound-variable-error u)
  (error 'convert-closures.rkt "unbound variable ~a" u))

(require typed/racket/unsafe)

(unsafe-require/typed
 "../language/form-map.rkt"
 ;; One common cause of type-checking time explosion is having type
 ;; errors involving this type.
 [form-map ((U= CoC4-Expr) (CoC4-Expr -> CoC6-Expr) -> (U= CoC6-Expr))])

;; TODO This should go in configuration.rkt
(define-type Fn-Proxy-Representation (U 'Hybrid 'Data))
(: fn-proxy-representation (Parameterof Fn-Proxy-Representation))
(define fn-proxy-representation
  (make-parameter 'Hybrid))

(struct (E) Let-Closures form
  ([bindings : (Listof (Closure E))]
   [body : E])
  #:transparent)
(struct (E) Closure form
  ([name : Uid]
   [code-label : Uid]
   [self-parameter : Uid]
   [caster : (Option Uid)]
   [free-vars : Uid*]
   [parameters : Uid*]
   [code : E])
  #:transparent)

(struct (E) Closure-Code form
  ([arg : E])
  #:transparent)

(struct (E) Closure-Caster form
  ([arg : E])
  #:transparent)

(struct (E) Closure-Ref form
  ([arg : Uid] [key : Uid])
  #:transparent)

(struct (E) Closure-App form
  ([code : E]
   [closure : E]
   [arguments : (Listof E)])
  #:transparent)

(define-type (Closure-Ops E)
  (U (LetP (Bnd* (Procedure Uid Uid* Uid (Option Uid) Uid* E)) E)
     (Let-Closures E)
     (Closure-Code E)
     (Closure-Caster E)
     Closure-Ref
     (Closure-App E)))

(define-type (Hybrid-Fn-Proxy-Forms E)
  (U (Hybrid-Proxy Uid E E)
     (Hybrid-Proxy-Huh E)
     (Hybrid-Proxy-Closure E)
     (Hybrid-Proxy-Coercion E)))

(define-type (Data-Fn-Proxy-Forms E)
  (U (Fn-Proxy Index E E)
     (Fn-Proxy-Huh E)
     (Fn-Proxy-Closure E)
     (Fn-Proxy-Coercion E)))

(define-type CoC6-Expr
  (Rec
   E
   (U (Closure-Ops E)
      (Data-Fn-Proxy-Forms E)
      (Hybrid-Fn-Proxy-Forms E)
      (Gen-Data-Forms E)
      (Code-Forms E)
      (Quote-Coercion Immediate-Coercion)
      (Hyper-Coercion-Operation-Forms E)
      (Coercion-Operation-Forms E)
      (Type Immediate-Type)
      (Type-Operation-Forms E)
      (Let (Bnd* E) E)
      (Var Uid)
      (Control-Flow-Forms E)
      (Op Grift-Primitive (Listof E))
      No-Op
      (Quote Cast-Literal)
      (Blame E)
      (Observe E Grift-Type)
      (Unguarded-Forms E)
      (Guarded-Proxy-Forms E)
      (Monotonic-Forms E Immediate-Type)
      (Error E)
      (Tuple-Operation-Forms E))))


;; Uncover Free Eliminates the following forms
(define-type (U- E)
  (U (Named-Castable-Lambda-Forms E)
     (Fn-Proxy-Forms E)))

;; Uncover Free Adds The following forms
(define-type (U+ E)
  (U (Closure-Ops E)
     (Data-Fn-Proxy-Forms E)
     (Hybrid-Fn-Proxy-Forms E)))

;; Uncover Free is invariant in the following forms
(define-type (U= E)
  (U (Gen-Data-Forms E)
     (Code-Forms E)
     (Quote-Coercion Immediate-Coercion)
     (Hyper-Coercion-Operation-Forms E)
     (Coercion-Operation-Forms E)
     (Type Immediate-Type)
     (Type-Operation-Forms E)
     (Let (Bnd* E) E)
     (Var Uid)
     (Control-Flow-Forms E)
     (Op Grift-Primitive (Listof E))
     No-Op
     (Quote Cast-Literal)
     (Blame E)
     (Observe E Grift-Type)
     (Unguarded-Forms E)
     (Guarded-Proxy-Forms E)
     (Monotonic-Forms E Immediate-Type)
     (Error E)
     (Tuple-Operation-Forms E)))

;; Debugging assertions (Are our types in the correct ballpark)
(assert-subtype? (U- CoC4-Expr) CoC4-Expr)
(assert-subtype? (U= CoC4-Expr) CoC4-Expr)
(assert-subtype? (U+ CoC6-Expr) CoC6-Expr)
(assert-subtype? (U= CoC6-Expr) CoC6-Expr)

;; INVARIANT CoC4 = U= + U-
;;We check this by checking subtyping in both directions because it is
;;easier to know what is broken when this fails.
(assert-subtype? (U (U= CoC4-Expr) (U- CoC4-Expr)) CoC4-Expr)
(assert-subtype? CoC4-Expr (U (U= CoC4-Expr) (U- CoC4-Expr)))

;; INVARIANT CoC6 = U= + U+
;;We check this by checking subtyping in both directions because it is
;;easier to know what is broken when this fails.
(assert-subtype? (U (U= CoC6-Expr) (U+ CoC6-Expr)) CoC6-Expr)
(assert-subtype? CoC6-Expr (U (U= CoC6-Expr) (U+ CoC6-Expr)))

(struct Bound-Var
  ([uid : Uid]
   [var : (Var Uid)]
   [depth : Nat])
  #:transparent)

(define-type Propagate
  (U Bound-Var (Quote Cast-Literal)))

(: uncover-closure-ops
   (->* (CoC4-Expr)
        (#:cast-rep Cast-Representation
         #:fn-proxy-rep Fn-Proxy-Representation)
        CoC6-Expr))
(define (uncover-closure-ops
         e
         #:cast-rep [cast-rep (cast-representation)]
         #:fn-proxy-rep [fn-proxy-rep (fn-proxy-representation)])
  (define-type Proc6 (Procedure Uid Uid* Uid (Option Uid) Uid* CoC6-Expr))
  (define apply-proc-bindings : (HashTable Integer (cons Uid Proc6))
    (make-hash '()))
  (: get-apply-uid! (Integer Uid -> Uid))
  (define (get-apply-uid! i cast-uid)
    (define apply-bnd? (hash-ref apply-proc-bindings i #f))
    (cond
      [apply-bnd? => (inst car Uid Proc6)]
      [else
       ;; The following code is kinda tricky but the Procedure
       ;; form gets lowered to only generating the code but not
       ;; instantiation of a closure.
       ;; TODO change this form's name to be less generic.
       ;; TODO change this form's fields to match Closure
       (define apply-name (string-append "apply_hybrid_" (number->string i)))
       (define apply-uid (next-uid! apply-name))
       (define self (next-uid! "hybrid_closure_self"))
       (define a* (build-list i (lambda (a) (next-uid! "arg"))))
       (define v* (map (inst Var Uid) a*))
       (define clos-field (next-uid! "closure_field"))
       (define crcn-field (next-uid! "corcion_field"))
       (define bnd
         (cons
          apply-uid
          (Procedure
           self a* apply-uid #f
           ;; The order of these two elements determines closure layout
           ;; which needs to be know for the hybrid representation
           ;; we could make the caster call closure specific code
           ;; that is also generated by the procedure form.
           (list clos-field crcn-field)
           (let$ ([hybrid-proxy-closure (Closure-Ref self clos-field)]
                  [hybrid-proxy-coercion (Closure-Ref self crcn-field)])
             (cast-apply-cast cast-uid
                              hybrid-proxy-closure
                              v*
                              hybrid-proxy-coercion)))))
       (hash-set! apply-proc-bindings i bnd)
       apply-uid]))
  
  (define rec/env-return
    (let rec/env : CoC6-Expr
         ([current-e : CoC4-Expr e]
          ;; Immutable map from function (closure) names to code labels
          [code-env : (HashTable Uid Uid) (hash)]
          ;; Map from variable names to representative constant or variable
          [opt-env : (HashTable Uid Propagate) (hash)]
          ;; The current number of bindings between `e` and top-level
          [current-binding-depth : Nat 0]
          ;; The name that the closure is bound in the body of the letrec
          [current-closure-name : (Option Uid) #f]
          ;; The self parameter of the closure being processed
          [current-closure-self : (Option Uid) #f]
          ;; The depth at which the current closure is bound
          [current-closure-depth : Nat 0]
          ;; Mutable set of free variables in the current closure
          ;; this is an out parameter.
          [free-variables : (MSet Uid) (mutable-set)])
         (define res : CoC6-Expr
           (let rec ([current-e current-e])
             (: rec* : (Listof CoC4-Expr) -> (Listof CoC6-Expr))
             (define (rec* e*) (map rec e*))
             (cond
               [(Var? current-e)
                (match-define (Var u) current-e)
                ;; Folding Constants and Aliases can reduce closure size
                ;; particularly when we rely on the c compiler to perform
                ;; these operations for use
                (define propagate (hash-ref opt-env u))
                (cond
                  [(Bound-Var? propagate)
                   (match-define (Bound-Var u v binding-depth) propagate)
                   (cond
                     ;; - If the variable is bound at a lesser depth
                     ;;   than the closure closes over then it is free.
                     ;; - If the variable is bound at the same depth
                     ;;   then it is another closure and we will try
                     ;;   to optimize it in optimize-closures pass,
                     ;;   but for he time being the variable is free.
                     [(<= binding-depth current-closure-depth)
                      (unless current-closure-self
                        (error 'convert-closures.rkt
                               "free variable without current closure: ~a" u))
                      (cond
                        ;; Optimize Self Reference
                        ;; If a free variable is refering to the outside/recusive
                        ;; name of the current-closure then replace it with the
                        ;; a reference to the closure self parameter.
                        [(eq? u current-closure-name) (Var current-closure-self)]
                        [else
                         ;; Otherwise the variable is indeed free and we need
                         ;; to put it in the closure, and refer to it from the
                         ;; the closure.
                         (set-add! free-variables u)
                         (Closure-Ref current-closure-self u)])]
                     ;; The bound var is not free so just use the variable
                     [else v])] 
                  ;; The variable was bound to a constant that can be propogated
                  [else propagate])]
               [(Letrec? current-e)
                (match-define (Letrec b* e) current-e)
                (define letrec-binding-depth (add1 current-binding-depth))
                (: code-label* Uid*)
                (: self-parameter* Uid*)
                (define-values (code-label* self-parameter*)
                  (for/lists ([code-label* : Uid*]
                              [self-parameter* : Uid*])
                             ([b b*])
                    (match-define (cons clos-name _) b)
                    (values (format-uid! "~a_code" clos-name)
                            (format-uid! "~a_closure_self" clos-name))))
                ;; Extend the code-env bindings for Optimize Direct Call
                ;; Extend opt-env without trying to eliminate bindings.
                (define-values (code-env^ opt-env^) 
                  (for/fold ([code-env^ : (HashTable Uid Uid) code-env]
                             [opt-env^ : (HashTable Uid Propagate) opt-env])
                            ([code-label code-label*]
                             [b b*])
                    (match-define (cons x _) b)
                    (define bv (Bound-Var x (Var x) letrec-binding-depth))
                    (values (hash-set code-env^ x code-label)
                            (hash-set opt-env^  x bv))))
                (define c* : (Listof (Closure CoC6-Expr))
                  (for/list ([b b*]
                             [code-label code-label*]
                             [self-parameter self-parameter*])
                    (match-define (cons closure-name (Lambda p* (Castable c? e))) b)
                    (define lambda-binding-depth (add1 letrec-binding-depth))
                    (define fvs : (MSet Uid) (mutable-set))
                    (define code-env^^
                      (hash-set code-env^ self-parameter code-label))
                    (define opt-env^^
                      (for/fold ([env : (HashTable Uid Propagate) opt-env^])
                                ([p p*])
                        (hash-set env p (Bound-Var p (Var p) lambda-binding-depth))))
                    (define e^
                      (rec/env e code-env^^ opt-env^^
                               lambda-binding-depth
                               closure-name
                               self-parameter
                               letrec-binding-depth
                               fvs))
                    ;; We have to check each free variable at this depth
                    ;; to see if they need added to the current free variable set.
                    ;; TODO find a way not to perfom this iteration/lookup again
                    (define fv* (set->list fvs))
                    (for ([u fv*])
                      (match (hash-ref opt-env^ u)
                        [(Bound-Var _ _ bd)
                         #:when (<= bd current-closure-depth)
                         (set-add! free-variables u)]
                        [other (void)]))
                    (Closure closure-name code-label self-parameter c? fv* p* e^)))

                (Let-Closures c* (rec/env e code-env^ opt-env^
                                          letrec-binding-depth
                                          current-closure-name
                                          current-closure-self
                                          current-closure-depth
                                          free-variables))]
               [(Let? current-e)
                (match-define (Let b* e) current-e)
                (define let-binding-depth (add1 current-binding-depth))
                (define-values (b*^ opt-env^)
                  (for/fold ([b*^ : (Bnd* CoC6-Expr) '()]
                             [env : (HashTable Uid Propagate) opt-env])
                            ([b b*])
                    (match-define (cons u e) b)
                    ;; Invoking `rec` here would cause free variables that
                    ;; are aliased but not used to be recorded as being free.
                    (define possibly-free-vars : (MSet Uid) (mutable-set))
                    ;; Notes on recursion:
                    ;; - recuring on the rhs `e` of binding
                  
                    ;; - current-depth isn't incremented because the binding hasn't occurred
                    ;; - current-closure-* remains the same
                    ;; - possibly-free-vars will be added to free variables if
                    ;;   the binding isn't eliminated.
                    (match (rec/env e code-env opt-env
                                    current-binding-depth
                                    current-closure-name
                                    current-closure-self
                                    current-closure-depth
                                    possibly-free-vars)
                      ;; Eliminate aliases
                      [(Var u^)
                       (values b*^ (hash-set env u (hash-ref opt-env u^)))]
                      ;; Propogate Constants
                      [(and (Quote _) lit) 
                       (values b*^ (hash-set env u lit))]
                      [e
                       ;; Can't eliminate this binding
                       (define bv (Bound-Var u (Var u) let-binding-depth))
                       ;; Need to merge the free variables of this expression
                       (set-union! free-variables possibly-free-vars)
                       (values (cons (cons u e) b*^)
                               (hash-set env u bv))])))
                (define e^
                  (rec/env e code-env
                           ;; May have added propagatable bindings
                           opt-env^
                           ;; Going under a binding
                           let-binding-depth
                           current-closure-name
                           current-closure-self
                           current-closure-depth
                           free-variables))
                (cond
                  [(null? b*^) e^]
                  [else (Let b*^ e^)])]
               ;; Castable Function Operations
               [(Fn-Caster? current-e)
                (match-define (Fn-Caster e) current-e)
                (Closure-Caster (rec e))]
               [(App-Fn? current-e)
                (: let-clos-app : CoC6-Expr (Listof CoC6-Expr) -> CoC6-Expr) 
                (define (let-clos-app e e*)
                  (let$ ([closure e]) (clos-app closure e*)))
                (: clos-app : (Var Uid) (Listof CoC6-Expr) -> CoC6-Expr) 
                (define (clos-app v e*) (Closure-App (Closure-Code v) v e*))
                (match-define (App-Fn e (app rec* e*)) current-e)
                (match (rec e)
                  [(and v (Var u))
                   (cond
                     [(hash-ref code-env u #f) =>
                      (lambda ([u : Uid])
                        (Closure-App (Code-Label u) v e*))]
                     [else (clos-app v e*)])]
                  [(and cr (Closure-Ref _ u))
                   (cond
                     [(hash-ref code-env u #f) =>
                      (lambda ([u : Uid])
                        (Closure-App (Code-Label u) cr e*))]
                     [else (let-clos-app cr e*)])]
                  [e (let-clos-app e e*)])]
               ;; Fn-Proxy Operations
               ;; App-Fn-or-Proxy is very similar to Fn-App
               [(App-Fn-or-Proxy? current-e)
                (: cast-clos-app : Uid CoC6-Expr (Listof CoC6-Expr) -> CoC6-Expr)
                (define (cast-clos-app cast v e*)
                  (If (Fn-Proxy-Huh v)
                      (let$ ([data-fn-proxy-closure (Fn-Proxy-Closure v)]
                             [data-fn-proxy-coercion (Fn-Proxy-Coercion v)])
                        (cast-apply-cast cast data-fn-proxy-closure e*
                                         data-fn-proxy-coercion)) 
                      (Closure-App (Closure-Code v) v e*)))
                (: let-cast-clos-app :
                   Uid CoC6-Expr (Listof CoC6-Expr) -> CoC6-Expr)
                (define (let-cast-clos-app cast e e*)
                  (let$ ([maybe-data-fn-proxy e])
                    (cast-clos-app cast e e*)))
                (match-define (App-Fn-or-Proxy cast-uid e e*) current-e)
                (case cast-rep
                  [(Coercions Hyper-Coercions)
                   (case fn-proxy-rep
                     ;; If it is a hybrid representation then proxies and closures
                     ;; are applied the same way.
                     [(Hybrid) (rec (App-Fn e e*))]
                     [(Data)
                      (define e*^ (rec* e*))
                      (match (rec e)
                        [(and v (Var u))
                         (cond
                           ;; If it a data representation and it is a
                           ;; known definition site then it can't be a
                           ;; proxy because we can see the lambda
                           [(hash-ref code-env u #f) =>
                            (lambda ([u : Uid])
                              (Closure-App (Code-Label u) v e*^))]
                           [else (cast-clos-app cast-uid v e*^)])]
                        [(and cr (Closure-Ref _ u))
                         (cond
                           ;; If it a data representation and it is a
                           ;; known definition site then it can't be a
                           ;; proxy because we can see the lambda
                           [(hash-ref code-env u #f) =>
                            (lambda ([u : Uid])
                              (Closure-App (Code-Label u) cr e*^))]
                           [else (let-cast-clos-app cast-uid cr e*^)])]
                        [e^ (let-cast-clos-app cast-uid e^ e*^)])]
                     [else (error 'covert-closures "Unkown Fn-Proxy Representaion")])]
                  [else (error 'covert-closures "Unkown Cast Representaion")])]
               [(Fn-Proxy? current-e)
                (match-define
                  (Fn-Proxy `(,i ,cast) (app rec e1)(app rec e2))
                  current-e)
                (case cast-rep
                  [(Coercions Hyper-Coercions)
                   (case fn-proxy-rep              
                     [(Hybrid) (Hybrid-Proxy (get-apply-uid! i cast) e1 e2)]
                     [(Data)   (Fn-Proxy i e1 e2)]
                     [else (error 'convert-closures "Unkown Fn-Proxy Representation")])]
                  [else (error 'convert-closures "unkown cast representation")])]
               [(Fn-Proxy-Huh? current-e)
                (match-define (Fn-Proxy-Huh (app rec e)) current-e) 
                (case cast-rep
                  [(Coercions Hyper-Coercions)
                   (case fn-proxy-rep
                     [(Hybrid) (Hybrid-Proxy-Huh e)]
                     [(Data)   (Fn-Proxy-Huh e)]
                     [else (error 'convert-closures "Unkown Fn-Proxy Representation")])]
                  [else (error 'convert-closures "unkown cast representation")])]
               [(Fn-Proxy-Closure? current-e)
                (match-define (Fn-Proxy-Closure (app rec e)) current-e)
                (case cast-rep
                  [(Coercions Hyper-Coercions)
                   (case fn-proxy-rep
                     [(Hybrid) (Hybrid-Proxy-Closure e)]
                     [(Data)   (Fn-Proxy-Closure e)]
                     [else (error 'convert-closures "Unkown Fn-Proxy Representation")])]
                  [else (error 'convert-closures "unkown cast representation")])]
               [(Fn-Proxy-Coercion? current-e)
                (match-define (Fn-Proxy-Coercion (app rec e)) current-e) 
                (case cast-rep
                  [(Coercions Hyper-Coercions)
                   (case fn-proxy-rep
                     [(Hybrid) (Hybrid-Proxy-Coercion e)]
                     [(Data)   (Fn-Proxy-Coercion e)]
                     [else (error 'convert-closures "Unkown Fn-Proxy Representation")])]
                  [else (error 'convert-closures "unkown cast representation")])] 
               [else (form-map current-e rec)])))
         (debug off
                current-e code-env opt-env current-binding-depth
                current-closure-name current-closure-self
                current-closure-depth
                free-variables res)))
  (define apply-proc-bnd* (hash-values apply-proc-bindings))
  (cond
    [(null? apply-proc-bnd*) rec/env-return]
    [else (LetP apply-proc-bnd* rec/env-return)]))


(module+ test
  (require
   (for-syntax
    racket/base
    racket/syntax)
   typed/rackunit)

  (define (refresh!)
    (current-unique-counter (make-unique-counter 0)))

  (define-syntax (define-u/v stx)
    (syntax-case stx ()
      [(_ u (c v) s n)
       #'(begin
           (define u (Uid s n))
           (define v (c u)))]))

  (define-syntax (define-u/v* stx)
    (syntax-case stx ()
      [(_ u (c v) s n)
       (with-syntax ([((u* v* n*) ...)
                      (for/list ([i (in-range (syntax->datum #'n))])
                        (list (format-id #'u "~a~a" #'u i)
                              (format-id #'v "~a~a" #'v i)
                              i))])
         #'(begin
             (define-u/v u* (c v*) s n*)
             ...))]))

  (define-u/v* u (Var v) "" 10)
  (define-u/v* p (Var pv) "arg" 10)
  (define-u/v* code (Code-Label label) "_code" 10)
  (define-u/v* self (Var selfv) "_closure_self" 5)

  
  ;; TODO Check these tests on optimize closures
  ;; TODO Write tests for most of the features of optimize closures
  
  ;; This need to be something that won't propagate for the tests to
  ;; be correct
  (define non-propagant (op$ + (Quote 1) (Quote 1)))
  ;; constant propagation
  (define input1 (Let (list (cons u0 (Quote 1))) v0))
  (refresh!)
  (check-equal? (uncover-closure-ops input1) (Quote 1))
  
  ;; copy propagation
  (define input2 (Let (list (cons u0 non-propagant))
                   (Let (list (cons u1 v0))
                     v1)))
  (define expect2 (Let (list (cons u0 non-propagant)) v0))
  (refresh!)
  (check-equal? (uncover-closure-ops input2) expect2)

  ;; correct free variable discovery for single closure
  (define input3
    (Let (list (cons u0 non-propagant))
      (Letrec (list (cons u1 (Lambda '() (Castable #f v0))))
        v1)))
  (define expect3
    (Let (list (cons u0 non-propagant))
      (Let-Closures
       (list (Closure
              u1 code0 self1 #f (list u0) '()
              (Closure-Ref self1 u0)))
       v1)))
  (refresh!)
  (check-equal? (uncover-closure-ops input3) expect3)

  ;; correct free variable discovery for nested closure 1
  ;; TODO
  (define input4 (void))

  ;; TODO
  ;; correct free variable discovery for nested closure 2
  (define input5 (void))

  ;; Mutually Recusive Closures
  (define input6
    (Letrec (list (cons u0 (Lambda '() (Castable #f (App-Fn v1 '()))))
                  (cons u1 (Lambda '() (Castable #f (App-Fn v0 '())))))
      v1))
  (define expect6
    (Let-Closures
     (list (Closure u0 code0 self1 #f (list u1) '()
                    (Closure-App (Code-Label code2)
                                 (Closure-Ref self1 u1)
                                 '()))
           (Closure u1 code2 self3 #f (list u0) '()
                    (Closure-App (Code-Label code0)
                                 (Closure-Ref self3 u0)
                                 '())))
     v1))
  (refresh!)
  (check-equal? (uncover-closure-ops input6) expect6)

  ;; Chaining of optimization leads to less free variables
  (define input7
    (Let (list (cons u0 (Quote 1)))
      (Letrec (list (cons u1 (Lambda (list u2) (Castable #f v0))))
        (Let (list (cons u3 v1))
          (Letrec
              (list
               (cons u4 (Lambda '() (Castable #f (App-Fn v1 (list v4))))))
            v4)))))
  (define expect7
    (Let-Closures
     (list (Closure u1 code0 self1 #f '() `(,u2) (Quote 1)))
     (Let-Closures
      (list (Closure u4 code2 self3 #f `(,u1) '()
                     (Closure-App label0
                                  (Closure-Ref self3 u1)
                                  (list selfv3))))
      v4)))
  (refresh!)
  (check-equal? (uncover-closure-ops input7) expect7)

  ;; Optimization works with proxied function applications where
  ;; possible
  (define input8
    (Letrec
        (list
         (cons
          u0
          (Lambda (list p0)
            (Castable
             u1
             (If (op$ = v0 pv0)
                 (App-Fn-or-Proxy u2 v0 (list pv0))
                 (App-Fn-or-Proxy u2 pv0 (list v0)))))))
      (App-Fn v0 (list v0))))
  (define expect8
    (Let-Closures
     (list
      (Closure
       u0 code0 self1 u1 '() (list p0)
       (If (op$ = selfv1 pv0)
           (Closure-App label0 selfv1 (list pv0))
           (Closure-App (Closure-Code pv0) pv0 (list selfv1)))))
     (Closure-App label0 v0 (list v0))))
  (refresh!)
  (check-equal? (uncover-closure-ops input8) expect8)
  )
  


;; (: uncover-free (Cast-or-Coerce4-Lang . -> . Cast-or-Coerce5-Lang))
;; (define (uncover-free prgm)
;;   (debug 'cast/uncover-free prgm)
;;   (match-define (Prog m (Let-Static* mtb* tb* mcb* cb* e)) prgm)
;;   (define-values (new-exp free*) (uf-expr e))
;;   (unless (set-empty? free*)
;;     (raise-pass-exn 'uncover-free "Free variables detect ~a" free*))
;;   (Prog m (Let-Static* mtb* tb* mcb* cb* new-exp)))

;; (define mt-set : (Setof Uid) (set))

;; (: uf-expr (CoC4-Expr -> (values CoC5-Expr (Setof Uid))))
;; (define (uf-expr e)
;;   (: ret-e CoC5-Expr)
;;   (: ret-fv (Setof Uid))
;;   (define-values (ret-e ret-fv)
;;     (match e
;;     ;; Interesting Cases
;;     ;; Free variables of an expression are returned in the set
;;     [(Var u) (values (Var u) (set u))]
;;     ;; All binding forms must both filter variables bound at
;;     ;; this site and return the free variables of the expression
;;     [(Letrec (app uf-bnd-lambda* b*-bvars b* b*-fvars)
;;              (app uf-expr e e-fvars))
;;      (values (Letrec b* e) (set-subtract (set-union e-fvars b*-fvars) b*-bvars))]
;;     [(Let (app uf-bnd-data* b*-bvars b* b*-fvars)
;;           (app uf-expr e e-fvars))
;;      (values (Let b* e) (set-subtract (set-union e-fvars b*-fvars) b*-bvars))]
;;     ;; I do not think that labels need to be treated like variables
;;     [(Labels (app uf-bnd-code* b*) (app uf-expr e fv))
;;      (values (Labels b* e) fv)]
;;     [(Repeat i (app uf-expr e1 f1) (app uf-expr e2 f2) a (app uf-expr e3 f3) (app uf-expr e4 f4))
;;      (values (Repeat i e1 e2 a e3 e4) (set-subtract (set-union f1 f2 f3 f4) (set i a)))]
;;     [(Break-Repeat) (values (Break-Repeat) (set))]
;;     [(If (app uf-expr t t-fv) (app uf-expr c c-fv) (app uf-expr a a-fv))
;;      (values (If t c a) (set-union t-fv c-fv a-fv))]
;;     [(Switch (app uf-expr e e-fv) c* (app uf-expr d d-fv))
;;      (: uf-case/pair :
;;         (Switch-Case CoC4-Expr)
;;         (Pair (Switch-Case* CoC5-Expr) (Setof Uid))
;;         -> (Pair (Switch-Case* CoC5-Expr) (Setof Uid)))
;;      (define/match (uf-case/pair c p)
;;        [((cons l (app uf-expr r f)) (cons c* v))
;;         (cons (cons (cons l r) c*) (set-union f v))])
;;      (define acc (cons '() ((inst set Uid))))
;;      (match-define (cons c*^ c*-fv) (foldr uf-case/pair acc c*))
;;      (values (Switch e c*^ d) (set-union e-fv c*-fv d-fv))]
;;     [(Op p (app uf-expr* e* e*-fvars)) (values (Op p e*) e*-fvars)]
;;     [(and nop (No-Op)) (values nop mt-set)]
;;     [(Quote k) (values (Quote k) mt-set)]
;;     [(Tag t) (values (Tag t) mt-set)]
;;     ;; Observables Representation
;;     [(Blame (app uf-expr e f)) (values (Blame e) f)]
;;     [(Observe (app uf-expr e f) t) (values (Observe e t) f)]
;;     ;; Function Representation Primitives
;;     [(Fn-Caster (app uf-expr e e-fvars)) (values (Fn-Caster e) e-fvars)]
;;     ;; control flow for effects
;;     [(Begin (app uf-expr* e* fv1) (app uf-expr e fv2))
;;      (values (Begin e* e) (set-union fv1 fv2))]
;;     ;; Type Representation
;;     [(Type t) (values (Type t) mt-set)]
;;     [(Type-Tag (app uf-expr e fv))
;;      (values (Type-Tag e) fv)]
;;     [(Type-GRef-Of (app uf-expr e f*))
;;      (values (Type-GRef-Of e) f*)]
;;     [(Type-GVect-Of (app uf-expr e f*))
;;      (values (Type-GVect-Of e) f*)]
;;     [(Type-Fn-arg (app uf-expr e e-fvars) (app uf-expr i i-fvars))
;;      (values (Type-Fn-arg e i) (set-union e-fvars i-fvars))]
;;     [(Type-Fn-return (app uf-expr e e-fvars))
;;      (values (Type-Fn-return e) e-fvars)]
;;     [(Type-Fn-arity (app uf-expr e e-fvars))
;;      (values (Type-Fn-arity e) e-fvars)]
;;     ;; Begin new stuff TODO take out this comment once the code works
;;     [(Type-Dyn-Huh (app uf-expr e fv)) (values (Type-Dyn-Huh e) fv)]
;;     [(Type-Fn-Huh (app uf-expr e fv)) (values (Type-Fn-Huh e) fv)]
;;     [(Type-GRef-Huh (app uf-expr e fv)) (values (Type-GRef-Huh e) fv)]
;;     [(Type-GVect-Huh (app uf-expr e fv)) (values (Type-GVect-Huh e) fv)]
;;     ;; Code Representation
;;     [(Code-Label u) (values (Code-Label u) mt-set)]
;;     [(App-Code (app uf-expr e e-fv) (app uf-expr* e* e*-fv))
;;      (values (App-Code e e*) (set-union e-fv e*-fv))]
;;     [(App-Fn (app uf-expr e e-fv) (app uf-expr* e* e*-fv))
;;      (values (App-Fn e e*) (set-union e-fv e*-fv))]
;;     [(App-Fn-or-Proxy i (app uf-expr e e-fv) (app uf-expr* e* e*-fv))
;;      (values (App-Fn-or-Proxy i e e*) (set-union e-fv e*-fv))]
;;     ;; Coercion Representation Stuff
;;     [(Quote-Coercion c)
;;      (values (Quote-Coercion c) mt-set)]
;;     [(HC (app uf-expr p? p?-fv) (app uf-expr t1 t1-fv) (app uf-expr l l-fv)
;;          (app uf-expr i? i?-fv) (app uf-expr t2 t2-fv)
;;          (app uf-expr m  m-fv))
;;      (values (HC p? t1 l i? t2 m)
;;              (set-union p?-fv t1-fv l-fv i?-fv t2-fv m-fv))]
;;     [(HC-Inject-Huh (app uf-expr h h-fv))
;;      (values (HC-Inject-Huh h) h-fv)]
;;     [(HC-Project-Huh (app uf-expr h h-fv))
;;      (values  (HC-Project-Huh h) h-fv)]
;;     [(HC-Identity-Huh (app uf-expr h h-fv))
;;      (values (HC-Identity-Huh h) h-fv)]
;;     [(HC-Label (app uf-expr h h-fv))
;;      (values (HC-Label h) h-fv)]
;;     [(HC-T1 (app uf-expr h h-fv))
;;      (values (HC-T1 h) h-fv)]
;;     [(HC-T2 (app uf-expr h h-fv))
;;      (values (HC-T2 h) h-fv)]
;;     [(HC-Med (app uf-expr h h-fv))
;;      (values (HC-Med h) h-fv)]
;;     [(Id-Coercion-Huh (app uf-expr e fv))
;;      (values (Id-Coercion-Huh e) fv)]
;;     [(Fn-Coercion-Huh (app uf-expr e fv))
;;      (values (Fn-Coercion-Huh e) fv)]
;;     [(Make-Fn-Coercion
;;       u (app uf-expr e1 fv1) (app uf-expr e2 fv2) (app uf-expr e3 fv3))
;;      (values (Make-Fn-Coercion u e1 e2 e3)
;;              (set-union fv1 fv2 fv3))]
;;     [(Fn-Coercion-Arity (app uf-expr e fv))
;;      (values (Fn-Coercion-Arity e) fv)]
;;     [(Fn-Coercion (app uf-expr* e* fv1) (app uf-expr e fv2))
;;      (values (Fn-Coercion e* e) (set-union fv1 fv2))]
;;     [(Fn-Coercion-Arg (app uf-expr e1 fv1)(app uf-expr e2 fv2))
;;      (values (Fn-Coercion-Arg e1 e2) (set-union fv1 fv2))]
;;     [(Fn-Coercion-Return (app uf-expr e fv))
;;      (values (Fn-Coercion-Return e) fv)]
;;     [(Id-Fn-Coercion (app uf-expr a a-fv))
;;      (values (Id-Fn-Coercion a) a-fv)]
;;     [(Fn-Coercion-Arg-Set! (app uf-expr f f-fv) (app uf-expr i i-fv) (app uf-expr a a-fv))
;;      (values (Fn-Coercion-Arg-Set! f i a) (set-union f-fv i-fv a-fv))]
;;     [(Fn-Coercion-Return-Set! (app uf-expr f f-fv) (app uf-expr r r-fv))
;;      (values (Fn-Coercion-Return-Set! f r) (set-union f-fv r-fv))]
;;     [(Tuple-Coercion-Item-Set! (app uf-expr t t-fv) (app uf-expr i i-fv) (app uf-expr e e-fv))
;;      (values (Tuple-Coercion-Item-Set! t i e) (set-union t-fv i-fv e-fv))]
;;     [(Id-Tuple-Coercion (app uf-expr a a-fv))
;;      (values (Id-Tuple-Coercion a) a-fv)]
;;     [(Ref-Coercion (app uf-expr e1 fv1) (app uf-expr e2 fv2) (app uf-expr e3 fv3))
;;      (values (Ref-Coercion e1 e2 e3) (set-union fv1 fv2 fv3))]
;;     [(Ref-Coercion-Huh (app uf-expr e fv))
;;      (values (Ref-Coercion-Huh e) fv)]
;;     [(Ref-Coercion-Read (app uf-expr e fv))
;;      (values (Ref-Coercion-Read e) fv)]
;;     [(Ref-Coercion-Write (app uf-expr e fv))
;;      (values (Ref-Coercion-Write e) fv)]
;;     [(Ref-Coercion-Ref-Huh (app uf-expr e fv))
;;      (values (Ref-Coercion-Ref-Huh e) fv)]
;;     [(Sequence-Coercion (app uf-expr e1 fv1) (app uf-expr e2 fv2))
;;      (values (Sequence-Coercion e1 e2) (set-union fv1 fv2))]
;;     [(Sequence-Coercion-Huh (app uf-expr e fv))
;;      (values (Sequence-Coercion-Huh e) fv)]
;;     [(Sequence-Coercion-Fst (app uf-expr e fv))
;;      (values (Sequence-Coercion-Fst e) fv)]
;;     [(Sequence-Coercion-Snd (app uf-expr e fv))
;;      (values (Sequence-Coercion-Snd e) fv)]
;;     [(Project-Coercion (app uf-expr e1 fv1) (app uf-expr e2 fv2))
;;      (values (Project-Coercion e1 e2) (set-union fv1 fv2))]
;;     [(Project-Coercion-Huh (app uf-expr e fv))
;;      (values (Project-Coercion-Huh e) fv)]
;;     [(Project-Coercion-Type (app uf-expr e fv))
;;      (values (Project-Coercion-Type e) fv)]
;;     [(Project-Coercion-Label (app uf-expr e fv))
;;      (values (Project-Coercion-Label e) fv)]
;;     [(Inject-Coercion (app uf-expr e fv))
;;      (values (Inject-Coercion e) fv)]
;;     [(Inject-Coercion-Type (app uf-expr e fv))
;;      (values (Inject-Coercion-Type e) fv)]
;;     [(Inject-Coercion-Huh (app uf-expr e fv))
;;      (values (Inject-Coercion-Huh e) fv)]
;;     [(Failed-Coercion (app uf-expr e fv))
;;      (values (Failed-Coercion e) fv)]
;;     [(Failed-Coercion-Huh (app uf-expr e fv))
;;      (values (Failed-Coercion-Huh e) fv)]
;;     [(Failed-Coercion-Label (app uf-expr e fv))
;;      (values (Failed-Coercion-Label e) fv)]
;;     ;; Function Proxy Representation
;;     [(Fn-Proxy i (app uf-expr e1 fv1)(app uf-expr e2 fv2))
;;      (values (Fn-Proxy i e1 e2) (set-union fv1 fv2))]
;;     [(Fn-Proxy-Huh (app uf-expr e fv))
;;      (values (Fn-Proxy-Huh e) fv)]
;;     [(Fn-Proxy-Closure (app uf-expr e fv))
;;      (values (Fn-Proxy-Closure e) fv)]
;;     [(Fn-Proxy-Coercion (app uf-expr e fv))
;;      (values (Fn-Proxy-Coercion e) fv)]
;;     ;; Gaurded Representation
;;     [(Unguarded-Box (app uf-expr e fv))
;;      (values (Unguarded-Box e) fv)]
;;     [(Unguarded-Box-Ref (app uf-expr e fv))
;;      (values (Unguarded-Box-Ref e) fv)]
;;     [(Unguarded-Box-Set! (app uf-expr e1 fv1) (app uf-expr e2 fv2))
;;      (values (Unguarded-Box-Set! e1 e2) (set-union fv1 fv2))]
;;     [(Unguarded-Vect (app uf-expr e1 fv1) (app uf-expr e2 fv2))
;;      (values (Unguarded-Vect e1 e2) (set-union fv1 fv2))]
;;     [(Unguarded-Vect-Ref (app uf-expr e1 fv1) (app uf-expr e2 fv2))
;;      (values (Unguarded-Vect-Ref e1 e2) (set-union fv1 fv2))]
;;     [(Unguarded-Vect-Set!
;;       (app uf-expr e1 fv1) (app uf-expr e2 fv2) (app uf-expr e3 fv3))
;;      (values (Unguarded-Vect-Set! e1 e2 e3) (set-union fv1 fv2 fv3))]
;;     [(Guarded-Proxy-Huh (app uf-expr e fv))
;;      (values (Guarded-Proxy-Huh e) fv)]
;;     [(Guarded-Proxy (app uf-expr e fv0) r)
;;      (match r
;;        [(Twosome (app uf-expr t1 fv1) (app uf-expr t2 fv2) (app uf-expr l fv3))
;;         (values (Guarded-Proxy e (Twosome t1 t2 l))
;;                 (set-union fv0 fv1 fv2 fv3))]
;;        [(Coercion (app uf-expr c fv1))
;;         (values (Guarded-Proxy e (Coercion c)) (set-union fv0 fv1))])]
;;     [(Guarded-Proxy-Ref (app uf-expr e fv))
;;      (values (Guarded-Proxy-Ref e) fv)]
;;     [(Guarded-Proxy-Source (app uf-expr e fv))
;;      (values (Guarded-Proxy-Source e) fv)]
;;     [(Guarded-Proxy-Target (app uf-expr e fv))
;;      (values (Guarded-Proxy-Target e) fv)]
;;     [(Guarded-Proxy-Blames (app uf-expr e fv))
;;      (values (Guarded-Proxy-Blames e) fv)]
;;     [(Guarded-Proxy-Coercion (app uf-expr e fv))
;;      (values (Guarded-Proxy-Coercion e) fv)]
;;     [(Unguarded-Vect-length (app uf-expr e fv)) (values (Unguarded-Vect-length e) fv)]
;;     [(Mbox (app uf-expr e fv) t) (values (Mbox e t) fv)]
;;     [(Mbox-val-set! (app uf-expr e1 fv1) (app uf-expr e2 fv2))
;;      (values (Mbox-val-set! e1 e2) (set-union fv1 fv2))]
;;     [(Mbox-val-ref (app uf-expr e fv)) (values (Mbox-val-ref e) fv)]
;;     [(Mbox-rtti-set! (app uf-expr addr fv1) (app uf-expr e fv2))
;;      (values (Mbox-rtti-set! addr e) (set-union fv1 fv2))]
;;     [(Mbox-rtti-ref (app uf-expr addr fv))
;;      (values (Mbox-rtti-ref addr) fv)]
;;     [(Mvector (app uf-expr e1 fv1) (app uf-expr e2 fv2) t)
;;      (values (Mvector e1 e2 t) (set-union fv1 fv2))]
;;     [(Mvector-length (app uf-expr e fv)) (values (Mvector-length e) fv)]
;;     [(Mvector-val-set! (app uf-expr e1 fv1) (app uf-expr e2 fv2) (app uf-expr e3 fv3))
;;      (values (Mvector-val-set! e1 e2 e3) (set-union fv1 fv2 fv3))]
;;     [(Mvector-val-ref (app uf-expr e1 fv1) (app uf-expr e2 fv2))
;;      (values (Mvector-val-ref e1 e2) (set-union fv1 fv2))]
;;     [(Mvector-rtti-set! (app uf-expr addr fv1) (app uf-expr e fv2))
;;      (values (Mvector-rtti-set! addr e) (set-union fv1 fv2))]
;;     [(Mvector-rtti-ref (app uf-expr addr fv))
;;      (values (Mvector-rtti-ref addr) fv)]
;;     [(Type-MVect (app uf-expr e fv)) (values (Type-MVect e) fv)]
;;     [(Type-MVect-Huh (app uf-expr e fv)) (values (Type-MVect-Huh e) fv)]
;;     [(Type-MVect-Of (app uf-expr e fv)) (values (Type-MVect-Of e) fv)]
;;     [(MVect-Coercion-Huh (app uf-expr e fv)) (values (MVect-Coercion-Huh e) fv)]
;;     [(MVect-Coercion-Type (app uf-expr e fv)) (values (MVect-Coercion-Type e) fv)]
;;     [(MVect-Coercion (app uf-expr e fv)) (values (MVect-Coercion e) fv)]
;;     [(Make-GLB-Two-Fn-Types e1 (app uf-expr e2 fv1) (app uf-expr e3 fv2))
;;      (values (Make-GLB-Two-Fn-Types e1 e2 e3) (set-union fv1 fv2))]
;;     [(Make-GLB-Two-Tuple-Types e1 (app uf-expr e2 fv1) (app uf-expr e3 fv2))
;;      (values (Make-GLB-Two-Tuple-Types e1 e2 e3) (set-union fv1 fv2))]
;;     [(MRef-Coercion-Huh (app uf-expr e fv)) (values (MRef-Coercion-Huh e) fv)]
;;     [(MRef-Coercion-Type (app uf-expr e fv)) (values (MRef-Coercion-Type e) fv)]
;;     [(MRef-Coercion (app uf-expr e fv)) (values (MRef-Coercion e) fv)]
;;     [(Type-GRef (app uf-expr e fv)) (values (Type-GRef e) fv)]
;;     [(Type-GVect (app uf-expr e fv)) (values (Type-GVect e) fv)]
;;     [(Type-MRef (app uf-expr e fv)) (values (Type-MRef e) fv)]
;;     [(Type-MRef-Huh (app uf-expr e fv)) (values (Type-MRef-Huh e) fv)]
;;     [(Type-MRef-Of (app uf-expr e fv)) (values (Type-MRef-Of e) fv)]
;;     [(Error (app uf-expr e fv)) (values (Error e) fv)]
;;     [(Create-tuple (app uf-expr* e* e*-fvars)) (values (Create-tuple e*) e*-fvars)]
;;     [(Copy-Tuple (app uf-expr n fv1) (app uf-expr v fv2))
;;      (values (Copy-Tuple n v) (set-union fv1 fv2))]
;;     [(Tuple-proj (app uf-expr e e-fvars) (app uf-expr i i-fvars))
;;      (values (Tuple-proj e i) (set-union e-fvars i-fvars))]
;;     [(Tuple-Coercion-Huh (app uf-expr e e-fvars)) (values (Tuple-Coercion-Huh e) e-fvars)]
;;     [(Tuple-Coercion-Num (app uf-expr e e-fvars)) (values (Tuple-Coercion-Num e) e-fvars)]
;;     [(Tuple-Coercion-Item (app uf-expr e e-fvars) (app uf-expr i i-fv))
;;      (values (Tuple-Coercion-Item e i) (set-union e-fvars i-fv))]
;;     [(Cast-Tuple uid
;;                  (app uf-expr e1 fv1) (app uf-expr e2 fv2)
;;                  (app uf-expr e3 fv3) (app uf-expr e4 fv4))
;;      (values (Cast-Tuple uid e1 e2 e3 e4) (set-union fv1 fv2 fv3 fv4))]
;;     [(Cast-Tuple-In-Place uid
;;                  (app uf-expr e1 fv1) (app uf-expr e2 fv2)
;;                  (app uf-expr e3 fv3) (app uf-expr e4 fv4)
;;                  (app uf-expr e5 fv5))
;;      (values (Cast-Tuple-In-Place uid e1 e2 e3 e4 e5) (set-union fv1 fv2 fv3 fv4 fv5))]
;;     [(Coerce-Tuple uid (app uf-expr e1 fv1) (app uf-expr e2 fv2))
;;      (values (Coerce-Tuple uid e1 e2) (set-union fv1 fv2))]
;;     [(Coerce-Tuple-In-Place uid (app uf-expr e1 fv1) (app uf-expr e2 fv2) (app uf-expr e3 fv3))
;;      (values (Coerce-Tuple-In-Place uid e1 e2 e3) (set-union fv1 fv2 fv3))]
;;     [(Type-Tuple-Huh (app uf-expr e e-fvars)) (values (Type-Tuple-Huh e) e-fvars)]
;;     [(Type-Tuple-num (app uf-expr e e-fvars)) (values (Type-Tuple-num e) e-fvars)]
;;     [(Type-Tuple-item (app uf-expr e e-fv) (app uf-expr i i-fv))
;;      (values (Type-Tuple-item e i) (set-union e-fv i-fv))]
;;     [(Make-Tuple-Coercion uid (app uf-expr e1 fv1) (app uf-expr e2 fv2) (app uf-expr e3 fv3))
;;      (values (Make-Tuple-Coercion uid e1 e2 e3) (set-union fv1 fv2 fv3))]
;;     [(Mediating-Coercion-Huh (app uf-expr e fv)) (values (Mediating-Coercion-Huh e) fv)]
;;     [(Construct t v (app uf-expr* e* fv))
;;      (values (Construct t v e*) fv)]
;;     [(Access t f (app uf-expr e e-fv) i?)
;;      (if i?
;;          (let-values ([(i i-fv) (uf-expr i?)])
;;            (values (Access t f e i) (set-union e-fv i-fv)))
;;          (values (Access t f e #f) e-fv))]
;;     [(Check t p (app uf-expr e e-fv) (app uf-expr* e* e*-fv))
;;      (values (Check t p e e*) (set-union e-fv e*-fv))]
;;     [(Type-Mu-Huh (app uf-expr e e-fv))
;;      (values (Type-Mu-Huh e) e-fv)]
;;     [(Type-Mu-Body (app uf-expr e e-fv))
;;      (values (Type-Mu-Body e) e-fv)]
;;     [other (error 'uncover-free "unmatched ~a" other)]))
;;   (debug 'cast/uncover-free/uf-expr/e e ret-e ret-fv)
;;   (values ret-e ret-fv))


;; (: uf-expr* (-> (Listof CoC4-Expr) (Values (Listof CoC5-Expr) (Setof Uid))))
;; (define (uf-expr* e*)
;;   (: help (CoC4-Expr (Pair CoC5-Expr* (Setof Uid)) -> (Pair CoC5-Expr* (Setof Uid))))
;;   (define (help e a)
;;     (let-values ([(e  fv1) (uf-expr e)]
;;                  [(e* fv2) (values (car a) (cdr a))])
;;       (cons (cons e e*) (set-union fv1 fv2))))
;;   (define acc (cons '() (ann mt-set (Setof Uid))))
;;   (define e.fv (foldr help acc e*))
;;   (values (car e.fv) (cdr e.fv)))

;; #|
;; (: uf-lambda (CoC4-Lambda . -> . (Values CoC5-Lambda (Setof Uid))))
;; (define (uf-lambda lam)
;;   (match-let ([(Lambda u* (Castable ctr? (app uf-expr e fvars))) lam])
;;     (let* ([fvars  (set-subtract fv (list->set u*))]
;;            [fvars^ (if ctr? (set-add fvars ctr?) fvars)])
;;       (values (Lambda f* (Free ctr? (set->list fvars) e)) fvars^))))
;; |#

;; (: uf-bnd-lambda*
;;    (-> CoC4-Bnd-Lambda*
;;        (values (Setof Uid)
;;                (Listof (Pairof Uid CoC5-Lambda))
;;                (Setof Uid))))
;; (define (uf-bnd-lambda* b*)
;;   (for/fold ([bv   : (Setof Uid)      mt-set]
;;              [bnd* : CoC5-Bnd-Lambda*   '()]
;;              [fv   : (Setof Uid)      mt-set])
;;             ([b    : CoC4-Bnd-Lambda     b*])
;;     (match-let ([(cons u (Lambda u* (Castable ctr? (app uf-expr e e-fv)))) b])
;;       (let* ([l-fv (set-subtract e-fv (list->set u*))]
;;              #;(TODO the second call to fvars is bloating closures and is could likely be removed)
;;              #;[l-fv (if ctr? (set-add l-fv ctr?) l-fv)]
;;              )
;;         (values (set-add bv u)
;;                 (cons (cons u (Lambda u* (Free ctr? (set->list l-fv) e)))
;;                       bnd*)
;;                 (set-union fv l-fv))))))

;; #|

;; (if (null? b*)
;;     (values mt-set '() mt-set)
;;     (let ([a (car b*)] [d (cdr b*)]) ;; free the list
;;       (let-values ([(u* b* f*) (uf-bnd-lambda* d)])
;;         (match-let ([(cons u (app uf-lambda rhs rhs-f*)) a])
;;           (values (set-add u* u)
;;                   (cons (cons u rhs) b*)
;;                   (set-union f* rhs-f*))))))|#

;; (: uf-bnd-data*
;;    (-> CoC4-Bnd-Data*
;;        (values (Setof Uid) CoC5-Bnd-Data* (Setof Uid))))
;; (define (uf-bnd-data* b*)
;;   (for/fold ([bv   : (Setof Uid)    mt-set]
;;              [bnd* : CoC5-Bnd-Data*   '()]
;;              [fv   : (Setof Uid)    mt-set])
;;             ([b : CoC4-Bnd-Data        b*])
;;     (match-let ([(cons u (app uf-expr e e-fv)) b])
;;       (values (set-add bv u)
;;               (cons (cons u e) bnd*)
;;               (set-union e-fv fv)))))

;; (: uf-bnd-code* (CoC4-Bnd-Code* -> CoC5-Bnd-Code*))
;; (define (uf-bnd-code* b*)
;;   (for/list : (Listof CoC5-Bnd-Code) ([b : CoC4-Bnd-Code b*])
;;     (match-let ([(cons u (Code u* (app uf-expr e e-fv))) b])
;;       (unless (set-empty? (set-subtract e-fv (list->set u*)))
;;         (error 'uncover-free "Code should never have free variables ~a\n\t~a" b (set-subtract e-fv (list->set u*))))
;;       (cons u (Code u* e)))))


  
;; (define cr  (cast-representation))
;; (define fr 'Hybrid)
;; (match-let ([(Prog (list name count type)
;;                (Let-Static* mu-tbnd*
;;                               tbnd*
;;                               mu-cbnd*
;;                               cbnd*
;;                               exp)) prgm])
;;     (let* ([next : (Boxof Nat) (box count)]
;;            [bndp : (HashTable Integer BP) (make-hash)]
;;            [exp ;; This is an abusively long function call
;;             (cc-expr cr fr next bndp empty-env empty-env no-selfp exp)]
;;            [next : Nat (unbox next)])
;;       (Prog (list name count type)
;;         (Let-Static* mu-tbnd*
;;                      tbnd*
;;                      mu-cbnd*
;;                      cbnd*
;;                      (LetP (hash-values bndp) exp)))))

;; (define-type BP  CoC6-Bnd-Procedure)
;; (define-type BC  CoC6-Bnd-Closure)
;; (define-type BP* CoC6-Bnd-Procedure*)
;; (define-type BC* CoC6-Bnd-Closure*)
;; (define-type BD* CoC6-Bnd-Data*)

;; (define-type Env (HashTable Uid CoC6-Expr))
;; (define empty-env : Env (hash))

;; (define-type SelfP (-> Uid (Option CoC6-Expr)))
;; (define no-selfp : SelfP
;;   (lambda (u) #f))



;; (: cc-expr (Cast-Representation
;;             Fn-Proxy-Representation
;;             (Boxof Nat) (HashTable Integer BP)
;;             Env Env SelfP CoC5-Expr
;;             -> CoC6-Expr))
;; (define (cc-expr cast-rep fn-proxy-rep next new-bnd-procs
;;                  code-env data-env selfp exp)
;;   ;; This is the only piece of code that should touch the unique counter
;;   (: next-uid! (String -> Uid))
;;   (define (next-uid! x)
;;     (let ([n (unbox next)])
;;       (set-box! next (add1 n))
;;       (Uid x n)))

;;   ;; Lookup or Create a Apply-Hybrid-Proxy-Fn



;;   ;; Make a procedure that references a closure data structure
;;   (: cc-make-proc (Env -> ((Pairof Uid CoC5-Bnd-Lambda) -> BP)))
;;   (define ((cc-make-proc code-env) b)
;;     ;; destructure the inner-closure-id and lambda binding
;;     (match-let ([(cons clos (cons code (Lambda fml* (Free ctr? uid* e)))) b])
;;       ;; create expressions that represent accessing the closure for each free var
;;       (define clos-ref* (map (mk-clos-ref clos) uid*))
;;       ;; create a new environment that maps uses of variables to closure access
;;       (define data-env   (extend* (hash) uid* clos-ref*))
;;       ;; create a new self reference check
;;       (define self (Var clos))
;;       (define (selfp [u : Uid]) (and (equal? u code) self))
;;       ;; Process the body of the lambda replacing free variable uses
;;       ;; with closure accesses
;;       (define body (recur/env code-env data-env selfp e))
;;       ;; return the new procedure binding after recuring on
;;       (cons code (Procedure clos fml* code ctr? uid* body))))


;;   ;; Make a closure data structure that can be reference by a procedure
;;   (: cc-make-clos (Env Env SelfP -> ((Pairof Uid CoC5-Bnd-Lambda) -> BC)))
;;   (define ((cc-make-clos code-env data-env selfp) b)
;;     (match-let ([(cons clos (cons code (Lambda fml* (Free ctr? uid* _)))) b])
;;       ;; Get the Variables that need to be bound by the closure
;;       (define bound* (map (lookup-in data-env selfp) uid*))
;;       (define ctr    (if ctr? (Code-Label ctr?) #f))
;;       (define data   (Closure-Data (Code-Label code) ctr bound*))
;;       ;; return the new closure binding
;;       (cons clos data)))

;;   ;; Process Expression in a particular closure environment
;;   (: recur/env (Env Env SelfP CoC5-Expr -> CoC6-Expr))
;;   (define (recur/env code-env data-env selfp exp)
    
;;     ;; Build new closure binding forms
;;     (: cc-bnd-lambda* (CoC5-Bnd-Lambda* -> (Values BP* BC* Env Env)))
;;     (define (cc-bnd-lambda* b*)
;;       ;; Make some code labels from the original bindings
;;       (define cp* : Uid* (map (inst car Uid Any) b*))
;;       (define cl* : CoC6-Expr* (map (inst Code-Label Uid) cp*))
;;       ;; Make some uids for the inner(ic) and outer(oc) closure variables
;;       (define ic* : Uid* (map (clos-uid next-uid!) cp*))
;;       (define oc* : Uid* (map (clos-uid next-uid!) cp*))
;;       ;; Extend the environments with new closure and code info
;;       (define oc-var* (map (inst Var Uid) oc*))
;;       (define ic-env (extend* code-env ic* cl*))
;;       (define oc-env (extend* code-env oc* cl*))
;;       (define od-env (extend* data-env cp* oc-var*))
;;       (define ic.b*  (map (inst cons Uid CoC5-Bnd-Lambda) ic* b*))
;;       (define oc.b*  (map (inst cons Uid CoC5-Bnd-Lambda) oc* b*))
;;       ;; Make the new bindings
;;       (define bp* : BP* (map (cc-make-proc ic-env) ic.b*))
;;       (define bd* : BC* (map (cc-make-clos oc-env od-env selfp) oc.b*))
;;       ;; return the new bindings and environments
;;       (values bp* bd* oc-env od-env))

;;     ;; recure through the expressions in the grammar
;;     (: recur (CoC5-Expr -> CoC6-Expr))
;;     (define (recur e)
;;       (match e
;;         ;; The interesting cases:
;;         ;; letrec are actually code bindings with shared state closure creating bodies
;;         [(Letrec b* e)
;;          (let-values ([(bp bc c-env d-env) (cc-bnd-lambda* b*)])
;;            (LetP bp (LetC bc (recur/env c-env d-env selfp e))))]
;;         ;; Function cast extraction removes the cast closure (could optimize to cast pointer)
;;         [(Fn-Caster e) (Closure-caster (recur e))]
;;         ;; Applications get the code pointer and pass the closure as the first argument
;;         [(App-Fn (app recur e) (app recur* e*))
;;          ;; When optimize known call is in effect any known closure
;;          ;; that is applied has the code label inlined
;;          ;; otherwise the closure pointer is extracted
;;          (cond
;;            [(Var? e)
;;             (define code?
;;               (and (optimize-known-call?) (hash-ref code-env (Var-id e) #f)))
;;             (cond
;;               [code? (App-Closure code? e e*)]
;;               [else (App-Closure (Closure-code e) e e*)])]
;;            [else
;;             (define id  (next-uid! "tmp_closure"))
;;             (define var (Var id))
;;             (Let (list (cons id e))
;;                  (App-Closure (Closure-code var) var e*))])]
;;         ;; If we are using hybrid representation we can treat
;;         ;; this like a normal apply-closure.
;;         ;; Alternatively if we are using data representation
;;         ;; we need to cast the arguments
;;         [(App-Fn-or-Proxy cast-uid e e*)
;;          (case cast-rep
;;            [(Coercions Hyper-Coercions)
;;             (case fn-proxy-rep
;;               ;; If it is a hybrid representation then proxies and closures
;;               ;; are applied the same way.
;;               [(Hybrid)
;;                (recur (App-Fn e e*))]
;;               ;; If it a data representation and it is a known definition
;;               ;; site then it can't be a proxy
;;               [(Data)
;;                (cond
;;                  [(and (Var? e) (optimize-known-call?)
;;                        (hash-ref code-env (Var-id e) #f))
;;                   => (lambda ([c : CoC6-Expr])
;;                        (App-Closure c e (recur* e*)))]
;;                  ;; othewise we build code to cast the arguments
;;                  ;; at the call site.
;;                  [else
;;                   (define e^  : CoC6-Expr  (recur  e))
;;                   (define e*^ : CoC6-Expr* (recur* e*))

;;                   (define prox (next-uid! "maybe_proxy"))
;;                   (define pvar (Var prox))
;;                   (define func (next-uid! "closure"))
;;                   (define fvar (Var prox))
;;                   (define crcn (next-uid! "coercion"))
;;                   (define cvar (Var crcn))
;;                   (Let (list (cons prox e^))
;;                        (If (Fn-Proxy-Huh (Var prox))
;;                            (Let (list (cons func (Fn-Proxy-Closure pvar))
;;                                       (cons crcn (Fn-Proxy-Coercion pvar)))
;;                                 (cast-apply-cast cast-uid fvar e*^ cvar))
;;                            (App-Closure (Closure-code pvar) pvar e*^)))])]
;;               [else (error 'covert-closures "Unkown Fn-Proxy Representaion")])]
;;            [else (error 'covert-closures "Unkown Cast Representaion")])]

;;         ;; varibles that are free are extracted from the closure
;;         ;; while variable that are not bound by the closure are rebuilt
;;         [(Var u) (lookup data-env selfp u)]
;;         ;; The rest of the cases are just recuring into sub expressions
;;         [(Let (app (cc-bnd-data* recur) b*) (app recur e)) (Let b* e)]
;;         [(If (app recur t) (app recur c) (app recur a)) (If t c a)]
;;         [(Switch e c* d)
;;          (: recur-case : (Switch-Case CoC5-Expr) -> (Switch-Case CoC6-Expr))
;;          (define/match (recur-case c)
;;            [((cons l r)) (cons l (recur r))])
;;          (Switch (recur e) (map recur-case c*) (recur d))]
;;         [(Op p (app recur* e*)) (Op p e*)]
;;         [(and nop (No-Op)) nop]
;;         [(Quote k) (Quote k)]
;;         [(Tag t)   (Tag t)]
;;         ;; Observables Representation
;;         [(Blame (app recur e)) (Blame e)]
;;         [(Observe (app recur e) t) (Observe e t)]
;;         ;; control flow for effects
;;         [(Begin (app recur* e*) (app recur e)) (Begin e* e)]
;;         [(Repeat i (app recur e1) (app recur e2) a (app recur e3) (app recur e4))
;;          (Repeat i e1 e2 a e3 e4)]
;;         [(Break-Repeat) (Break-Repeat)]
;;         ;; Type Representation
;;         [(Type t) (Type t)]
;;         [(Type-Tag (app recur e)) (Type-Tag e)]
;;         [(Type-GRef-Of (app recur e)) (Type-GRef-Of e)]
;;         [(Type-GVect-Of (app recur e)) (Type-GVect-Of e)]
;;         [(Type-Fn-arg (app recur e) (app recur i)) (Type-Fn-arg e i)]
;;         [(Type-Fn-return (app recur e)) (Type-Fn-return e)]
;;         [(Type-Fn-arity (app recur e)) (Type-Fn-arity e)]
;;         [(Type-Fn-Huh (app recur e)) (Type-Fn-Huh e)]
;;         [(Type-GVect-Huh (app recur e)) (Type-GVect-Huh e)]
;;         [(Type-GRef-Huh (app recur e)) (Type-GRef-Huh e)]
;;         [(Type-Dyn-Huh (app recur e)) (Type-Dyn-Huh e)]
        
;;         [(Labels (app (cc-bnd-code* recur) b*) (app recur e))
;;          (Labels b* e)]
;;         [(App-Code (app recur e) (app recur* e*))
;;          (App-Code e e*)]
;;         [(Code-Label u)  (Code-Label u)]

;;         ;; Coercion Representation Stuff
;;         [(Quote-Coercion c)
;;          (Quote-Coercion c)]
;;         [(HC (app recur p?) (app recur t1) (app recur lbl)
;;              (app recur i?) (app recur t2)
;;              (app recur m))
;;          (HC p? t1 lbl i? t2 m)]
;;         [(HC-Inject-Huh (app recur h)) (HC-Inject-Huh h)]
;;         [(HC-Project-Huh (app recur h)) (HC-Project-Huh h)]
;;         [(HC-Identity-Huh (app recur h)) (HC-Identity-Huh h)]
;;         [(HC-Label (app recur h)) (HC-Label h)]
;;         [(HC-T1 (app recur h)) (HC-T1 h)]
;;         [(HC-T2 (app recur h)) (HC-T2 h)]
;;         [(HC-Med (app recur h)) (HC-Med h)]
;;         [(Id-Coercion-Huh (app recur e))
;;          (Id-Coercion-Huh e)]
;;         [(Fn-Coercion-Huh (app recur e))
;;          (Fn-Coercion-Huh e)]
;;         [(Make-Fn-Coercion u (app recur e1) (app recur e2) (app recur e3))
;;          (Make-Fn-Coercion u e1 e2 e3)]
;;         [(Fn-Coercion (app recur* e*) (app recur e))
;;          (Fn-Coercion e* e)]
;;         [(Fn-Coercion-Arity (app recur e))
;;          (Fn-Coercion-Arity e)]
;;         [(Fn-Coercion-Arg (app recur e1)(app recur e2))
;;          (Fn-Coercion-Arg e1 e2)]
;;         [(Fn-Coercion-Return (app recur e))
;;          (Fn-Coercion-Return e)]
;;         [(Id-Fn-Coercion (app recur a)) (Id-Fn-Coercion a)]
;;         [(Fn-Coercion-Arg-Set! (app recur f) (app recur i) (app recur a))
;;          (Fn-Coercion-Arg-Set! f i a)]
;;         [(Fn-Coercion-Return-Set! (app recur f) (app recur r))
;;          (Fn-Coercion-Return-Set! f r)]
;;         [(Tuple-Coercion-Item-Set! (app recur t) (app recur i) (app recur e))
;;          (Tuple-Coercion-Item-Set! t i e)]
;;         [(Id-Tuple-Coercion (app recur a))
;;          (Id-Tuple-Coercion a)]
;;         [(Ref-Coercion (app recur e1) (app recur e2) (app recur flag))
;;          (Ref-Coercion e1 e2 flag)]
;;         [(Ref-Coercion-Huh (app recur e))
;;          (Ref-Coercion-Huh e)]
;;         [(Ref-Coercion-Read (app recur e))
;;          (Ref-Coercion-Read e)]
;;         [(Ref-Coercion-Write (app recur e))
;;          (Ref-Coercion-Write e)]
;;         [(Ref-Coercion-Ref-Huh (app recur e))
;;          (Ref-Coercion-Ref-Huh e)]
;;         [(Sequence-Coercion (app recur e1) (app recur e2))
;;          (Sequence-Coercion e1 e2)]
;;         [(Sequence-Coercion-Huh (app recur e))
;;          (Sequence-Coercion-Huh e)]
;;         [(Sequence-Coercion-Fst (app recur e))
;;          (Sequence-Coercion-Fst e)]
;;         [(Sequence-Coercion-Snd (app recur e))
;;          (Sequence-Coercion-Snd e)]
;;         [(Project-Coercion (app recur e1) (app recur e2))
;;          (Project-Coercion e1 e2)]
;;         [(Project-Coercion-Huh (app recur e))
;;          (Project-Coercion-Huh e)]
;;         [(Project-Coercion-Type (app recur e))
;;          (Project-Coercion-Type e)]
;;         [(Project-Coercion-Label (app recur e))
;;          (Project-Coercion-Label e)]
;;         [(Inject-Coercion (app recur e))
;;          (Inject-Coercion e)]
;;         [(Inject-Coercion-Type (app recur e))
;;          (Inject-Coercion-Type e)]
;;         [(Inject-Coercion-Huh (app recur e))
;;          (Inject-Coercion-Huh e)]
;;         [(Failed-Coercion (app recur e))
;;          (Failed-Coercion e)]
;;         [(Failed-Coercion-Huh (app recur e))
;;          (Failed-Coercion-Huh e)]
;;         [(Failed-Coercion-Label (app recur e))
;;          (Failed-Coercion-Label e)]
;;         ;; Function Proxy Representation
        
;;         ;; Gaurded Representation
;;         [(Unguarded-Box (app recur e))(Unguarded-Box e)]
;;         [(Unguarded-Box-Ref (app recur e)) (Unguarded-Box-Ref e)]
;;         [(Unguarded-Box-Set! (app recur e1) (app recur e2))
;;          (Unguarded-Box-Set! e1 e2)]
;;         [(Unguarded-Vect (app recur e1) (app recur e2))
;;          (Unguarded-Vect e1 e2)]
;;         [(Unguarded-Vect-Ref (app recur e1) (app recur e2))
;;          (Unguarded-Vect-Ref e1 e2)]
;;         [(Unguarded-Vect-Set!
;;           (app recur e1) (app recur e2) (app recur e3))
;;          (Unguarded-Vect-Set! e1 e2 e3)]
;;         [(Guarded-Proxy-Huh (app recur e))
;;          (Guarded-Proxy-Huh e)]
;;         [(Guarded-Proxy (app recur e) r)
;;          (match r
;;            [(Twosome (app recur t1) (app recur t2) (app recur l))
;;             (Guarded-Proxy e (Twosome t1 t2 l))]
;;            [(Coercion (app recur c))
;;             (Guarded-Proxy e (Coercion c))])]
;;         [(Guarded-Proxy-Ref (app recur e))
;;          (Guarded-Proxy-Ref e)]
;;         [(Guarded-Proxy-Source (app recur e))
;;          (Guarded-Proxy-Source e)]
;;         [(Guarded-Proxy-Target (app recur e))
;;          (Guarded-Proxy-Target e)]
;;         [(Guarded-Proxy-Blames (app recur e))
;;          (Guarded-Proxy-Blames e)]
;;         [(Guarded-Proxy-Coercion (app recur e))
;;          (Guarded-Proxy-Coercion e)]
;;         [(Unguarded-Vect-length (app recur e)) (Unguarded-Vect-length e)]
;;         [(Mbox (app recur e) t) (Mbox e t)]
;;         [(Mbox-val-set! (app recur e1) (app recur e2)) (Mbox-val-set! e1 e2)]
;;         [(Mbox-val-ref (app recur e)) (Mbox-val-ref e)]
;;         [(Mbox-rtti-set! (app recur addr) (app recur e))
;;          (Mbox-rtti-set! addr e)]
;;         [(Mbox-rtti-ref (app recur addr)) (Mbox-rtti-ref addr)]
;;         [(Make-GLB-Two-Fn-Types e1 (app recur e2) (app recur e3))
;;          (Make-GLB-Two-Fn-Types e1 e2 e3)]
;;         [(Make-GLB-Two-Tuple-Types e1 (app recur e2) (app recur e3))
;;          (Make-GLB-Two-Tuple-Types e1 e2 e3)]
;;         [(MRef-Coercion-Huh (app recur e)) (MRef-Coercion-Huh e)]
;;         [(MRef-Coercion-Type (app recur e)) (MRef-Coercion-Type e)]
;;         [(MRef-Coercion (app recur e)) (MRef-Coercion e)]
;;         [(Type-GRef (app recur e)) (Type-GRef e)]
;;         [(Type-GVect (app recur e)) (Type-GVect e)]
;;         [(Type-MRef (app recur e)) (Type-MRef e)]
;;         [(Type-MRef-Huh (app recur e)) (Type-MRef-Huh e)]
;;         [(Type-MRef-Of (app recur e)) (Type-MRef-Of e)]
;;         [(Mvector (app recur e1) (app recur e2) t) (Mvector e1 e2 t)]
;;         [(Mvector-length (app recur e)) (Mvector-length e)]
;;         [(Mvector-val-set! (app recur e1) (app recur e2) (app recur e3)) (Mvector-val-set! e1 e2 e3)]
;;         [(Mvector-val-ref (app recur e1) (app recur e2)) (Mvector-val-ref e1 e2)]
;;         [(Mvector-rtti-set! (app recur addr) (app recur e))
;;          (Mvector-rtti-set! addr e)]
;;         [(Mvector-rtti-ref (app recur addr)) (Mvector-rtti-ref addr)]
;;         [(Type-MVect e) (Type-MVect (recur e))]
;;         [(Type-MVect-Huh e) (Type-MVect-Huh (recur e))]
;;         [(Type-MVect-Of e) (Type-MVect-Of (recur e))]
;;         [(MVect-Coercion-Huh e) (MVect-Coercion-Huh (recur e))]
;;         [(MVect-Coercion-Type e) (MVect-Coercion-Type (recur e))]
;;         [(MVect-Coercion e) (MVect-Coercion (recur e))]
;;         [(Error (app recur e)) (Error e)]
;;         [(Create-tuple (app recur* e*)) (Create-tuple e*)]
;;         [(Copy-Tuple (app recur n) (app recur v))
;;          (Copy-Tuple n v)]
;;         [(Tuple-proj e i) (Tuple-proj (recur e) (recur i))]
;;         [(Tuple-Coercion-Huh e) (Tuple-Coercion-Huh (recur e))]
;;         [(Tuple-Coercion-Num e) (Tuple-Coercion-Num (recur e))]
;;         [(Tuple-Coercion-Item e i) (Tuple-Coercion-Item (recur e) (recur i))]
;;         [(Coerce-Tuple uid e1 e2) (Coerce-Tuple uid (recur e1) (recur e2))]
;;         [(Coerce-Tuple-In-Place uid e1 e2 e3)
;;          (Coerce-Tuple-In-Place uid (recur e1) (recur e2) (recur e3))]
;;         [(Cast-Tuple uid e1 e2 e3 e4) (Cast-Tuple uid (recur e1) (recur e2) (recur e3) (recur e4))]
;;         [(Cast-Tuple-In-Place uid e1 e2 e3 e4 e5)
;;          (Cast-Tuple-In-Place uid (recur e1) (recur e2) (recur e3) (recur e4) (recur e5))]
;;         [(Type-Tuple-Huh e) (Type-Tuple-Huh (recur e))]
;;         [(Type-Tuple-num e) (Type-Tuple-num (recur e))]
;;         [(Type-Tuple-item e i) (Type-Tuple-item (recur e) (recur i))]
;;         [(Make-Tuple-Coercion uid t1 t2 lbl) (Make-Tuple-Coercion uid (recur t1) (recur t2) (recur lbl))]
;;         [(Mediating-Coercion-Huh e) (Mediating-Coercion-Huh (recur e))]
;;         [(Construct t v (app recur* e*))
;;          (Construct t v e*)]
;;         [(Access t f (app recur e) i?)
;;          (Access t f e (if i? (recur i?) #f))]
;;         [(Check t p (app recur e) (app recur* e*))
;;          (Check t p e e*)]
;;         [(Type-Mu-Huh (app recur e)) (Type-Mu-Huh e)]
;;         [(Type-Mu-Body (app recur e)) (Type-Mu-Body e)]
;;         [other (error 'Convert-Closures "unmatched ~a" other)]))

;;     ;; recur through a list of expressions
;;     (: recur* (CoC5-Expr* -> CoC6-Expr*))
;;     (define (recur* e*) (map recur e*))
    
;;     (recur exp))
;;   (recur/env code-env data-env selfp exp))

;; ;; THE following are helpers that don't require any
;; ;; of the internal state of this pass


;; ;; make an expression the expresses the notion of fetching
;; ;; a variable from a closure
;; (: mk-clos-ref : Uid -> (Uid -> CoC6-Expr))
;; (define (mk-clos-ref clos)
;;   (lambda ([fvar : Uid]) : CoC6-Expr
;;           (Closure-ref clos fvar)))

;; ;; Recur through bound data
;; (: cc-bnd-data*
;;    ((CoC5-Expr -> CoC6-Expr) -> CoC5-Bnd-Data* -> CoC6-Bnd-Data*))
;; (define ((cc-bnd-data* cc-expr) b*)
;;   (: help (CoC5-Bnd-Data -> CoC6-Bnd-Data))
;;   (define (help b)
;;     (cons (car b) (cc-expr (cdr b))))
;;   (map help b*))

;; ;; Recur through bound code
;; (: cc-bnd-code*
;;    ((CoC5-Expr -> CoC6-Expr) -> CoC5-Bnd-Code* -> CoC6-Bnd-Code*))
;; (define ((cc-bnd-code* cc-expr) b*)
;;   ;; Handle a single code binding 
;;   (: help (CoC5-Bnd-Code -> CoC6-Bnd-Code))
;;   (define (help b)
;;     (match-let ([(cons u (Code u* e)) b])
;;       (cons u (Code u* (cc-expr e)))))
;;   (map help b*))

;; ;; create a clos annotated unique name
;; (: clos-uid ((String -> Uid) -> (Uid -> Uid)))
;; (define ((clos-uid uid!) u)
;;   (uid! (string-append (Uid-prefix u) "_clos")))


;; ;; Lookup a variables local access instruction
;; ;; within an environment that represents 
;; (: lookup : Env SelfP Uid -> CoC6-Expr)
;; (define (lookup env selfp uid)
;;   ;; If optimize self reference is enabled then check
;;   ;; to see if this is a self reference.
;;   (or (and (optimize-self-reference?) (selfp uid))
;;       (hash-ref env uid (thunk (Var uid)))))

;; (: lookup-in : Env SelfP -> (Uid -> CoC6-Expr))
;; (define ((lookup-in env selfp) uid)
;;   (lookup env selfp uid))

;; (: extend* :  Env Uid* CoC6-Expr* -> Env)
;; (define (extend* env k* v*)
;;   (for/fold ([env : Env env])
;;             ([k : Uid k*] [v : CoC6-Expr v*])
;;     (hash-set env k v)))

;; Helpers that belong here

;; create a casted fn application call site
(: cast-apply-cast (Uid (Var Uid) (Listof CoC6-Expr) (Var Uid) -> CoC6-Expr))
(define (cast-apply-cast cast-uid fun arg* crcn)
  (define cast-label (Code-Label cast-uid))
  (define-values (v* b*)
    (for/lists ([v* : (Listof (Var Uid))]
                [b* : (Bnd* CoC6-Expr)])
               ([arg : CoC6-Expr arg*] [i (in-range 0)])
      (define u (next-uid! (format "fn-cast-arg~a" i)))
      (define arg-crcn (Fn-Coercion-Arg crcn (Quote i)))
      (values (Var u)
              (cons u (App-Code cast-label (list arg arg-crcn ZERO-EXPR))))))
  (define clos-app (Closure-App (Closure-Code fun) fun v*))
  (App-Code cast-label (list clos-app (Fn-Coercion-Return crcn) ZERO-EXPR)))





;; Helpers that should be moved elsewhere
(: format-uid! : String Uid -> Uid)
(define (format-uid! fmt x)
  (next-uid! (format fmt (Uid-prefix x))))

;; Typed Racket Doesn't come with mutable set support
(define-type (MSet T) (HashTable T True))

(: mutable-set : (All (T) -> (MSet T)))
(define (mutable-set) (make-hash '()))

(: set->list (All (T) (MSet T) -> (Listof T)))
(define (set->list st) (hash-keys st))

(: set-add! (All (T) (MSet T) T -> Void))
(define (set-add! st e) (hash-set! st e #t))

(: set-union! (All (T) (MSet T) (MSet T) -> Void))
(define (set-union! st0 st1)
  (for ([k (in-hash-keys st1)])
    (set-add! st0 k)))


