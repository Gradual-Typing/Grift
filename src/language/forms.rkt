#lang racket/base

(require
 "../unique-identifiers.rkt"
 "../errors.rkt"
 "../logging.rkt"
 (for-syntax racket/base racket/syntax)
 racket/contract/base
 racket/match
 racket/set)

(module+ test
  (require rackunit))

(provide
 (except-out (all-defined-out) Scope-open)
 (all-from-out "../unique-identifiers.rkt"))

#|
All language "Forms" are just polymorphic racket structs.
These are used to define valid ASTs which are passed between
passes of the compiler. There are many forms because we try
to switch forms whenever there is any change in the meaning
of the form.

The macro generates a function for each name in the list plus
an predicate to identify values of this struct.
So (name field1 field2) would create the following definitions.
name? - predicate
name  - constructor
name-field1 - accessor for field1
name-field2 - accessor for field2
And a type constructor "name" expecting the types of field1 and field2 
|#


(define-syntax (define-forms stx)
  (syntax-case stx ()
    [(_ super (name* field** ...) ...)
     (begin
       (define (f name) (format-id stx "~a/c" name))
       (with-syntax ([(name/c* ...) (map f (syntax->list #'(name* ...)))])
         #'(begin
             (struct name* super (field** ...) #:transparent)
             ...
             (define (name/c* field** ...)
               (struct/c name* field** ...))
             ...)))]))

;; These struct types describe how AST are mapped
;; and form a unsafe description of how to map an AST
(struct form () #:transparent)
;; Leaf Nodes Contain No other AST Nodes only data
;; maping them is always the identity.
(struct form:leaf form () #:transparent)
;; Simple branchest just apply the function being maped to each sub-field
;; if the subfield contains a proper list then it is maped with map
;; any non-form subfields are left alone.
(struct form:simple-branch form () #:transparent)
;; Custom-branches have custom defined mapping behavior in form-map.rkt
(struct form:custom-branch form () #:transparent)

(define-forms form:leaf
  ;; Top Level expression marker
  (Expression e)
  ;; Variable node
  (Var id)
  ;; Global is for referencing global variables by their names as strings
  (Global id)
  ;; various imediates markers
  (Quote literal)    ;; immediate data in general
  ;; Node that references a piece of code identified by the UID value
  (Code-Label value)
  (Tag bits)         ;; an tag for an imediate value
  (Type type)        ;; an atomic type
  )

(define-forms form:simple-branch
  ;; Conditionals
  (If test then else)
  ;; Top level wrapper for combining compiler
  ;; state, meta-information, and the program AST
  (Prog annotation expression)
  ;; Top level define marker
  (Define rec? id type expression)
  ;; Annotations that may be added to other AST nodes
  (Ann value data)
  (Ann2 value data) ;; This is a hack to work around a bug
  ;; Function abstraction
  (Lambda formals body)
  ;; Function application 
  (App operator operands)
  ;; Type ascription
  (Ascribe expression type label)
  ;; Primitive operators application 
  (Op operator operands)
  ;; recursive binding
  (Letrec bindings body)
  ;; non recursive binding
  (Let bindings body)
  ;; sequence operator
  (Begin effects value)
  ;; Prim Multi-way branching
  (Switch expr cases default)
  ;; Perform a No Operation
  (No-Op)
  ;; Generic Data-Type
  (Construct type varient args)
  (Access type base field index)
  (Check type prim object args)
  ;; Effect operations
  (Gvector-length vect)
  (Unguarded-Vect-length vect)
  (Mvector-length vect)
  ;; Monotonic effects
  (MboxS value) ;; source level mbox has no type annotation
  (Mbox value type)
  (Munbox box)
  (MunboxT box type)
  (Mbox-set! box value)
  (Mbox-set!T box value type)
  (Mbox-val-ref expression)
  (Mbox-val-set! expression1 expression2)
  (Mbox-rtti-ref expr)
  (Mbox-rtti-set! expression1 expression2)
  (Make-GLB-Two-Fn-Types expression1 expression2 expression3) ;; create meeted function type in runtime
  (Make-GLB-Two-Tuple-Types expression1 expression2 expression3)
  ;; the underlying value can be accessed by the location encoded in the type
  ;; What is the meaning of the above comment? - Andre (TODO)
  (MBoxCastedRef addr type)
  (MBoxCastedSet! addr v type)
  (MvectorS value constructor)
  (Mvector value constructor type)
  (Mvector-set! vector index value)
  (Mvector-set!T vector index value type)
  (Mvector-ref vector index)
  (Mvector-refT vector index type)
  (Mvector-val-ref vector index guarded?)
  (Mvector-val-set! vector index value guarded?)
  (Mvector-rtti-ref vector)
  (Mvector-rtti-set! expression1 expression2)
  (MVectCastedRef addr index type)
  (MVectCastedSet! addr index value type)
  ;; Guarded effects
  (Gbox value)
  (Gunbox box)
  (Gbox-set! box value)
  (Gvector len init-val)
  (Gvector-set! vector offset value)
  (Gvector-ref vector offset)
  ;;
  (Create-tuple values)
  (Copy-Tuple n v)
  (Tuple-proj tuple index)
  (Coerce-Tuple cast value coercion top-level?)
  (Cast-Tuple cast value t1 t2 lbl top-level?)
  ;; Effectfull expressions
  ;; typed bindings annotations
  (Fml identifier type)
  (Bnd identifier type expression)
  ;; Different casts
  (Cast expression instruction)
  (Fn-Caster expression)
  ;;Type Operations
  (Type-Dyn-Huh exp)
  (Type-Tag expression)
  (Type-Fn index return-type argument-types)
  (Type-GRef type)
  (Type-GVect type)
  (Type-MRef type)
  (Type-MVect type)
  (Type-Fn-arity expression)
  (Type-Fn-arg expression index)
  (Type-Fn-return expression)
  (Type-Fn-Huh type)
  (Type-GVect-Huh type)
  (Type-GRef-Huh type)
  (Type-GRef-Of expression)
  (Type-GVect-Of expression)
  (Type-MRef-Huh expression)
  (Type-MRef-Of expression)
  (Type-MVect-Huh expression)
  (Type-MVect-Of expression)
  (Type-Tuple-Huh type)
  (Type-Tuple-num type)
  (Type-Tuple-item type index)
  (Type-Mu-Huh type)
  (Type-Mu-Body type)
  (Let-Static* mu-type-bindings
               type-bindings
               mu-crcn-bindings
               crcn-bindings
               body)
  (Static-Id id)
  (Static* bindings body)
  (LetP bindings body)
  (LetC bindings body);; Can create cyclic immutable data
  (Procedure this params code caster bound-vars body)
  ;; The dynamic type and it's operations
  (Dyn-Object value type)
  (Dyn-Value dyn)
  (Dyn-Type  dyn)
  (Dyn-Immediate-Value dyn)
  (Dyn-Immediate-Tag=Huh dyn type)
  (Dyn-Immediate-Object value type)
  (Dyn-Box-Object value type)
  (Dyn-Box-Value dyn)
  (Dyn-Box-Type dyn)
  ;; represents a set of moves to initialize variables before
  (Code variables body)
  (Dyn-Fn-App expr expr* type* label)
  (Dyn-GRef-Ref expr label)
  (Dyn-GRef-Set! expr1 expr2 type label)
  (Dyn-GVector-Ref expr index label)
  (Dyn-GVector-Set! expr1 index expr2 type label)
  (Dyn-GVector-Len vec label)
  (Dyn-MVector-Set! e1 i e2 e2-ty label)
  (Dyn-MVector-Ref e i label)
  (Dyn-MRef-Set! e1 e2 e2-ty label)
  (Dyn-MRef-Ref e label)
  (Dyn-Tuple-Proj tuple index label)
  ;; Observational Operations
  (Blame expression)
  (Observe expression type)
  (Error expression)
  ;; Lambda subforms
  (Castable caster body)
  (Bound closure variables body)
  (Free caster variables body)
  (GlobDecs vars body) ;; declaration of global variables
  ;; Static Global Binding
  (Labels bindings body)
  (App-Code rand rators)
  (App-Fn rand rators)
  (App/Fn-Proxy-Huh rand rators)
  ;; cast:    Uid of apply-coercion
  ;; compose: Uid of compose-coercions
  ;; (cast and compose are necessary in cast-apply-cast in convert-closures)
  ;; rand for operand; rators for operators.
  (App-Fn-or-Proxy cast compose rand rators)
  ;; Benchmarking tools language forms
  ;; low cost repetition
  (Repeat var start end acc init body)
  (While bool-expression expression)
  (Break-Repeat)
  ;; TODO figue out an appropriate comment about all forms here
  (Halt)
  (Success)
  (Assign lhs rhs)
  ;; Declares Local Variables
  (Locals names space body)
  (Return value)
  (Relop primitive expressions)
  ;; Uil Memory Ops
  (Malloc expression)
  ;; Uil IO Primitives todo add C Calls and replace these
  (Printf format expressions)
  (Print expression)
  (BinOp primitive expression1 expression2)
  ;; Guarded references IL Representation
  (Unguarded-Box init)
  (Unguarded-Box-On-Stack init)
  (Unguarded-Box-Ref box)
  (Unguarded-Box-Set! box value)
  (Unguarded-Vect size value)
  (Unguarded-Vect-Ref vect index)
  (Unguarded-Vect-Set! vect index value)
  (Guarded-Proxy expression representation)
  (Guarded-Proxy-Ref guarded)
  (Guarded-Proxy-Source guarded)
  (Guarded-Proxy-Target guarded)
  (Guarded-Proxy-Blames guarded)
  (Guarded-Proxy-Coercion guarded)
  (Guarded-Proxy-Huh expression)
  )

(define ((map-switch-case f) c)
  (cons (car c) (f (cdr c))))

(define (map-switch-case* f c*)
  (map (map-switch-case f) c*))

(define NO-OP (No-Op))

(define blame-label? string?)

#| Types throughout the languages |#

;; Grift types
(struct type ()
  #:transparent)
(struct structural-type type ()
  #:transparent)
(struct base-type structural-type () #:transparent)
(struct logical-type type ()
  #:transparent)
(struct Unit base-type ()
  #:transparent)
(struct String base-type ()
  #:transparent)
(struct Void base-type ()
  #:transparent)
(struct Obj base-type ()
  #:transparent)
(struct RT-Type base-type ()
  #:transparent)
(struct Assoc-Stack base-type ()
  #:transparent)
(struct Bot base-type ()
  #:transparent)
(struct Int base-type ()
  #:transparent)
(struct Float base-type ()
  #:transparent)
(struct Bool base-type ()
  #:transparent)
(struct Character base-type ()
  #:transparent)
(struct Dyn structural-type ()
  #:transparent)
(struct Fn structural-type (arity fmls ret) #:transparent)
(struct MRef structural-type (arg)
  #:transparent)
(struct MVect structural-type (arg)
  #:transparent)
(struct GRef structural-type (arg)
  #:transparent)
(struct GVect structural-type (arg)
  #:transparent)
(struct STuple structural-type (num items)
  #:transparent)
;; Not a type but abstraction enforcement
(struct Scope (open) #:transparent)
(struct Mu logical-type (body) #:transparent)
(struct TVar (index) #:transparent)
(struct FVar (name) #:transparent)

;; Grift-Type (Grift-Type -> Grift-Type) -> Grift-Type
(define (grift-type-map t f)
  (match t
    [(Fn n a* r)
     (Fn n (map f a*) (f r))]
    [(STuple n a*)
     (STuple n (map f a*))]
    [(GVect a)
     (GVect (f a))]
    [(GRef a)
     (GRef (f a))]
    [(MVect a)
     (MVect (f a))]
    [(MRef a)
     (MRef (f a))]
    [(Mu (Scope t))
     (Mu (Scope (f t)))]
    [t t]))

;; (Scope Grift-Type) Grift-Type -> (Values Grift-Type Boolean)
(define (grift-type-instantiate/used? s t1)
  (define used? (box #f))
  ;; Natural -> (Grift-Type -> Grift-Type)
  (define (subst i)
    ;; Grift-Type -> Grift-Type
    (define (rec t0)
      (define who 'grift-type-instantiate:subst:rec)
      (debug off who s t1 i t0)
      (cond
        [(and (TVar? t0) (= i (TVar-index t0)))
         (set-box! used? #t)
         t1]
        [(Mu? t0) (Mu (Scope ((subst (+ 1 i)) (Scope-open (Mu-body t0)))))]
        [else (grift-type-map t0 rec)]))
    rec)
  (define t2 ((subst 0) (Scope-open s)))
  (values t2 (unbox used?)))

;; (Scope Grift-Type) Grift-Type -> Grift-Type
(define (grift-type-instantiate s t)
  (define-values (t0 _) (grift-type-instantiate/used? s t))
  t0)

(module+ test
  (define u1 (Uid "foo" 0))
  (define u2 (Uid "bar" 1))

  (define ln1 (Mu (Scope (STuple 2 (list (FVar u1) (TVar 0))))))

  (check-equal? (grift-type-instantiate (Mu-body ln1) (FVar u2))
                (STuple 2 (list (FVar u1) (FVar u2))))
  (check-equal? (grift-type-instantiate (Mu-body ln1) ln1)
                (STuple 2 (list (FVar u1)
                                (Mu (Scope (STuple 2 (list (FVar u1) (TVar 0))))))))
  (check-equal? (grift-type-abstract u1 ln1)
                (Scope (Mu (Scope (STuple 2 (list (TVar 1) (TVar 0))))))))

;; (Mu (Scope Grift-Type)) -> Grift-Type
(define (unfold-mu t)
  (match-define (Mu s) t)
  (grift-type-instantiate s t))

;; Close over a free variable in a type
;; Uid, Grift-Type -> (Scope Grift-Type), Boolean
(define (grift-type-abstract/used? x t)
  (define used? (box #f))
  (define (subst i)
    (define (rec t)
      (cond
        [(and (FVar? t) (equal? x (FVar-name t)))
         (set-box! used? #t)
         (TVar i)]
        [(Mu? t) (Mu (Scope ((subst (+ 1 i)) (Scope-open (Mu-body t)))))]
        [else (grift-type-map t rec)]))
    rec)
  (define s (Scope ((subst 0) t)))
  (values s (unbox used?)))

;; Uid, Grift-Type -> (Scope Grift-Type)
(define (grift-type-abstract x t)
  (define-values (s _) (grift-type-abstract/used? x t))
  s)


;;Constants for the types
(define UNIT (Unit))
(define UNIT-TYPE UNIT)
(define UNIT-EXPR (Type UNIT-TYPE))

(define STRING (String))

(define VOID (Void))
(define OBJ (Obj))
(define RT-TYPE (RT-Type))
(define ASSOC-STACK (Assoc-Stack))
(define BOTTOM (Bot))

(define INT (Int))
(define INT-TYPE INT)
(define INT-EXPR (Type INT-TYPE))

(define FLOAT (Float))
(define FLOAT-TYPE FLOAT)
(define FLOAT-EXPR (Type FLOAT-TYPE))

(define BOOL (Bool))
(define BOOL-TYPE BOOL)
(define BOOL-EXPR (Type BOOL-TYPE))

(define CHAR (Character))
(define CHAR-TYPE CHAR)
(define CHAR-EXPR (Type CHAR-TYPE))

(define DYN-TYPE (Dyn))
(define DYN-EXPR (Type DYN-TYPE))



(define PBOX-DYN-TYPE (GRef DYN-TYPE))
(define PBOX-DYN-EXPR (Type PBOX-DYN-TYPE))

(define PVEC-DYN-TYPE (GVect DYN-TYPE))
(define PVEC-DYN-EXPR (Type PVEC-DYN-TYPE))

(define MBOX-DYN-TYPE (MRef DYN-TYPE))
(define MBOX-DYN-EXPR (Type MBOX-DYN-TYPE))

(define MVEC-DYN-TYPE (MVect DYN-TYPE))
(define MVEC-DYN-EXPR (Type MVEC-DYN-TYPE))

(define FN-DYN-DYN-TYPE (Fn 1 (list DYN-TYPE) DYN-TYPE))
(define FN-DYN-DYN-EXPR (Type FN-DYN-DYN-TYPE))

(define TUPLE-DYN-TYPE (STuple 1 (list DYN-TYPE)))
(define TUPLE-DYN-EXPR (Type TUPLE-DYN-TYPE))

;; define type id = t and id* = (c* t) ...
(define-syntax-rule (define-type+ id ([id* c*] ...) t)
  (begin (define-type id t)
	 (define-type id* (c* id)) ...))

;; Are two types consistent at the top of the types?
;; (Any Any -> Boolean)
(define (shallow-consistent? t g)
  ;; Typed racket made me do it
  ;; This uber modular structure speeds up type checking
  (define (both-int? t g)  (and (Int? t)  (Int? g)))
  (define (both-bool? t g) (and (Bool? t) (Bool? g)))
  (define (both-fn? t g)  
    (and (Fn? t)
         (Fn? g)
         (equal? (Fn-arity t)
                 (Fn-arity g))))
  (define (both-tuple? t g)  
    (and (STuple? t)
         (STuple? g)
         (equal? (STuple-num t)
                 (STuple-num g))))
  (define (ref-shallow-consistent? t g)
    (or (and (GRef? t) (GRef? g))
        (and (GVect? t) (GVect? g))
        (and (MRef? t) (MRef? t))
        (and (MVect? t) (MVect? t))))
  (or (Dyn? t)
      (Dyn? g)
      (both-int? t g)
      (both-bool? t g)
      (both-fn? t g)
      (both-tuple? t g)
      (ref-shallow-consistent? t g)))


;; Is the type t devoid of dyn?
;; Grift-Type -> Boolean
(define (completely-static-type? t)
  ;; Typed Racket made me do it
  ;; This uber modular structure speeds up type checking
  (define (fn-completely-static? t ms)
    (and (Fn? t)
         (andmap (cst/ms? ms) (Fn-fmls t))
         (cst? (Fn-ret t) ms)))
  ;; Is it a completely static tuple type?
  (define (tuple-completely-static? t ms)
    (and (STuple? t) (andmap (cst/ms? ms) (STuple-items t))))
  ;; Is it a completely static reference type?
  (define (ref-completely-static? t ms)
    (or (and (GRef? t) (cst? (GRef-arg t) ms))
        (and (MRef? t) (cst? (MRef-arg t) ms))
        (and (GVect? t) (cst? (GVect-arg t) ms))
        (and (MVect? t) (cst? (MVect-arg t) ms))))
  ;; Is it a completely static recursive type?
  (define (mu-completely-static? t ms)
    (and (Mu? t)
         (or (set-member? ms t)
             (cst? (grift-type-instantiate (Mu-body t) t) (set-add ms t)))))
  (define ((cst/ms? ms) t) (cst? t ms))
  (define (cst? t ms)
    (or (Int? t)
        (Bool? t)
        (Character? t)
        (Float? t)
        (Unit? t)
        (fn-completely-static? t ms)
        (tuple-completely-static? t ms)
        (ref-completely-static? t ms)
        (mu-completely-static? t ms)))
  (cst? t (set)))


#|-----------------------------------------------------------------------------+
| Types shared by the Grift language family
+-----------------------------------------------------------------------------|#

#| Literals of the grift languages
Only Integers and Booleans in the grift language are first
class literal constants
|#

(define (grift-literal? x)
  (or (exact-integer? x)
      (char? x)
      (boolean? x)
      (null? x)
      (real? x)))

(define (grift-literal->base-type x)
  (cond
    [(char? x) CHAR-TYPE]
    [(boolean? x) BOOL-TYPE]
    [(exact-integer? x) INT-TYPE]
    [(inexact-real? x) FLOAT-TYPE]
    [(null? x)    UNIT-TYPE]
    [else (error 'language/grift-literal->type "~a" x)]))

;; Grift Type =
;; (U Dyn Base-Type Grift-Fn-Type Grift-Ref-Type
;;    Grift-Tuple-Type Grift-Mu-Type FVar TVar)

;; type known at runtime only for monotonic references, the uid is for
;; the entire reference cell, you have to access the second component
;; of the cell to get the type.


;; Grift-Type, Grift-Type -> Boolean
(define (consistent? t g)
  (define (set-memp st x)
    (and (set-member? st x) st))
  (define (andfold f a ls)
    (cond
      [(not a) #f]
      [(null? ls) a]
      [else (andfold f (f (car ls) a) (cdr ls))]))  
  (define (consist-pair? p ms)
    (consist? (car p) (cdr p) ms))
  (define (consist?-fold ms l1 l2)
    (andfold consist-pair? ms (map cons l1 l2)))
  (define (consist? t g ms)
    (match* (t g)
      [(_ _) #:when (or (Dyn? t) (Dyn? g) (equal? t g))
             ms]
      [((Mu s) _)
       (define p (cons t g))
       (or (set-memp ms p)
           (consist? (grift-type-instantiate s t) g (set-add ms p)))]
      [(_ (Mu s))
       (define p (cons t g))
       (or (set-memp ms p)
           (consist? t (grift-type-instantiate s g) (set-add ms p)))]
      [((Fn n ta tr) (Fn n ga gr))
       (consist?-fold (consist? tr gr ms) ta ga)]
      [((STuple n ta) (STuple n ga))
       (consist?-fold ms ta ga)]
      [((GRef t0) (GRef g0)) (consist? t0 g0 ms)]
      [((GVect t0) (GVect g0)) (consist? t0 g0 ms)]
      [((MRef t0) (MRef g0)) (consist? t0 g0 ms)]
      [((MVect t0) (MVect g0)) (consist? t0 g0 ms)]
      [(_ _) #f]))
  ;; map truthy values to true
  (not (not (consist? t g (set)))))

(module+ test
  (define mu-dyn
    (Mu (Scope (STuple 2 (list DYN-TYPE (TVar 0))))))
  (define mu-int
    (Mu (Scope (STuple 2 (list INT-TYPE (TVar 0))))))
  (test-equal? "Mu type" (consistent? mu-dyn mu-int) #t))

(struct Bottom (t1 t2) #:transparent)

;; (define-type Grift-Type?! (U Bottom Grift-Type))

;; (Bottom -> Grift-Type) -> Grift-Type, Grift-Type -> Grift-Type
(define ((up exit) t g)
  (match* (t g)
    [((Dyn) g)     g]
    [(t     (Dyn)) t]
    [(t     t)     t]
    [(t     g)     (exit (Bottom t g))]))

;; (Bottom -> Grift-Type) -> Grift-Type Grift-Type -> Grift-Type
(define ((down exit) t g)
  (match* (t g)
    [((and t (Dyn)) _) t]
    [(_ (and g (Dyn))) g]
    [(t t) t]
    [(t g) (exit (Bottom t g))]))


;; (Grift-Type, Grift-Type -> Grift-Type) -> Grift-Type, Grift-Type -> Grift-Type
(define (move u/d/fail)
  (define (mv/e e)
    (define (mv t g)
      (match* (t g) 
        [((Fn ta ta* tr) (Fn ga ga* gr)) #:when (= ta ga)
         (Fn ta (map mv ta* ga*) (mv tr gr))]
        [((STuple ta t*) (STuple ga g*)) #:when (= ta ga)
         (STuple ta (map mv t* g*))]
         [((GRef t) (GRef g))
          (GRef (mv t g))]
         [((GVect t) (GVect g))
          (GVect (mv t g))]
         [((MRef t) (MRef g))
          (MRef (mv t g))]
         [(t g) #:when (or (Mu? t) (Mu? g))
          (define p (cons t g))
          (define x? (hash-ref e p #f))
          (cond
            [x? x?]
            [else
             (define (move-mu t mv)
               (match-define (Mu s) t)
               (define u  (next-uid! 'r))
               (define t0 (grift-type-instantiate s t))
               (define t1  (mv (hash-set e p (FVar u)) t0))
               (define-values (s0 used?) (grift-type-abstract/used? u t1))
               (cond
                 [(not used?) t1]
                 [else (Mu s0)]))
             (match* (t g) 
               [((Mu _) g)
                (define (mv/other e t0) ((mv/e e) t0 g))
                (move-mu t mv/other)]
               [(t (Mu _))
                (define (mv/other e g0) ((mv/e e) t g0))
                (move-mu g mv/other)])])]
         [(t g) (u/d/fail t g)]))
    mv)
  (mv/e (hash)))

;; ((Bottom -> Grift-Type) -> Grift-Type, Grift-Type -> Grift-Type) ->
;;    Grift-Type, Grift-Type -> Grift-Type?!
(define ((move?! u/d) t g)
  (call/ec
   (lambda (return)
     ((move (u/d return)) t g))))

;; ((Bottom -> Grift-Type) -> Grift-Type Grift-Type -> Grift-Type) ->
;;       Grift-Type* -> Grift-Type?!
(define ((move+ u/d) t+)
  (call/ec
   (lambda (return)
     (define m (move (u/d return)))
     (let move+ ([t+ t+])
       (match t+
         [(list) (error 'move+ "must be non empty list")]
         [(list t) t]
         [(cons t t+) (m t (move+ t+))])))))

;; Grift-Type, Grift-Type -> Grift-Type?!
(define join (move?! up))

;; Grift-Type* -> Grift-Type?!
(define join+ (move+ up))

;; Grift-Type, Grift-Type -> Grift-Type?!
(define meet (move?! down))
;; Grift-Type* -> Grift-Type?!
(define meet+ (move+ down))

;; returns #t if `t1` is less precise than `t2`.
;; if t1 and t2 are unrelated it returns #f.
;; Grift-Type, Grift-Type -> Boolean
(define (le-precise? t1 t2)
  (let loop ([t1 t1]
             [t2 t2]
             ;; seen : (Setof (Pairof Grift-Type Grift-Type))
             [seen (set)])
    (let rec/s ([t1 t1] [t2 t2])
      (match* (t1 t2)
        [(t t) #t]
        [((Dyn) t) #t]
        [((Fn n t1* t1) (Fn n t2* t2))
         (and (rec/s t1 t2)
              (for/and ([t1 t1*] [t2 t2*])
                (rec/s t1 t2)))]
        [((STuple n t1*) (STuple n t2*))
         (for/and ([t1 t1*] [t2 t2*])
           (rec/s t1 t2))]
        [((GRef t1)(GRef t2)) (rec/s t1 t2)]
        [((GVect t1) (GVect t2)) (rec/s t1 t2)]
        [((MRef t1) (MRef t2)) (rec/s t1 t2)]
        [((MVect t1) (MVect t2)) (rec/s t1 t2)]
        [(t1 t2)
         #:when (or (Mu? t1) (Mu? t2))
         (define p (cons t1 t2))
         (match* (t1 t2)
           [(_ _) #:when (set-member? seen p) #t]
           [((Mu s) t2)
            (loop (grift-type-instantiate s t1) t2 (set-add seen p))]
           [(t1 (Mu s))
            (loop t1 (grift-type-instantiate s t2) (set-add seen p))])]
        [(other wise) #f]))))

(define-forms form:simple-branch
  ;; The following forms are used to parameterize
  ;; the cast forms for 1 versus 3 extra arguments
  (Coercion coercion)
  (Twosome type1 type2 lbl)
  ;; This might be able to be gotten rid of now
  (Compose-Coercions fst snd)
  (Make-Coercion t1 t2)
  ;; Id Coercion reflection
  (Id-Coercion-Huh expression)
  ;; Function Coercion
  ;; "Proxy a function call with args coercions and return coercion"
  (Fn-Coercion-Arity c)
  (Fn-Coercion args return)
  (Fn-Coercion-Arg coercion index)
  (Fn-Coercion-Return coercion)
  (Id-Fn-Coercion arity)
  (Fn-Coercion-Return-Set! fn return)
  (Fn-Coercion-Arg-Set! fn index arg)
  ;; Tuples
  (CTuple num items)
  (Tuple-Coercion items)
  (Tuple-Coercion-Huh c)
  (Tuple-Coercion-Num c)
  (Tuple-Coercion-Item c indx)
  (Id-Tuple-Coercion num)
  (Tuple-Coercion-Item-Set! tuple index item)
  ;; We should be able to get rid of this now
  (Make-Tuple-Coercion make-uid t1 t2 lbl)
  ;; Mediating Coercions cast two non-dynamic-types
  (Mediating-Coercion-Huh c)
  ;; Proxied Reference Coercion
  ;; "Proxy a Proxied Reference's Reads and writes"
  ;; the choice indicates whether it is a ref or a vector coercion
  (Ref read write choice)  
  (Ref-Coercion read write flag)
  (Ref-Coercion-Read expression)
  (Ref-Coercion-Write ref)
  ;; checks if the ref coercion is for a ref or a vector value
  (Ref-Coercion-Ref-Huh crcn) 
  (MonoRef type) ;; Monotonic Reference Coercion
  (MonoVect type)
  (MRef-Coercion type)
  (MRef-Coercion-Type expression)
  (MVect-Coercion expression)
  (MVect-Coercion-Type expression)
  (Fn-Proxy arity coercion closure)
  (Fn-Proxy-Coercion expression)
  (Fn-Proxy-Closure expression)
  (Fn-Proxy-Huh expression)
  (Hybrid-Proxy apply closure coercion)
  (Hybrid-Proxy-Huh clos)
  (Hybrid-Proxy-Closure hybrid)
  (Hybrid-Proxy-Coercion hybrid)
  ;; Sequence Coercions
  ;; "Perform the first Coercion and then Another"
  (Sequence fst snd)
  (Sequence-Coercion-Huh crcn)
  (Sequence-Coercion fst snd)
  (Sequence-Coercion-Fst seq)
  (Sequence-Coercion-Snd seq)
  ;; Projection Coercion
  ;; "Project from dynamic at type blaming label if it fails"
  (Project type label)
  (Project-Coercion type label)
  (Project-Coercion-Huh crcn)
  (Project-Coercion-Type prj)
  (Project-Coercion-Label prj)
  ;; Injection Coercion
  ;; "Inject into the dynamic type at type"
  (Inject type)
  (Inject-Coercion type)
  (Inject-Coercion-Huh crnc)
  (Inject-Coercion-Type inj)
  ;; Fail Coercion
  ;; "Fail with the label"
  (Failed label)
  (Failed-Coercion label)
  (Failed-Coercion-Huh crcn)
  (Failed-Coercion-Label fld)
  (Ref-Coercion-Huh crcn)
  (MRef-Coercion-Huh crcn)
  (MVect-Coercion-Huh crcn)
  (Fn-Coercion-Huh crcn)
  (Make-Fn-Coercion make-uid t1 t2 lbl)
  (Mu-Coercion-Huh crcn)
  (Mu-Coercion-Body mu)
  (Mu-Coercion-Body-Set! mu body)
  (HC project? t1 label inject? t2 med)
  (Quote-HCoercion coercion)
  (HC-Project-Huh hc)
  (HC-Inject-Huh hc)
  (HC-Identity-Huh hc)
  (HC-Med hc)
  (HC-Label hc)
  (HC-T1 hc)
  (HC-T2 hc))

(define-forms form:leaf
  ;; Identity Cast
  ;; "Calculated No Op Cast"
  (Identity)
  (Make-Mu-Coercion)
  (Quote-Coercion const)
  ;; FIXME Does this need to exist?
  ;; Can't we just use the (Identity) "litereral" everywhere it is needed?
  (Id-Coercion))

(struct CVar form:leaf (uid) #:transparent)
(struct CRec form:simple-branch (uid body) #:transparent)

(define IDENTITY (Identity))
(define ID-EXPR (Quote-Coercion IDENTITY))
(define ZERO-EXPR (Quote 0))

(define (data-literal? x)
  (or (exact-integer? x)
      (inexact-real? x)
      (char? x)
      (string? x)))

(define ((map-bnd f) b)
  (match b
    [(Bnd i t e) (Bnd i t (f e))]
    [(cons i e) (cons i (f e))]))

(module+ test
  (check-true (form:leaf? (Quote 1))))

(struct Stack-Alloc form:leaf (bytes) #:transparent)


(struct Closure form:simple-branch
  (name ; Uid
   well-known? ; Boolean
   code-generation ; (U 'regular 'code-only 'closure-only)
   code-label ; Uid
   self ; Uid
   caster ; Uid
   free-vars ; (Listof (U Uid Expr))
   parameters ; Uid*
   code ; Expr, but it is bogus if code-generation == 'code-only
   )
  #:transparent)
(define closure-code-generation/c (symbols 'regular 'code-only 'closure-only))
(define (Closure/c E)
  (struct/c Closure
            Uid?
            boolean?
            closure-code-generation/c
            Uid?
            Uid?
            Uid?
            (or/c (listof Uid?) (listof E))
            (listof Uid?)
            E))

(struct Closure-Code form:simple-branch
  (arg)
  #:transparent)

(struct Closure-Caster form:simple-branch
  (arg)
  #:transparent)

(struct Closure-Ref form:leaf
  (arg key)
  #:transparent)

(struct Closure-App form:simple-branch
  (code closure arguments)
  #:transparent)

(struct Static-Let* form:simple-branch
  (bindings program)
  #:transparent)

(struct Closure-Proxy form:leaf (closure)
  #:transparent)

(struct Let-Closures form:simple-branch (bindings body)
  #:transparent)

(define do-not-suspend-monotonic-heap-casts (Quote #f))
(define suspend-monotonic-heap-casts (Quote #t))
