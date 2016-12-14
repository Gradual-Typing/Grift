#lang typed/racket/base

(require "../helpers.rkt"
         "../unique-identifiers.rkt"
         "../errors.rkt"
         (for-syntax racket/base)
         racket/match)

(provide (all-defined-out)
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
    [(_ (name* field** ...) ...)
     (let ([f** (syntax->list #'((field** ...) ...))])
       (with-syntax ([((type** ...) ...) (map generate-temporaries f**)])
         #'(begin
             (struct (type** ...) name* ([field** : type**] ...) #:transparent)
             ...)))]))

(define-forms
  ;; Top level wrapper for combining compiler
  ;; state, meta-information, and the program AST
  (Prog annotation expression)
  (Define rec? id type expression)
  ;; Top Level Forms
  (Expression e)
  ;; Annotations that may be added to other AST nodes
  (Ann value data)
  ;; Function abstraction
  (Lambda formals body)
  ;; Function application 
  (App operator operands)
  ;; Variable node
  (Var id)
  ;; Conditionals
  (If test then else)
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
  ;; Effect operations
  ;; Monotonic effects
  (MboxS value) ;; source level mbox has no type annotation
  (Mbox value type)
  (Munbox box)
  (MunboxT box type)
  (Mbox-set! box value)
  (Mbox-set!T box value type)
  (Mbox-val-ref expression)
  (Mbox-val-set! expression1 expression2)
  (Mbox-rtti-ref address)
  (Mbox-rtti-set! address expression)
  (Make-Fn-Type expression1 expression2 expression3) ;; create meeted function type in runtime
  (Make-Tuple-Type expression1 expression2 expression3)
  ;; the underlying value can be accessed by the location encoded in the type
  (MBoxCastedRef addr type)
  (MBoxCastedSet! addr v type)
  (MvectorS value constructor)
  (Mvector value constructor type)
  (Mvector-size value)
  (Mvector-set! vector index value)
  (Mvector-set!T vector index value type)
  (Mvector-ref vector index)
  (Mvector-refT vector index type)
  (Mvector-val-ref vector index)
  (Mvector-val-set! vector index value)
  (Mvector-rtti-ref vector)
  (Mvector-rtti-set! address expression)
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
  (Coerce-Tuple cast value coercion)
  (Coerce-Tuple-In-Place cast value coercion address)
  (Cast-Tuple cast value t1 t2 lbl)
  (Cast-Tuple-In-Place cast value t1 t2 lbl address)
  ;; various imediates markers
  (Quote literal)    ;; immediate data in general
  ;; Node that references a piece of code identified by the UID value
  (Code-Label value)
  (Tag bits)         ;; an tag for an imediate value
  (Type type)        ;; an atomic type
  ;; Effectfull expressions
  ;; typed bindings annotations
  (Fml identifier type)
  (Bnd identifier type expression)
  ;; Different casts
  (Cast expression instruction)
  ;; TODO Interpreted-Cast
  (Interpreted-Cast expression instruction)
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
  ;; closure Representation
  (App-Closure code data exprs)
  (Closure-Data code caster variables)
  (Closure-code var)
  (Closure-ref this var)
  (Closure-caster this)
  (Let-Static* type-bindings crcn-bindings body)
  (Static-Id id)
  (LetP bindings body)
  (LetC bindings body);; Can create cyclic immutable data
  (Procedure this params code caster bound-vars body)
  ;; represents a set of moves to initialize variables before
  (Code variables body)
  ;; Dyn operations
  (Dyn-tag expression)
  (Dyn-immediate expression)
  (Dyn-type expression)
  (Dyn-value expression)
  (Dyn-make expression type)
  (Dyn-Fn-App expr expr* type* label)
  (Dyn-GRef-Ref expr label)
  (Dyn-GRef-Set! expr1 expr2 type label)
  (Dyn-GVector-Ref expr index label)
  (Dyn-GVector-Set! expr1 index expr2 type label)
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
  (App-Fn-or-Proxy cast rand rators)
  ;; Benchmarking tools language forms
  ;; low cost repetition
  (Repeat var start end acc init body)
  (Break-Repeat)
  ;; TODO figue out an appropriate comment about all forms here
  (Halt)
  (Success)
  (Assign lhs rhs)
  ;; Declares Local Variables
  (Locals names body)
  (Return value)
  (Relop primitive expression1 expression2)
  ;; Uil Memory Ops
  (Malloc expression)
  ;; Uil IO Primitives todo add C Calls and replace these
  (Printf format expressions)
  (Print expression)
  (BinOp primitive expression1 expression2)
  ;; Guarded references IL Representation
  (Unguarded-Box init)
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
  #|
  (GRep-proxied? expression)
  (UGbox expression)
  (UGbox-set! expression1 expression2)
  (UGbox-ref expression)
  (UGvect expression1 expression2)
  (UGvect-set! expression1 expression2 expression3)
  (UGvect-ref expression1 expression2)
  (Gproxy for-exp from-exp to-exp blames-exp)
  (Gproxy-for expression)
  (Gproxy-from expression)
  (Gproxy-to expression)
  (Gproxy-blames expression)
  |#
  )

(define-type (Switch-Case  e) (Pair (Listof Integer) e))
(define-type (Switch-Case* e) (Listof (Switch-Case e)))

(: map-switch-case :
   (All (A B) (A -> B) -> ((Switch-Case A) -> (Switch-Case B))))
(define ((map-switch-case f) c)
  (cons (car c) (f (cdr c))))

(: map-switch-case* :
   (All (A B) (A -> B) (Switch-Case* A) -> (Switch-Case* B)))
(define (map-switch-case* f c*)
  (map (map-switch-case f) c*))

(define NO-OP (No-Op))



(define-type Blame-Label String)
(define blame-label? string?)


#| Types throughout the languages |#

;; Schml types
(define-forms
  (Unit)
  (Int)
  (Float)
  (Bool)
  (Character)
  (Dyn)
  (Fn arity fmls ret)
  (MRef  arg)
  (MVect arg)
  (GRef  arg)
  (GVect arg)
  (STuple num items))

;;Constants for the types
(define UNIT-TYPE (Unit))
(define INT-TYPE (Int))
(define FLOAT-TYPE (Float))
(define BOOL-TYPE (Bool))
(define CHAR-TYPE (Character))
(define DYN-TYPE (Dyn))
(define REF-DYN-TYPE (GRef DYN-TYPE))

;; define type id = t and id* = (c* t) ...
(define-syntax-rule (define-type+ id ([id* c*] ...) t)
  (begin (define-type id t)
	 (define-type id* (c* id)) ...))

#|
(define ANY-TYPE (Any-Type))
(define STRING-PTR (String-Ptr))
(define ANY-VALUE (Any-Value))
(define BOTTOM-TYPE (Bottom-Type))
(define VOID-TYPE (Void-Type))
|#

;; Are two types consistent at the top of the types?
(: shallow-consistent? (Any Any -> Boolean))
(define (shallow-consistent? t g)
  ;; Typed racket made me do it
  ;; This uber modular structure speeds up type checking
  (define (both-int? t g) : Boolean (and (Int? t)  (Int? g)))
  (define (both-bool? t g): Boolean (and (Bool? t) (Bool? g)))
  (define (both-fn? t g)  : Boolean
    (and (Fn? t)
         (Fn? g)
         (equal? (Fn-arity t)
                 (Fn-arity g))))
  (define (both-tuple? t g)  : Boolean
    (and (STuple? t)
         (STuple? g)
         (equal? (STuple-num t)
                 (STuple-num g))))
  (define (ref-shallow-consistent? t g) : Boolean
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


(: completely-static-type? (-> Schml-Type Boolean))
;; Is the type t devoid of dyn?
(define (completely-static-type? t)
  ;; Typed Racket made me do it
  ;; This uber modular structure speeds up type checking
  (define (fn-completely-static? [t : Schml-Type]): Boolean
    (and (Fn? t)
         (andmap completely-static-type? (Fn-fmls t))
         (completely-static-type? (Fn-ret t))))
  (define (tuple-completely-static? [t : Schml-Type]): Boolean
    (and (STuple? t)
         (andmap completely-static-type? (STuple-items t))))
  (define (ref-completely-static? [t : Schml-Type])
    (or (and (GRef? t) (completely-static-type? (GRef-arg t)))
        (and (MRef? t) (completely-static-type? (MRef-arg t)))
        (and (GVect? t) (completely-static-type? (GVect-arg t)))
        (and (MVect? t) (completely-static-type? (MVect-arg t)))))
  (or (Int? t)
      (Bool? t)
      (Character? t)
      (Float? t)
      (Unit? t)
      (fn-completely-static? t)
      (tuple-completely-static? t)
      (ref-completely-static? t)))




#|-----------------------------------------------------------------------------+
| Types shared by the Schml language family
+-----------------------------------------------------------------------------|#

#| Literals of the schml languages
Only Integers and Booleans in the schml language are first
class literal constants
|#

(define-type Schml-Literal
  (U Integer Boolean Null Real Char))

#;(: platform-integer? (Any -> Boolean : Integer))
#;
(define (platform-integer? x)
  (fixnum? x))

(: schml-literal? (Any -> Boolean : Schml-Literal))
(define (schml-literal? x)
  (or (exact-integer? x)
      (char? x)
      (boolean? x)
      (null? x)
      (real? x)))

(: schml-literal->base-type (Schml-Literal -> Base-Type))
(define (schml-literal->base-type x)
  (cond
    [(char? x) CHAR-TYPE]
    [(boolean? x) BOOL-TYPE]
    [(exact-integer? x) INT-TYPE]
    [(inexact-real? x) FLOAT-TYPE]
    [(null? x)    UNIT-TYPE]
    [else (error 'language/schml-literal->type "~a" x)]))

;; Types in the schml languages
(define-type  Base-Type (U Int Bool Unit Character Float))

(: base-type? (Any -> Boolean : Base-Type))
(define (base-type? x)
  (or (Int? x)
      (Bool? x)
      (Character? x)
      (Unit? x)
      (Float? x)))

(define-type+ Schml-Type ([Schml-Type* Listof]
			  [Schml-Type? Option])
  (U Dyn
     Base-Type
     Schml-Fn-Type
     Schml-Ref-Type
     Schml-Tuple-Type))

;; type known at runtime only for monotonic references, the uid is for
;; the entire reference cell, you have to access the second component
;; of the cell to get the type.

(define-type Schml-Fn-Type
  (Fn Index Schml-Type* Schml-Type))

(define-type Schml-Tuple-Type
  (STuple Index Schml-Type*))

(define-type Schml-Ref-Type
  (U (GRef  Schml-Type)
     (GVect Schml-Type)
     (MRef  Schml-Type)
     (MVect Schml-Type)))

(define-type Atomic-Type (U Base-Type Dyn))

(: atomic-type? : Any -> Boolean : Atomic-Type)
(define (atomic-type? x)
  (or (Dyn? x) (base-type? x)))

(: schml-type? (Any -> Boolean : Schml-Type))
(define (schml-type? x)
  (or (atomic-type? x)
      (schml-fn? x)
      (schml-ref? x)
      (schml-tuple? x)))

(define-predicate schml-type*? Schml-Type*)
#;(: schml-type*? (Any -> Boolean : Schml-Type*))
#;
(define (schml-type*? x)
  (or (null? x)
      (and (pair? x)
           (schml-type? (car x))
           (schml-type*? (cdr x)))))

(define-predicate schml-fn? Schml-Fn-Type)
(define-predicate schml-tuple? Schml-Tuple-Type)
#;(: schml-fn? (Any -> Boolean : Schml-Fn-Type))
#;(define (schml-fn? x)
    (and (Fn? x)
         (index? (Fn-arity x))
         (schml-type*? (Fn-fmls x))
         (schml-type? (Fn-ret x))))


(define-predicate schml-ref? Schml-Ref-Type)
#;(: schml-ref? (Any -> Boolean : Schml-Ref-Type))

#;
(define (schml-ref? x)
  (or (and (GRef? x)  (schml-type? (GRef-arg x)))
      (and (GVect? x) (schml-type? (GVect-arg x)))
      (and (MRef? x)  (schml-type? (MRef-arg x)))
      (and (MVect? x) (schml-type? (MVect-arg x)))))

(define-type+ Schml-Fml ([Schml-Fml* Listof])
  (Fml Uid Schml-Type))



(define-type ConsistentT (Schml-Type Schml-Type -> Boolean))
(: consistent? ConsistentT)
(define (consistent? t g)
  ;; Typed racket made me structure the code this way.
  (: both-unit? ConsistentT)
  (define (both-unit? t g) (and (Unit? t) (Unit? g)))
  (: both-bool? ConsistentT)
  (define (both-bool? t g) (and (Bool? t) (Bool? g)))
  (: both-int? ConsistentT)
  (define (both-int? t g) (and (Int? t) (Int? g)))
  (: both-char? ConsistentT)
  (define (both-char? t g) (and (Character? t) (Character? g)))
  (: both-float? ConsistentT)
  (define (both-float? t g) (and (Float? t) (Float? g)))
  (: consistent-fns? ConsistentT)
  (define (consistent-fns? t g)
    (and (Fn? t) (Fn? g)
         (= (Fn-arity t) (Fn-arity g))
         (andmap consistent? (Fn-fmls t) (Fn-fmls g))
         (consistent? (Fn-ret t) (Fn-ret g))))
  (: consistent-tuples? ConsistentT)
  (define (consistent-tuples? t g)
    (and (STuple? t) (STuple? g)
         (= (STuple-num t) (STuple-num g))
         (andmap consistent? (STuple-items t) (STuple-items g))))
  (: consistent-grefs? ConsistentT)
  (define (consistent-grefs? t g)
    (and (GRef? t) (GRef? g)
         (consistent? (GRef-arg t) (GRef-arg g))))
  (: consistent-gvects? ConsistentT)
  (define (consistent-gvects? t g)
    (and (GVect? t) (GVect? g)
         (consistent? (GVect-arg t) (GVect-arg g))))
  (: consistent-mrefs? ConsistentT)
  (define (consistent-mrefs? t g)
    (and (MRef? t) (MRef? g)
         (consistent? (MRef-arg t) (MRef-arg g))))
  (: consistent-mvects? ConsistentT)
  (define (consistent-mvects? t g)
    (and (MVect? t) (MVect? g)
         (consistent? (MVect-arg t) (MVect-arg g))))
  (or (Dyn? t)
      (Dyn? g)
      (both-unit? t g)
      (both-bool? t g)
      (both-int? t g)
      (both-char? t g)
      (both-float? t g)
      (consistent-fns? t g)
      (consistent-tuples? t g)
      (consistent-grefs? t g)
      (consistent-gvects? t g)
      (consistent-mrefs? t g)
      (consistent-mvects? t g)))

#|
Join :: type X type -> type
This is the join of the least precise latice
⊑ latice example:
Int --> Int
/   \
/     \
/       \        Joins ↑
Dyn --> Int Int --> Dyn
\       /        Meets ↓
\     /
\   /
Dyn --> Dyn
|
Dyn
|#

(struct Bottom ([t1 : Schml-Type] [t2 : Schml-Type]) #:transparent)
(define-type Schml-Type?! (U Bottom Schml-Type))

#;(: join+ (Schml-Type* -> Schml-Type?!))
#;(define (join+ t+)
  (match t+
    [(list) (error 'join+ "needs non-empty list")]
    [(list t) t]
    [(cons t t+)
     (let ([t?! (join+ t+)])
       (if (Bottom? t?!)
           t?!
           (join t t?!)))]))

(: up ((Bottom -> Schml-Type) -> (Schml-Type Schml-Type -> Schml-Type)))
(define ((up exit) t g)
  (match* (t g)
    [((Dyn) g)     g]
    [(t     (Dyn)) t]
    [(t     t)     t]
    [(t     g)     (exit (Bottom t g))]))

(: down ((Bottom -> Schml-Type) -> (Schml-Type Schml-Type -> Schml-Type)))
(define ((down exit) t g)
  (match* (t g)
    [((and t (Dyn)) _) t]
    [(_ (and g (Dyn))) g]
    [(t t) t]
    [(t g) (exit (Bottom t g))]))
  
#;(: join (Schml-Type Schml-Type -> Schml-Type?!))
#;(define (join t g)
  ;; Typed racket made me do it...
  (call/ec
   (lambda ([exit : (Bottom -> Schml-Type)])
     (: j (Schml-Type Schml-Type -> Schml-Type))
     (define (j t g)
       (match* (t g)
         [((Dyn) g) g]
         [(t (Dyn)) t]
         [((Unit) (Unit)) UNIT-TYPE]
         [((Int)  (Int))  INT-TYPE]
         [((Bool) (Bool)) BOOL-TYPE]
         [((Fn ta ta* tr) (Fn ga ga* gr)) #:when (= ta ga)
          (Fn ta (map j ta* ga*) (j tr gr))]
         [((STuple ta t*) (STuple ga g*)) #:when (= ta ga)
          (STuple ta (map j t* g*))]
         [((GRef t) (GRef g))
          (GRef (j t g))]
         [((GVect t) (GVect g))
          (GVect (j t g))]
         [((MRef t) (MRef g))
          (j t g)]
         [(t g) (exit (Bottom t g))]))
     (j t g))))

(: move :
   (Schml-Type Schml-Type -> Schml-Type)
   -> (Schml-Type Schml-Type -> Schml-Type))
(define ((move u/d/fail) t g)
  (let m : Schml-Type ([t : Schml-Type t] [g : Schml-Type g])
       (match* (t g)
         [((Fn ta ta* tr) (Fn ga ga* gr)) #:when (= ta ga)
          (Fn ta (map m ta* ga*) (m tr gr))]
         [((STuple ta t*) (STuple ga g*)) #:when (= ta ga)
          (STuple ta (map m t* g*))]
         [((GRef t) (GRef g))
          (GRef (m t g))]
         [((GVect t) (GVect g))
          (GVect (m t g))]
         [((MRef t) (MRef g))
          (MRef (m t g))]
         [(t g) (u/d/fail t g)])))

(: move?! : ((Bottom -> Schml-Type) -> (Schml-Type Schml-Type -> Schml-Type))
   -> (Schml-Type Schml-Type -> Schml-Type?!))
(define ((move?! u/d) t g)
  (call/ec
   (lambda ([return : (Bottom -> Schml-Type)])
     ((move (u/d return)) t g))))

(: move+ : ((Bottom -> Schml-Type) -> (Schml-Type Schml-Type -> Schml-Type))
   -> (Schml-Type* -> Schml-Type?!))
(define ((move+ u/d) t+)
  (call/ec
   (lambda ([return : (Bottom -> Schml-Type)])
     (define m (move (u/d return)))
     (let move+ : Schml-Type ([t+ : Schml-Type* t+])
          (match t+
            [(list) (error 'move+ "must be non empty list")]
            [(list t) t]
            [(cons t t+) (m t (move+ t+))])))))

(: join : Schml-Type Schml-Type -> Schml-Type?!)
(define join (move?! up))

(: join+ : Schml-Type* -> Schml-Type?!)
(define join+ (move+ up))

(: meet : Schml-Type Schml-Type -> Schml-Type?!)
(define meet (move?! down))
(: meet+ : Schml-Type* -> Schml-Type?!)
(define meet+ (move+ down))


(define-forms
  (Coercion coercion)
  (Twosome type1 type2 lbl)
  ;; TODO Come up with a better name for this
  (Quote-Coercion const)
  (Compose-Coercions fst snd)
  (Make-Coercion t1 t2)
  ;; I am swithing to using the Cast and Interpreted Cast for the following
  ;; Forms
  ;(Coerce coercion expression) 
  ;(Interpreted-Coerce coercion expression)
  ;; Identity Cast
  ;; "Calculated No Op Cast"
  (Identity)
  (Id-Coercion-Huh expression)
  (Id-Coercion)
  ;; Projection Coercion
  ;; "Project from dynamic at type blaming label if it fails"
  ;; G?ˡ in most papers
  ;; T?ˡ for this compiler
  (Project type label)
  ;; Injection Coercion
  ;; "Inject into the dynamic type at type"
  (Inject type)
  ;; Sequence Coercion
  ;; "Perform the first Coercion and then Another"
  (Sequence fst snd)
  ;; Fail Coercion
  ;; "Fail with the label"
  (Failed label)
  ;; Function Coercion
  ;; "Proxy a function call with args coercions and return coercion"
  ;; I am switching over to using the Fn form for this
  #;(Proxy-Fn arity args return)
  (Fn-Coercion-Arity c)
  (Fn-Coercion args return)
  (Fn-Coercion-Arg coercion index)
  (Fn-Coercion-Return coercion)
  ;;
  (CTuple num items)
  (Tuple-Coercion items)
  (Tuple-Coercion-Huh c)
  (Tuple-Coercion-Num c)
  (Tuple-Coercion-Item c indx)
  (Make-Tuple-Coercion make-uid t1 t2 lbl)
  (Compose-Tuple-Coercion Uid c1 c2)
  (Mediating-Coercion-Huh c)
  ;; Guarded Reference Coercion
  ;; "Proxy a Guarded Reference's Reads and writes"
  (Ref read write)
  (Ref-Coercion read write)
  (Ref-Coercion-Read expression)
  (Ref-Coercion-Write ref)
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
  ;; Coercion Manipulation Stuff
  (Sequence-Coercion-Huh crcn)
  (Sequence-Coercion fst snd)
  (Sequence-Coercion-Fst seq)
  (Sequence-Coercion-Snd seq)
  (Project-Coercion type label)
  (Project-Coercion-Huh crcn)
  (Project-Coercion-Type prj)
  (Project-Coercion-Label prj)
  (Inject-Coercion type)
  (Inject-Coercion-Huh crnc)
  (Inject-Coercion-Type inj)
  (Failed-Coercion label)
  (Failed-Coercion-Huh crcn)
  (Failed-Coercion-Label fld)
  (Ref-Coercion-Huh crcn)
  (MRef-Coercion-Huh crcn)
  (MVect-Coercion-Huh crcn)
  (Fn-Coercion-Huh crcn)
  ;; (Make-Coercion t1 t2 lbl)
  (Make-Fn-Coercion make-uid t1 t2 lbl)
  (Compose-Fn-Coercion comp-uid c1 c2))

(define-type Cast-Fml* (Listof Cast-Fml))
(define-type Cast-Fml (Fml Uid Schml-Type))

(define-type Cast-Literal (U Schml-Literal Blame-Label))

(define-type Src srcloc)

(define-type Tag-Symbol (U 'Int 'Bool 'Char 'Unit
                           'Fn 'Atomic 'Boxed 'GRef
                           'GVect 'MRef 'MVect 'STuple))

(define-type Schml-Coercion
  (Rec C (U Identity
            (Project Schml-Type Blame-Label)
            (Inject Schml-Type)
            (Sequence C C)
            (Failed Blame-Label)
            (Fn Index (Listof C) C)
            (Ref C C)
            (MonoRef Schml-Type)
            (MonoVect Schml-Type)
            (CTuple Index (Listof C)))))

(define IDENTITY : Identity (Identity))

(define-type Schml-Coercion* (Listof Schml-Coercion))

(define-type Data-Literal (U Integer Inexact-Real Char String))

(: data-literal? : Any -> Boolean : Data-Literal)
(define (data-literal? x)
  (or (exact-integer? x)
      (inexact-real? x)
      (char? x)
      (string? x)))

#|------------------------------------------------------------------------------
  Compact Types and Coercions are a compile time hash-consing of types
  They are introduced by hoist types in Language Cast-or-Coerce3.1
------------------------------------------------------------------------------|#

;; Represents the shallow tree structure of types where all subtrees
;; of the type are either and atomic type or a identifier for a type.
(define-type Compact-Type
  (U (Fn Index (Listof Immediate-Type) Immediate-Type)
     (STuple Index (Listof Immediate-Type))
     (GRef Immediate-Type) (MRef Immediate-Type)
     (GVect Immediate-Type) (MVect Immediate-Type)))

;; Represent the shallow tree structure of coercions where all
;; subtrees of the type are either atomic types, the identity coercion
;; or coercion identifiers.
(define-type Compact-Coercion
  (U (Project Immediate-Type Blame-Label)
     (Inject Immediate-Type)
     (Sequence Immediate-Coercion Immediate-Coercion)
     (Failed Blame-Label)
     (Fn Index (Listof Immediate-Coercion) Immediate-Coercion)
     (MonoRef Immediate-Type)
     (MonoVect Immediate-Type)
     (CTuple Index (Listof Immediate-Coercion))
     (Ref Immediate-Coercion Immediate-Coercion)))

;; TODO (andre) a more descriptive name for this would be
;; Immediate-Type
(define-type Immediate-Type (U Atomic-Type (Static-Id Uid)))

;; A type representing coercions that have already been
;; allocated at runntime or are small enought to fit into
;; a register at runtime. 
;; TODO (andre) since types are completely static consider changing
;; the type representation so that types are allocated as
;; untagged values. Doing so would simplify type case and
;; allow cheaper allocation of fails and injects at runtime.
;; (NOTE) This should be done after implementation of the non
;; N^2 implementation of guarded reference coercions because
;; that may require injects to carry blame labels thus reducing
;; the impact of this optimization.
(define-type Immediate-Coercion (U Identity (Static-Id Uid)))
 
(define-type Coercion/Immediate-Type
  (Rec C (U Identity
            (Failed Blame-Label)
            (Project Immediate-Type Blame-Label)
            (Inject Immediate-Type)
            (Sequence C C)
            (Fn Index (Listof C) C)
            (Ref C C)
            (MonoRef Immediate-Type)
            (MonoVect Immediate-Type)
            (CTuple Index (Listof C)))))

(define-type Coercion/Immediate-Type* (Listof Coercion/Immediate-Type))

#;(define-type (Map-Expr E1 E2)
  (case->
   [(Type Schml-Type) -> (Type Schml-Type)]
   [(Quote Schml-Literal) -> (Quote Schml-Literal)]
   [(Quote Cast-Literal) -> (Quote Cast-Literal)]
   [(Quote-Coercion Schml-Coercion) -> (Quote-Coercion Schml-Coercion)]
   [(Quote-Coercion Immediate-Coercion) -> (Quote-Coercion Immediate-Coercion)]
   [(Code-Label Uid) -> (Code-Label Uid)]
   [(Var Uid) -> (Var Uid)]
   [(Static-Id Uid) -> (Static-Id Uid)]
   [No-Op -> No-Op]
   [Halt -> Halt]
   [Success -> Success] 
   [(Lambda (Listof Uid) E1) -> (Lambda (Listof Uid) E2)]
   [(Lambda (Listof (Fml Uid Schml-Type)) E1) ->
    (Lambda (Listof (Fml Uid Schml-Type)) E2)]
   [(App E1 (Listof E1)) -> (App E2 (Listof E2))]
   [(App-Code E1 (Listof E1)) -> (App-Code E2 (Listof E2))]
   [(App-Fn E1 (Listof E1)) -> (App-Fn E2 (Listof E2))]
   [(App-Fn-or-Proxy Uid E1 (Listof E1)) -> (App-Fn-or-Proxy Uid E2 (Listof E2))]
   [(App-Closure E1 E1 (Listof E1)) -> (App-Closure E2 E2 (Listof E2))]
   [(If E1 E1 E1) -> (If E2 E2 E2)]
   [(Observe E1 Schml-Type) -> (Observe E2 Schml-Type)]
   [(Blame E1) -> (Blame E2)]
   [(Repeat Uid E1 E1 E1) -> (Repeat Uid E2 E2 E2)]
   [(Op Schml-Prim (Listof E1)) -> (Op Schml-Prim (Listof E2))]
   [(Letrec (Listof (Pair Uid E1)) E1) -> (Letrec (Listof (Pair Uid E2)) E2)]
   [(Letrec (Listof (Bnd Uid Schml-Type E1)) E1) -> (Letrec (Listof (Bnd Uid Schml-Type E2)) E2)]
   [(Let (Listof (Pair Uid E1)) E1) -> (Let (Listof (Pair Uid E2)) E2)]
   [(Let (Listof (Bnd Uid Schml-Type E1)) E1) -> (Let (Listof (Bnd Uid Schml-Type E2)) E2)]
   [(Labels (Listof (Pair Uid (Code Uid* E1))) E1) ->
    (Labels (Listof (Pair Uid (Code Uid* E2))) E2)]
   [(Begin (Listof E1) E1) -> (Begin (Listof E2) E2)]
   [(Gbox E1) -> (Gbox E2)]
   [(Gunbox E1) -> (Gunbox E2)]
   [(Gbox-set! E1 E1) -> (Gbox-set! E2 E2)]
   [(Gvector E1 E1) -> (Gvector E2 E2)]
   [(Gvector-set! E1 E1 E1) -> (Gvector-set! E2 E2 E2)]
   [(Gvector-ref E1 E1) -> (Gvector-ref E2 E2)]
   [(Cast E1 (Twosome Schml-Type Schml-Type Blame-Label)) ->
    (Cast E2 (Twosome Schml-Type Schml-Type Blame-Label))]
   [(Cast E1 (Coercion Schml-Coercion)) ->
    (Cast E2 (Coercion Schml-Coercion))]
   [(Interpreted-Cast E1 (Twosome E1 E1 E1)) -> (Interpreted-Cast E2 (Twosome E2 E2 E2))]
   [(Interpreted-Cast E1 (Coercion E1)) -> (Interpreted-Cast E2 (Coercion E2))]
   [(Fn-Caster E1) -> (Fn-Caster E2)]
   [(Type-Dyn-Huh E1) -> (Type-Dyn-Huh E2)]
   [(Type-Tag E1) -> (Type-Tag E2)] 
   [(Type-Fn-arity E1) -> (Type-Fn-arity E2)]
   [(Type-Fn-arg E1 E1) -> (Type-Fn-arg E2 E2)]
   [(Type-Fn-return E1) -> (Type-Fn-return E2)]
   [(Type-Fn-Huh E1) -> (Type-Fn-Huh E2)]
   [(Type-GVect-Huh E1) -> (Type-GVect-Huh E2)]
   [(Type-GRef-Huh E1) -> (Type-GRef-Huh E2)]
   [(Type-GRef-Of E1) -> (Type-GRef-Of E2)]
   [(Type-GVect-Of E1) -> (Type-GRef-Of E2)]
   #;[(Let-Static* (Listof (Pair Uid Compact-Type))
        (Listof (Pair Uid Compact-Coercion)) E1)
      -> (Let-Static* (Listof (Pair Uid Compact-Type))
           (Listof (Pair Uid Compact-Coercion)) E2)]
   [(Dyn-tag E1) -> (Dyn-tag E2)]
   [(Dyn-immediate E1) -> (Dyn-immediate E2)]
   [(Dyn-type E1) -> (Dyn-type E2)]
   [(Dyn-value E1) -> (Dyn-value E2)]
   [(Fn-Proxy (List Index Uid) E1 E1) ->
    (Fn-Proxy (List Index Uid) E2 E2)]
   [(Fn-Proxy-Huh E1) ->
    (Fn-Proxy-Huh E2)]
   [(Fn-Proxy-Closure E1) ->
    (Fn-Proxy-Closure E2)]
   [(Fn-Proxy-Coercion E1) ->
    (Fn-Proxy-Coercion E2)]
   [(Compose-Coercions E1 E1) -> (Compose-Coercions E2 E2)]
   [(Compose-Fn-Coercion Uid E1 E1) ->
    (Compose-Fn-Coercion Uid E2 E2)]
   [(Fn-Coercion-Arity E1) -> (Fn-Coercion-Arity E2)]
   [(Fn-Coercion (Listof E1) E1) -> (Fn-Coercion (Listof E2) E2)]
   [(Fn-Coercion-Arg E1 E1) -> (Fn-Coercion-Arg E2 E2)]
   [(Fn-Coercion-Return E1) -> (Fn-Coercion-Return E2)]
   [(Ref-Coercion E1 E1) -> (Ref-Coercion E2 E2)]
   [(Ref-Coercion-Read E1) -> (Ref-Coercion-Read E2)]
   [(Ref-Coercion-Write E1) -> (Ref-Coercion-Write E2)]
   [(Hybrid-Proxy Uid E1 E1) -> (Hybrid-Proxy Uid E1 E2)]
   [(Hybrid-Proxy-Huh E1) ->(Hybrid-Proxy-Huh E2)] 
   [(Hybrid-Proxy-Closure E1) -> (Hybrid-Proxy-Closure E2)] 
   [(Hybrid-Proxy-Coercion E1) -> (Hybrid-Proxy-Coercion E2)]
   [(Sequence-Coercion E1 E1) -> (Sequence-Coercion E2 E2)]
   [(Sequence-Coercion-Huh E1) -> (Sequence-Coercion-Huh E2)]
   [(Sequence-Coercion-Fst E1) -> (Sequence-Coercion-Fst E2)]
   [(Sequence-Coercion-Snd E1) -> (Sequence-Coercion-Snd E2)]
   [(Project-Coercion E1 E1) -> (Project-Coercion E2 E2)]
   [(Project-Coercion-Huh E1) ->   (Project-Coercion-Huh E2)]
   [(Project-Coercion-Type E1) -> (Project-Coercion-Type E2)]
   [(Project-Coercion-Label E1) -> (Project-Coercion-Label E2)]
   [(Inject-Coercion E1) -> (Inject-Coercion E2)]
   [(Inject-Coercion-Huh E1) -> (Inject-Coercion-Huh E2)]
   [(Inject-Coercion-Type E1) -> (Inject-Coercion-Type E2)]
   [(Failed-Coercion E1) ->   (Failed-Coercion E2)]
   [(Failed-Coercion-Huh E1) -> (Failed-Coercion-Huh E2)]
   [(Failed-Coercion-Label E1) -> (Failed-Coercion-Label E2)]
   [(Ref-Coercion-Huh E1) -> (Ref-Coercion-Huh E2)]
   [(Fn-Coercion-Huh E1) -> (Fn-Coercion-Huh E2)]
   [(Make-Coercion E1 E1 E1) -> (Make-Coercion E2 E2 E2)]
   [(Make-Fn-Coercion Uid E1 E1 E1) -> (Make-Fn-Coercion Uid E2 E2 E2)]
   [(Compose-Fn-Coercion Uid E1 E1) -> (Compose-Fn-Coercion Uid E2 E2)]
   [(Id-Coercion-Huh E1) -> (Id-Coercion-Huh E2)]
   [Id-Coercion -> Id-Coercion]   
   [(Tag Tag-Symbol) -> (Tag Tag-Symbol)]))

;; TODO submit a patch to racket so that this isn't necisarry
;; adhoc polymorphism encoded by case lambda across all


;(: map-expr (All (A B) (A -> B) -> (Map-Expr A B)))
#;(define ((map-expr f) e)
  (match e
    [(and c (or (Type _) (Quote _) (Quote-Coercion _)
                (Code-Label _) (Var _) (Static-Id _)
                (Tag _)))
     c]
    [(and c (or (No-Op) (Halt) (Success))) c]
    ;; Don't use compound forms anymore
    [(Lambda i* e) (Lambda i* (f e))]
    [(App e e*) (App (f e) (map f e*))]
    [(App-Code e e*) (App-Code (f e) (map f e*))]
    [(App-Fn e e*) (App-Fn (f e) (map f e*))]
    [(App-Fn-or-Proxy u e e*) (App-Fn-or-Proxy u (f e) (map f e*))]
    [(Fn-Proxy i e1 e2)
     (Fn-Proxy i (f e1) (f e2))]
    [(Fn-Proxy-Huh e) 
     (Fn-Proxy-Huh (f e))]
    [(Fn-Proxy-Closure e)
     (Fn-Proxy-Closure (f e))]
    [(Fn-Proxy-Coercion e)
     (Fn-Proxy-Coercion (f e))]
    [(If e1 e2 e3) (If (f e1) (f e2) (f e3))]
    [(Op p e*) (Op p (map f e*))]
    [(Letrec b* e) (Letrec (map (map-bnd f) b*) (f e))]
    [(Let b* e) (Let (map (map-bnd f) b*) (f e))]
    [(Labels b* e) (Labels (map (map-bnd-code f) b*) (f e))]
    [(Begin e* e) (Begin (map f e*) (f e))]
    [(Gbox e) (Gbox (f e))]
    [(Gunbox e) (Gunbox (f e))]
    [(Gbox-set! e1 e2) (Gbox-set! (f e1) (f e2))]
    [(Gvector e1 e2) (Gvector (f e1) (f e1))]
    [(Gvector-set! e1 e2 e3) (Gvector-set! (f e1) (f e2) (f e3))]
    [(Gvector-ref e1 e2) (Gvector-ref (f e1) (f e2))]
    [(Cast e i) (Cast (f e) i)]
    [(Interpreted-Cast e1 (Twosome e2 e3 e4))
     (Interpreted-Cast (f e1) (Twosome (f e2) (f e3) (f e4)))]
    [(Interpreted-Cast e1 (Coercion e2))
     (Interpreted-Cast (f e1) (Coercion (f e2)))]
    [(Compose-Coercions e1 e2) (Compose-Coercions (f e1) (f e2))]
    [(Compose-Fn-Coercion u c1 c2)
     (Compose-Fn-Coercion u (f c1) (f c2))]
    [(Id-Coercion-Huh e) (Id-Coercion-Huh (f e))]
    [(and id (Id-Coercion)) id]
    [(Fn-Caster e) (Fn-Caster (f e))]
    [(Type-Dyn-Huh e) (Type-Dyn-Huh (f e))]
    [(Type-Tag e) (Type-Tag (f e))] 
    [(Type-Fn-arity e) (Type-Fn-arity (f e))]
    [(Type-Fn-arg e1 e2) (Type-Fn-arg (f e1) (f e2))]
    [(Type-Fn-return e) (Type-Fn-return (f e))]
    [(Type-Fn-Huh e) (Type-Fn-Huh (f e))]
    [(Type-GVect-Huh e) (Type-GVect-Huh (f e))]
    [(Type-GRef-Huh e) (Type-GRef-Huh (f e))]
    [(Type-GRef-Of e) (Type-GRef-Of (f e))]
    [(Type-GVect-Of e) (Type-GRef-Of (f e))]
    [(App-Closure e1 e2 e*) (App-Closure (f e1) (f e2) (map f e*))]
    [(Let-Static* bt bc e) (Let-Static* bt bc (f e))]
    [(Dyn-tag e) (Dyn-tag (f e))]
    [(Dyn-immediate e) (Dyn-immediate (f e))]
    [(Dyn-type e) (Dyn-type (f e))]
    [(Dyn-value e) (Dyn-value (f e))]
    [other (error 'forms/map-expr "match failed: ~a" other)]))

(define-type (Map-Bnd E1 E2)
  (case->
   [(Bnd Uid Schml-Type E1) -> (Bnd Uid Schml-Type E2)]
   [(Pair Uid E1) -> (Pair Uid E2)]))

(: map-bnd (All (A B) (A -> B) -> (Map-Bnd A B)))
(define ((map-bnd f) b)
  (match b
    [(Bnd i t e) (Bnd i t (f e))]
    [(cons i e) (cons i (f e))]))


