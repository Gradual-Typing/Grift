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
  ;; Global is for referencing global variables by their names as strings
  (Global id)
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
  (Coerce-Tuple cast value coercion)
  (Coerce-Tuple-In-Place cast value coercion mono-address base-address index)
  (Cast-Tuple cast value t1 t2 lbl)
  (Cast-Tuple-In-Place cast value t1 t2 lbl mono-address base-address index)
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
  (Guarded-Proxy-Huh expression))

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

;; Grift types
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
(define UNIT-EXPR (Type UNIT-TYPE))

(define INT-TYPE (Int))
(define INT-EXPR (Type INT-TYPE))

(define FLOAT-TYPE (Float))
(define FLOAT-EXPR (Type FLOAT-TYPE))

(define BOOL-TYPE (Bool))
(define BOOL-EXPR (Type BOOL-TYPE))

(define CHAR-TYPE (Character))
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


(: completely-static-type? (-> Grift-Type Boolean))
;; Is the type t devoid of dyn?
(define (completely-static-type? t)
  ;; Typed Racket made me do it
  ;; This uber modular structure speeds up type checking
  (define (fn-completely-static? [t : Grift-Type]): Boolean
    (and (Fn? t)
         (andmap completely-static-type? (Fn-fmls t))
         (completely-static-type? (Fn-ret t))))
  (define (tuple-completely-static? [t : Grift-Type]): Boolean
    (and (STuple? t)
         (andmap completely-static-type? (STuple-items t))))
  (define (ref-completely-static? [t : Grift-Type])
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
| Types shared by the Grift language family
+-----------------------------------------------------------------------------|#

#| Literals of the grift languages
Only Integers and Booleans in the grift language are first
class literal constants
|#

(define-type Grift-Literal
  (U Integer Boolean Null Real Char))

#;(: platform-integer? (Any -> Boolean : Integer))
#;
(define (platform-integer? x)
  (fixnum? x))

(: grift-literal? (Any -> Boolean : Grift-Literal))
(define (grift-literal? x)
  (or (exact-integer? x)
      (char? x)
      (boolean? x)
      (null? x)
      (real? x)))

(: grift-literal->base-type (Grift-Literal -> Base-Type))
(define (grift-literal->base-type x)
  (cond
    [(char? x) CHAR-TYPE]
    [(boolean? x) BOOL-TYPE]
    [(exact-integer? x) INT-TYPE]
    [(inexact-real? x) FLOAT-TYPE]
    [(null? x)    UNIT-TYPE]
    [else (error 'language/grift-literal->type "~a" x)]))

;; Types in the grift languages
(define-type  Base-Type (U Int Bool Unit Character Float))

(: base-type? (Any -> Boolean : Base-Type))
(define (base-type? x)
  (or (Int? x)
      (Bool? x)
      (Character? x)
      (Unit? x)
      (Float? x)))

(define-type+ Grift-Type ([Grift-Type* Listof]
			  [Grift-Type? Option])
  (U Dyn
     Base-Type
     Grift-Fn-Type
     Grift-Ref-Type
     Grift-Tuple-Type))

;; type known at runtime only for monotonic references, the uid is for
;; the entire reference cell, you have to access the second component
;; of the cell to get the type.

(define-type Grift-Fn-Type
  (Fn Index Grift-Type* Grift-Type))

(define-type Grift-Tuple-Type
  (STuple Index Grift-Type*))

(define-type Grift-Ref-Type
  (U (GRef  Grift-Type)
     (GVect Grift-Type)
     (MRef  Grift-Type)
     (MVect Grift-Type)))

(define-type Atomic-Type (U Base-Type Dyn))

(: atomic-type? : Any -> Boolean : Atomic-Type)
(define (atomic-type? x)
  (or (Dyn? x) (base-type? x)))

(: grift-type? (Any -> Boolean : Grift-Type))
(define (grift-type? x)
  (or (atomic-type? x)
      (grift-fn? x)
      (grift-ref? x)
      (grift-tuple? x)))

(define-predicate grift-type*? Grift-Type*)

(define-predicate grift-fn? Grift-Fn-Type)
(define-predicate grift-tuple? Grift-Tuple-Type)
(define-predicate grift-ref? Grift-Ref-Type)

(define-type+ Grift-Fml ([Grift-Fml* Listof])
  (Fml Uid Grift-Type))



(define-type ConsistentT (Grift-Type Grift-Type -> Boolean))
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

(struct Bottom ([t1 : Grift-Type] [t2 : Grift-Type]) #:transparent)
(define-type Grift-Type?! (U Bottom Grift-Type))

(: up ((Bottom -> Grift-Type) -> (Grift-Type Grift-Type -> Grift-Type)))
(define ((up exit) t g)
  (match* (t g)
    [((Dyn) g)     g]
    [(t     (Dyn)) t]
    [(t     t)     t]
    [(t     g)     (exit (Bottom t g))]))

(: down ((Bottom -> Grift-Type) -> (Grift-Type Grift-Type -> Grift-Type)))
(define ((down exit) t g)
  (match* (t g)
    [((and t (Dyn)) _) t]
    [(_ (and g (Dyn))) g]
    [(t t) t]
    [(t g) (exit (Bottom t g))]))

(: move :
   (Grift-Type Grift-Type -> Grift-Type)
   -> (Grift-Type Grift-Type -> Grift-Type))
(define ((move u/d/fail) t g)
  (let m : Grift-Type ([t : Grift-Type t] [g : Grift-Type g])
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

(: move?! : ((Bottom -> Grift-Type) -> (Grift-Type Grift-Type -> Grift-Type))
   -> (Grift-Type Grift-Type -> Grift-Type?!))
(define ((move?! u/d) t g)
  (call/ec
   (lambda ([return : (Bottom -> Grift-Type)])
     ((move (u/d return)) t g))))

(: move+ : ((Bottom -> Grift-Type) -> (Grift-Type Grift-Type -> Grift-Type))
   -> (Grift-Type* -> Grift-Type?!))
(define ((move+ u/d) t+)
  (call/ec
   (lambda ([return : (Bottom -> Grift-Type)])
     (define m (move (u/d return)))
     (let move+ : Grift-Type ([t+ : Grift-Type* t+])
          (match t+
            [(list) (error 'move+ "must be non empty list")]
            [(list t) t]
            [(cons t t+) (m t (move+ t+))])))))

(: join : Grift-Type Grift-Type -> Grift-Type?!)
(define join (move?! up))

(: join+ : Grift-Type* -> Grift-Type?!)
(define join+ (move+ up))

(: meet : Grift-Type Grift-Type -> Grift-Type?!)
(define meet (move?! down))
(: meet+ : Grift-Type* -> Grift-Type?!)
(define meet+ (move+ down))

(define-forms
  (Coercion coercion)
  (Twosome type1 type2 lbl)
  ;; TODO Come up with a better name for this
  (Quote-Coercion const)
  (Compose-Coercions fst snd)
  (Make-Coercion t1 t2)
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
  (Fn-Coercion-Arity c)
  (Fn-Coercion args return)
  (Fn-Coercion-Arg coercion index)
  (Fn-Coercion-Return coercion)
  (Id-Fn-Coercion arity)
  (Fn-Coercion-Return-Set! fn return)
  (Fn-Coercion-Arg-Set! fn index arg)
  ;;
  (CTuple num items)
  (Tuple-Coercion items)
  (Tuple-Coercion-Huh c)
  (Tuple-Coercion-Num c)
  (Tuple-Coercion-Item c indx)
  (Id-Tuple-Coercion num)
  (Tuple-Coercion-Item-Set! tuple index item)
  (Make-Tuple-Coercion make-uid t1 t2 lbl)
  (Mediating-Coercion-Huh c)
  ;; Proxied Reference Coercion
  ;; "Proxy a Proxied Reference's Reads and writes"
  (Ref read write choice) ;; the choice indicates whether it is a ref or a vector coercion
  (Ref-Coercion read write flag)
  (Ref-Coercion-Read expression)
  (Ref-Coercion-Write ref)
  (Ref-Coercion-Ref-Huh crcn) ;; checks if the ref coercion is for a ref or a vector value
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
  (HC project? t1 label inject? t2 med)
  (Quote-HCoercion coercion)
  (HC-Project-Huh hc)
  (HC-Inject-Huh hc)
  (HC-Identity-Huh hc)
  (HC-Med hc)
  (HC-Label hc)
  (HC-T1 hc)
  (HC-T2 hc))

(define-type Cast-Fml* (Listof Cast-Fml))
(define-type Cast-Fml (Fml Uid Grift-Type))

(define-type Cast-Literal (U Grift-Literal Blame-Label))

(define-type Src srcloc)

(define-type Tag-Symbol (U 'Int 'Bool 'Char 'Unit
                           'Fn 'Atomic 'Boxed 'GRef
                           'GVect 'MRef 'MVect 'STuple))

(define-type Proxied-Symbol (U 'GRef 'GVect))

(define-type (Mediating-Coercion C)
  (U Identity
     (Fn Index (Listof C) C)
     (CTuple Index (Listof C))
     (Ref C C Proxied-Symbol)
     (MonoRef Grift-Type)
     (MonoVect Grift-Type)
     (Failed Blame-Label)))

(define-type Grift-Coercion
  (Rec C (U (Project Grift-Type Blame-Label)
            (Inject Grift-Type)
            (Sequence C C)
            (Mediating-Coercion C))))

(define-type Hyper-Coercion
  (HC Boolean Grift-Type (Option Blame-Label)
      Boolean Grift-Type
      (Mediating-Coercion Hyper-Coercion)))

(define-type Mixed-Coercion
  (U Grift-Coercion
     Hyper-Coercion
     (Mediating-Coercion Hyper-Coercion)))


(define IDENTITY : Identity (Identity))
(define ID-EXPR (Quote-Coercion IDENTITY))
(define ZERO-EXPR (Quote 0))


(define-type Grift-Coercion* (Listof Grift-Coercion))

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
     (HC Boolean Immediate-Type (Option Blame-Label)
         Boolean Immediate-Type
         Immediate-Coercion)
     (Failed Blame-Label)
     (Fn Index (Listof Immediate-Coercion) Immediate-Coercion)
     (MonoRef Immediate-Type)
     (MonoVect Immediate-Type)
     (CTuple Index (Listof Immediate-Coercion))
     (Ref Immediate-Coercion Immediate-Coercion Proxied-Symbol)))

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
            (Ref C C Proxied-Symbol)
            (MonoRef Immediate-Type)
            (MonoVect Immediate-Type)
            (CTuple Index (Listof C)))))

(define-type Coercion/Immediate-Type* (Listof Coercion/Immediate-Type))

(define-type (Map-Bnd E1 E2)
  (case->
   [(Bnd Uid Grift-Type E1) -> (Bnd Uid Grift-Type E2)]
   [(Pair Uid E1) -> (Pair Uid E2)]))

(: map-bnd (All (A B) (A -> B) -> (Map-Bnd A B)))
(define ((map-bnd f) b)
  (match b
    [(Bnd i t e) (Bnd i t (f e))]
    [(cons i e) (cons i (f e))]))

(define-type Id (U Uid String))

(define-type VectorAccessMode (U 'check-bounds 'no-check-bounds))
