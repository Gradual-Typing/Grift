#lang typed/racket/base

(require "../helpers.rkt"
         "../unique-identifiers.rkt"
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
  ;; Perform a No Operation
  (No-Op)
  ;; Effect operations
  ;; Monotonic effects
  (Mbox value)
  (Munbox box)
  (Mbox-set! box value)
  (Mvector value constructor)
  (Mvector-set! vector offset value)
  (Mvector-ref vector offset)
  ;; Guarded effects
  (Gbox value)
  (Gunbox box)
  (Gbox-set! box value)
  (Gvector len init-val)
  (Gvector-set! vector offset value)
  (Gvector-ref vector offset)
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
  (Type-Fn-arity expression)
  (Type-Fn-arg expression index)
  (Type-Fn-return expression)
  (Type-Fn-Huh type)
  (Type-GVect-Huh type)
  (Type-GRef-Huh type)
  (Type-GRef-Of expression)
  (Type-GVect-Of expression)
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
  (Repeat var start end body)
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

(define NO-OP (No-Op))


(define-type Blame-Label String)
(define blame-label? string?)


#| Types throughout the languages |#

;; Schml types
(define-forms
  (Unit)
  (Int)
  (Bool)
  (Dyn)
  (Fn arity fmls ret)
  (MRef  arg)
  (MVect arg)
  (GRef  arg)
  (GVect arg))

;; TODO I am unsure of if these are being used
;; find out and act appropriately
#;
(define-forms
  (String-Ptr)
  (Any-Value)
  (Any-Type)
  (Void-Type)
  (Bottom-Type))

;;Constants for the types
(define UNIT-TYPE (Unit))
(define INT-TYPE (Int))
(define BOOL-TYPE (Bool))
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
(define INTxINT-TYPE (list INT-TYPE INT-TYPE))
(define INTxINT->BOOL-TYPE (Fn 2 INTxINT-TYPE BOOL-TYPE))
(define INTxINT->INT-TYPE (Fn 2 INTxINT-TYPE INT-TYPE))
(define ->UNIT-TYPE (Fn 0 '() UNIT-TYPE))
(define ->INT-TYPE (Fn 0 '() INT-TYPE))

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
  (define (ref-completely-static? [t : Schml-Type])
    (or (and (GRef? t) (completely-static-type? (GRef-arg t)))
        (and (MRef? t) (completely-static-type? (MRef-arg t)))
        (and (GVect? t) (completely-static-type? (GVect-arg t)))
        (and (MVect? t) (completely-static-type? (MVect-arg t)))))
  (or (Int? t)
      (Bool? t)
      (fn-completely-static? t)
      (ref-completely-static? t)))







#|-----------------------------------------------------------------------------+
| Types shared by the Schml language family
+-----------------------------------------------------------------------------|#

(define-type Schml-Primitive (U Schml-Prim Schml-Prim!))

#;(: schml-primitive? (-> Any Boolean : Schml-Primitive))
#;
(define (schml-primitive? x)
  (or (schml-prim? x) (schml-prim!? x)))

(define-predicate schml-primitive? Schml-Primitive)

(define-type Schml-Prim
  (U IntxInt->Int-Primitive
     IntxInt->Bool-Primitive
     ->Int-Primitive))

(define-predicate schml-prim? Schml-Prim)

#;(: schml-prim? (Any -> Boolean : Schml-Prim))
#;
(define (schml-prim? x)
  (or (IntxInt->Int-primitive? x)
      (IntxInt->Bool-primitive? x)))

(define-type Schml-Prim!
  (U Timer-Primitive))

(define-predicate schml-prim!? Schml-Prim!)
#;(: schml-prim!? (Any -> Boolean : Schml-Prim!))
#;
(define (schml-prim!? x)
  (or (timer-primitive? x)))

(define-type IntxInt->Int-Primitive (U '* '+ '-
                                        'binary-and 'binary-or 'binary-xor
                                        '%/ '%>> '%<<))

(define-type IxI->I-Prim IntxInt->Int-Primitive)

(define-predicate IntxInt->Int-primitive? IntxInt->Int-Primitive)

(define-type ->Int-Primitive (U 'read-int))
(define-type ->I-Prim ->Int-Primitive)

(define-predicate ->Int-primitive? ->Int-Primitive)


#;(: IntxInt->Int-primitive? (-> Any Boolean : IntxInt->Int-Primitive))
#;
(define (IntxInt->Int-primitive? x)
  
  #;(memq x '(* + - binary-and binary-or binary-xor %/ %>> %<<))
  )


(define-type IntxInt->Bool-Primitive (U '< '<= '= '> '>=))
(define-type IxI->B-Prim IntxInt->Bool-Primitive)
(define-predicate IntxInt->Bool-primitive? IntxInt->Bool-Primitive)

#;(: IntxInt->Bool-primitive? (Any -> Boolean : IntxInt->Bool-Primitive))
#;
(define (IntxInt->Bool-primitive? x)
  (memq x '(< <= = > >=))
  #;                     
  (or (eq? '< x)
      (eq? '<= x)
      (eq? '= x)
      (eq? '> x)
      (eq? '>= x)))

#|
(define-type IntxNon0->Int-Primitives '/)
(define-type Ix!0->I IntxNon0->Int-Primitives)
(define-predicate IntxNon0->Int-Prim? IntxNon0->Int-Primitives)

(define-type IntxNibble->Int-Primitives (U '<< '>>))
(define-type IxN->I IntxNibble->Int-Primitives)
(define-predicate IntxNibble->Int-Prim? IntxNibble->Int-Primitives)
|#

(define-type Timer-Primitive (U 'timer-start 'timer-stop 'timer-report))

(: timer-primitive? (Any -> Boolean : Timer-Primitive))
(define (timer-primitive? x)
  (or (eq? 'timer-start  x)
      (eq? 'timer-stop   x)
      (eq? 'timer-report x)))

#| Literals of the schml languages
   Only Integers and Booleans in the schml language are first
   class literal constants
|#

(define-type Schml-Literal
  (U Integer Boolean Null))

#;(: platform-integer? (Any -> Boolean : Integer))
#;
(define (platform-integer? x)
  (fixnum? x))

(: schml-literal? (Any -> Boolean : Schml-Literal))
(define (schml-literal? x)
  (or (exact-integer? x)
      (boolean? x)
      (null? x)))

(: schml-literal->type (Schml-Literal -> (U Bool Int Unit)))
(define (schml-literal->type x)
  (cond
    [(boolean? x) BOOL-TYPE]
    [(integer? x) INT-TYPE]
    [(null? x)    UNIT-TYPE]
    [else (error 'language/schml-literal->type "~a" x)]))

;; Types in the schml languages
(define-type  Base-Type (U Int Bool Unit))

(: base-type? (Any -> Boolean : Base-Type))
(define (base-type? x)
  (or (Int? x)
      (Bool? x)
      (Unit? x)))

(define-type+ Schml-Type ([Schml-Type* Listof]
			  [Schml-Type? Option])
  (U Dyn
     Base-Type
     Schml-Fn-Type
     Schml-Ref-Type))

(define-type Schml-Fn-Type
  (Fn Index Schml-Type* Schml-Type))

(define-type Schml-Ref-Type
  (U (GRef  Schml-Type)
     (GVect Schml-Type)
     (MRef  Schml-Type)
     (MVect Schml-Type)))

(define-type Atomic-Schml-Type (U Unit Int Bool Dyn))

(: schml-type? (Any -> Boolean : Schml-Type))
(define (schml-type? x)
  (or (Dyn? x)
      (base-type? x)
      (schml-fn? x)
      (schml-ref? x)))
      

(define-predicate schml-type*? Schml-Type*)
#;(: schml-type*? (Any -> Boolean : Schml-Type*))
#;
(define (schml-type*? x)
  (or (null? x)
      (and (pair? x)
           (schml-type? (car x))
           (schml-type*? (cdr x)))))

(define-predicate schml-fn? Schml-Fn-Type)
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

(define-type ConsistentT (Schml-Type Schml-Type . -> . Boolean))
(: consistent? ConsistentT)
(define (consistent? t g)
  ;; Typed racket made me structure the code this way.
  ;; TODO change the names to be both-unit? ...
  (: both-unit? ConsistentT)
  (define (both-unit? t g) (and (Unit? t) (Unit? g)))
  (: both-bool? ConsistentT)
  (define (both-bool? t g) (and (Bool? t) (Bool? g)))
  (: both-int? ConsistentT)
  (define (both-int? t g) (and (Int? t) (Int? g)))
  (: consistent-fns? ConsistentT)
  (define (consistent-fns? t g)
    (and (Fn? t) (Fn? g)
         (= (Fn-arity t) (Fn-arity g))
         (andmap consistent? (Fn-fmls t) (Fn-fmls g))
         (consistent? (Fn-ret t) (Fn-ret g))))
  (: consistent-grefs? ConsistentT)
  (define (consistent-grefs? t g)
    (and (GRef? t) (GRef? g)
         (consistent? (GRef-arg t) (GRef-arg g))))
  (: consistent-gvects? ConsistentT)
  (define (consistent-gvects? t g)
    (and (GVect? t) (GVect? g)
         (consistent? (GVect-arg t) (GVect-arg g))))
  (or (Dyn? t)
      (Dyn? g)
      (both-unit? t g)
      (both-bool? t g)
      (both-int? t g)
      (consistent-fns? t g)
      (consistent-grefs? t g)
      (consistent-gvects? t g)))

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

(: join (Schml-Type Schml-Type . -> . Schml-Type))
(define (join t g)
  (cond
    [(Dyn? t) g] [(Dyn? g) t]
    [(and (Unit? t) (Unit? g)) UNIT-TYPE]
    [(and (Int? t) (Int? g)) INT-TYPE]
    [(and (Bool? t) (Bool? g)) BOOL-TYPE]
    [(and (Fn? t) (Fn? g) (= (Fn-arity t) (Fn-arity g)))
     (Fn (Fn-arity t)
         (map join (Fn-fmls t) (Fn-fmls g))
         (join (Fn-ret t) (Fn-ret g)))]
    [(and (GRef? t) (GRef? g))
     (GRef (join (GRef-arg t) (GRef-arg g)))]
    [(and (GVect? t) (GVect? g))
     (join (GVect-arg t) (GVect-arg g))]
    [else (error 'join "Types are not consistent")]))


(define-forms
  (Coercion coercion)
  (Twosome type1 type2 lbl)
  ;; TODO Come up with a better name for this
  (Quote-Coercion const)
  (Compose-Coercions fst snd)
  ;; I am swithing to using the Cast and Interpreted Cast for the following
  ;; Forms
  ;(Coerce coercion expression) 
  ;(Interpreted-Coerce coercion expression)
  ;; Identity Cast
  ;; "Calculated No Op Cast"
  (Identity)
  (Id-Coercion-Huh E)
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
  ;; Guarded Reference Coercion
  ;; "Proxy a Guarded Reference's Reads and writes"
  (Ref read write)
  (Ref-Coercion read write)
  (Ref-Coercion-Read expression)
  (Ref-Coercion-Write ref)
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
  (Fn-Coercion-Huh crcn)
  (Make-Coercion t1 t2 lbl)
  (Make-Fn-Coercion make-uid t1 t2 lbl)
  (Compose-Fn-Coercion comp-uid c1 c2))

(define-type Cast-Fml* (Listof Cast-Fml))
(define-type Cast-Fml (Fml Uid Schml-Type))

(define-type Cast-Literal (U Schml-Literal Blame-Label))

(define-type Src srcloc)

(define-type Tag-Symbol (U 'Int 'Bool 'Unit 'Fn 'Atomic 'Boxed 'GRef 'GVect))

(define-type Schml-Coercion
  (Rec C (U Identity
            (Project Schml-Type Blame-Label)
            (Inject Schml-Type)
            (Sequence C C)
            (Failed Blame-Label)
            (Fn Index (Listof C) C)
            (Ref C C))))

(define IDENTITY : Identity (Identity))

(define-type Schml-Coercion* (Listof Schml-Coercion))

(define-type Data-Literal (U Integer String))

#|------------------------------------------------------------------------------
  Compact Types and Coercions are a compile time hash-consing of types
  They are introduced by hoist types in Language Cast-or-Coerce3.1
------------------------------------------------------------------------------|#

;; Represents the shallow tree structure of types where all subtrees
;; of the type are either and atomic type or a identifier for a type.
(define-type Compact-Type
  (U (Fn Index (Listof Prim-Type) Prim-Type)
     (GRef Prim-Type) (MRef Prim-Type)
     (GVect Prim-Type) (MVect Prim-Type)))

;; Represent the shallow tree structure of coercions where all
;; subtrees of the type are either atomic types, the identity coercion
;; or coercion identifiers.
(define-type Compact-Coercion
  (U (Project Prim-Type Blame-Label)
     (Inject Prim-Type)
     (Sequence Immediate-Coercion Immediate-Coercion)
     (Failed Blame-Label)
     (Fn Index (Listof Immediate-Coercion) Immediate-Coercion)
     (Ref Immediate-Coercion Immediate-Coercion)))

;; TODO (andre) a more descriptive name for this would be
;; Immediate-Type
(define-type Prim-Type (U Atomic-Schml-Type (Static-Id Uid)))

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
 
(define-type Coercion/Prim-Type
  (Rec C (U Identity
            (Failed Blame-Label)
            (Project Prim-Type Blame-Label)
            (Inject Prim-Type)
            (Sequence C C)
            (Fn Index (Listof C) C)
            (Ref C C))))

(define-type Coercion/Prim-Type* (Listof Coercion/Prim-Type))




#;(define-type (Map-Bnd E1 E2)
  (case->
   [(Bnd Uid Schml-Type E1) -> (Bnd Uid Schml-Type E2)]
   [(Pair Uid E1) -> (Pair Uid E2)]))

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

#;(: map-bnd (All (A B) (A -> B) -> (Map-Bnd A B)))
#;(define ((map-bnd f) b)
  (match b
    [(Bnd i t e) (Bnd i t (f e))]
    [(cons i e) (cons i (f e))]))

(: map-bnd-code
   (All (A B) (A -> B) -> ((cons Uid (Code Uid* A)) -> (cons Uid (Code Uid* B)))))
(define ((map-bnd-code f) b)
  (match-define (cons i (Code i* e)) b)
  (cons i (Code i* (f e))))
