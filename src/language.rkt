#lang typed/racket
#|------------------------------------------------------------------------------+
|File: src/language.rkt                                                         |
+-------------------------------------------------------------------------------+
|Authors:                                                                       |
| Andre Kuhlenshmidt (akuhlens@indiana.edu)                                     |
|                                                                               |
+-------------------------------------------------------------------------------+
|Description:
|This file contains the definitions of the various intermediate languages that
|the compiler uses and helper functions that define some of the semantics of
|this language. The languages are composed of reusable language forms that are
|parameterizabe. These language forms are defined by the define-forms macro. The
|language forms are actually the types created by defining polymorphic racket
|structs which are the same structs that the compiler manipulates in order to
|change the AST.
|
|Notes:
|There are many functions, particularly predicates, that are defined in an
|overly modular fashion. This is because typed-racket's typechecker currently
|has exponential type-checking behavior for conditionals. Simplifying and
|deviding the cases dramatically reduces the time needed to typecheck.
|
|
+-------------------------------------------------------------------------------+
TODO

+------------------------------------------------------------------------------|#
(require "./helpers.rkt")
(provide (all-defined-out))

#|
   This is the structure that is passed to the compiler in order
   to store state about which options are enabled. This really shouldn't be here.
   TODO: find a more logical location for this structure...
|#

(define-type Semantics (U 'Lazy-D))
(struct Config ([source-path : Path]
                [semantics : Semantics]
                [exec-path : Path]
                [c-path : Path]
                [keep-c : Boolean]
                [c-flags : (Listof String)]
                [asm-path : (Option Path)]))


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
|#

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
  (Cast expression type-exp type-cast label)
  ;; TODO Interpreted-Cast
  (Runtime-Cast expression type-exp type-cast label)
  (Fn-Cast expressiong type-exp type-cast label)
  ;;Type Operations
  (Type-tag expression)
  (Type-Fn-arity expression)
  (Type-Fn-arg expression index)
  (Type-Fn-return expression)
  (Type-GRef-to expression)
  (Type-GVect-to expression)
  ;; closure Representation
  (Fn-Caster expression)
  (Closure-Data code caster variables)
  (Closure-code var)
  (Closure-ref this var)
  (Closure-caster this)
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
  ;; Observational Operations
  (Blame expression)
  (Observe expression type)
  ;; Lambda subforms
  (Castable caster body)
  (Bound closure variables body)
  (Free caster variables body)
  ;; Static Global Binding
  (Labels bindings body)
  ;; Benchmarking tools language forms
  ;; low cost repetition
  (Repeat var start end body)
  ;; TODO figue out an appropriate comment about all forms here
  (Halt)
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
  (Gproxy-blames expression))

(define NO-OP (No-Op))


(define-type Blame-Label String)



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


#|
   Unique Identifier
|#

(struct Uid ([prefix : String] [suffix : Natural]) #:transparent)

(define-type Uid* (Listof Uid))

(define FIRST-UID-SUFFIX 0)

(: uid->string (-> Uid String))
(define (uid->string u)
  (format "u~a_~a" (number->string (Uid-suffix u)) (Uid-prefix u)))

;; Are two uid equal?
(: uid=? (-> Uid Uid Boolean))
(define (uid=? [u : Uid] [v : Uid])
  ;; a little bit of a hack that shouldn't be noticable
  (or #;(eq? u v)
      (= (Uid-suffix u) (Uid-suffix v))))

;; If you are in the state monad you can purely allocate and increment
(define-type Nat Natural)
(: uid-state (String -> (State Nat Uid)))
(define (uid-state (name : String))
  (lambda ((s : Natural))
    (values (Uid name s) (add1 s))))

;; If you are in state passing style you
;; could use this to allocate and increment
(: next-uid (-> String Natural (values Uid Natural)))
(define (next-uid prefix suffix) next-uid
  (values (Uid prefix suffix) (add1 suffix)))

;; A simple macro for helpign with state passing style
;; I am trying to move away from this so that code can
;; be less verbose
(define-syntax let-uid*
  (syntax-rules ()
    [(_ (wrong ...) others ...) (raise-syntax-error 'let-uid "next must always be an identifier")]
    [(_ next () body ...)(let () body ...)]
    [(_ next ([name0 prefix0] [name* prefix*] ...) body ...)
     (let-values ([(name0 next) (next-uid prefix0 next)])
       (let-uid* next ([name* prefix*] ...)
		 body ...))]))

;; Sometimes the order of the types is important but I have
;; not figured out a good system for figuring out what the
;; order should be except trial and error.
(: make-begin
   (case->
    (-> D3-Effect* No-Op D3-Effect)
    (-> D3-Effect* D3-Value D3-Value)
    (-> D3-Effect* D3-Tail  D3-Tail)
    (-> D3-Effect* D3-Pred  D3-Pred)
    (-> C7-Effect* C7-Value  C7-Value)
    (-> C7-Effect* No-Op C7-Effect)
    (-> D1-Effect* D1-Value  D1-Value)
    (-> D1-Effect* No-Op D1-Effect)
#;  (-> D3-Effect* D3-Trivial D3-Value)
    (-> D2-Effect* D2-Value  D2-Value)
    (-> D2-Effect* D2-Pred D2-Pred)
    (-> D2-Effect* No-Op D2-Effect)
    (-> D4-Effect* D4-Tail D4-Tail)
    (-> D4-Effect* No-Op D4-Effect)
    (-> D5-Effect* D5-Tail D5-Tail)))

;; make a begin language form but splice nested begins into the
;; newly made begin.
(define (make-begin eff* res)
  (let ([eff* (foldr splice-eff '() eff*)])
    (cond
      [(null? eff*) res]
      ;; In effect position a begin of one effect is the ;
      ;; same as the effect alone
      [(and (No-Op? res) (null? (cdr eff*))) (car eff*)]
      ;; If the result is a begin I assume that the begin
      ;; was made with make-begin.
      [(Begin? res)
       (Begin (append eff* (Begin-effects res)) (Begin-value res))]
      [else (Begin eff* res)])))

(: splice-eff
   (case->
    ;; Since D2 effect is a strict subset of D1-effect
    ;; It must come before D1 because the first type to
    ;; match is used
    (-> D5-Effect D5-Effect* D5-Effect*)
    (-> D4-Effect D4-Effect* D4-Effect*)
    (-> D3-Effect D3-Effect* D3-Effect*)
    (-> C7-Effect C7-Effect* C7-Effect*)
    (-> D2-Effect D2-Effect* D2-Effect*)
    (-> D1-Effect D1-Effect* D1-Effect*)))
(define (splice-eff eff rest)
  (cond
    [(No-Op? eff) rest]
    [(Begin? eff) (foldr splice-eff rest (Begin-effects eff))]
    [else (cons eff rest)]))

#|-----------------------------------------------------------------------------+
| Language/Schml-Syntax 
+-----------------------------------------------------------------------------|#
#| Maybe type |#
(define-type Src srcloc)

;; The language created by schml/read
(define-type Syntax-Lang (Prog String (Listof Stx)))

(define-type Stx (Syntaxof Any));; This might not be what I want considier Just Syntax
(define-type Stx* (Listof Stx))

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
  (U IntxInt->Int-Primitive IntxInt->Bool-Primitive))

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

#|-----------------------------------------------------------------------------+
| Language/Schml0
+-----------------------------------------------------------------------------|#
(define-type Schml0-Lang (Prog (List String Natural) S0-Expr))

(define-type (S0-Form E)
  (U (Lambda Schml-Fml* (Ann E (Option Schml-Type)))
     (Letrec S0-Bnd* E)
     (Let S0-Bnd* E)
     (App E (Listof E))
     (Op Schml-Primitive (Listof E))
     (If E E E)
     (Ascribe E Schml-Type (Option Blame-Label))
     (Var Uid)
     (Quote Schml-Literal)
     (Begin (Listof E) E)
     (Repeat Uid E E E)
     ;; Monotonic effects
     (Mbox E)
     (Munbox E)
     (Mbox-set! E E)
     (Mvector E E)
     (Mvector-set! E E E)
     (Mvector-ref E E)
     ;; Guarded effects
     (Gbox E)
     (Gunbox E)
     (Gbox-set! E E)
     (Gvector E E)
     (Gvector-set! E E E)
     (Gvector-ref E E)))

(define-type S0-Expr
  (Rec E (Ann (S0-Form E) Src)))

(define-type S0-Expr* (Listof S0-Expr))
(define-type S0-Bnd (Bnd Uid Schml-Type? S0-Expr))
(define-type S0-Bnd* (Listof S0-Bnd))

#|-----------------------------------------------------------------------------+
| Language/Schml1 created by type-check
+-----------------------------------------------------------------------------|#
(define-type Schml1-Lang
  (Prog (List String Natural Schml-Type) S1-Expr))

(define-type S1-Expr
  ;; This slightly complicated formulation of lambda's Structure allows me
  ;; To rely on lambdas to have function types during cast insertion
  (Rec E (U (Ann (Lambda Schml-Fml* E) (Pair Src Schml-Fn-Type))
            (Ann (U
		 (Letrec S1-Bnd* E)
		 (Let S1-Bnd* E)
		 (App E (Listof E))
		 (Op (Ann Schml-Primitive Schml-Type*) (Listof E))
		 (If E E E)
		 (Ascribe E Schml-Type (Option Blame-Label))
		 (Var Uid)
		 (Quote Schml-Literal)
                 (Begin (Listof E) E)
                 (Repeat Uid E E E)
                 ;; Monotonic effects
                 (Mbox E)
                 (Munbox E)
                 (Mbox-set! E E)
                 (Mvector E E)
                 (Mvector-set! E E E)
                 (Mvector-ref E E)
                 ;; Guarded effects
                 (Gbox E)
                 (Gunbox E)
                 (Gbox-set! E E)
                 (Gvector E E)
                 (Gvector-set! E E E)
                 (Gvector-ref E E))
	      (Pair Src Schml-Type)))))

(define-type S1-Bnd (Bnd Uid Schml-Type S1-Expr))
(define-type S1-Bnd* (Listof S1-Bnd))

(: schml-primitive->type
   (-> Schml-Primitive (Fn Index (Listof (U Int Bool)) (U Int Bool Unit))))
(define (schml-primitive->type p)
  (cond
   [(IntxInt->Bool-primitive? p) INTxINT->BOOL-TYPE]
   [(IntxInt->Int-primitive? p)  INTxINT->INT-TYPE]
   [(timer-primitive? p)         ->UNIT-TYPE]))

(define-type ConsistentT (Schml-Type Schml-Type . -> . Boolean))
(: consistent? ConsistentT)
(define (consistent? t g)
  ;; Typed racket made me structure the code this way.
  ;; TODO change the names to be both-unit? ...
  (: unit? ConsistentT)
  (define (unit? t g) (and (Unit? t) (Unit? g)))
  (: bool? ConsistentT)
  (define (bool? t g) (and (Bool? t) (Bool? g)))
  (: int? ConsistentT)
  (define (int? t g) (and (Int? t) (Int? g)))
  (: fn? ConsistentT)
  (define (fn? t g)
    (and (Fn? t) (Fn? g)
         (= (Fn-arity t) (Fn-arity g))
         (andmap consistent? (Fn-fmls t) (Fn-fmls g))
         (consistent? (Fn-ret t) (Fn-ret g))))
  (: gref? ConsistentT)
  (define (gref? t g)
    (and (GRef? t) (GRef? g)
         (consistent? (GRef-arg t) (GRef-arg g))))
  (: gvect? ConsistentT)
  (define (gvect? t g)
    (and (GVect? t) (GVect? g)
         (consistent? (GVect-arg t) (GVect-arg g))))
  (or (Dyn? t) (Dyn? g)
      (unit? t g)
      (bool? t g)
      (int? t g)
      (fn? t g)
      (gref? t g)
      (gvect? t g)
      


      ;; These will need to be added back but were adding
      ;; considerable type checking time.
      #;
      (and (GVect? t) (GVect? g)
           (consistent? (GVect-arg t) (GVect-arg g)))
      #;
      (and (MRef? t) (MRef? g)
           (consistent? (MRef-arg t) (MRef-arg g)))
      #;
      (and (MVect? t) (MVect? g)
           (consistent? (MVect-arg t) (MVect-arg g)))))

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
    #;
    [(and (MRef? t) (MRef? g))
     (join (MRef-arg t) (MRef-arg g))]
    #;
    [(and (MVect? t) (MVect? g))
    (join (MVect-arg t) (MVect-arg g))]
    [else (error 'join "Types are not consistent")]))


#|-----------------------------------------------------------------------------
We are going to UIL
-----------------------------------------------------------------------------|#

(define-type UIL-Prim  (U Schml-Prim Array-Prim))
(define-type UIL-Prim! (U Schml-Prim! Array-Prim! Print-Prim! Runtime-Prim!))
(define-predicate uil-prim-effect? UIL-Prim!)
(define-predicate uil-prim-value? UIL-Prim)

(define-type UIL-Expr-Prim (U Array-Prim IxI->I-Prim))

(define-type Array-Prim (U 'Alloc 'Array-ref))
(define-type Array-Prim! 'Array-set!)
(define-type Print-Prim! (U 'Printf 'Print))
(define-type Runtime-Prim! (U 'Exit))

(define-type (UIL-Op E) (Op UIL-Prim (Listof E)))
(define-type (UIL-Op! E) (Op UIL-Prim! (Listof E)))

(define-type Cast-Fml* (Listof Cast-Fml))
(define-type Cast-Fml (Fml Uid Schml-Type))

(define-type Cast-Literal (U Schml-Literal Blame-Label))

#|-----------------------------------------------------------------------------+
| The Cast Language Family Types, Primitives, Literals, and Terminals        |
+-----------------------------------------------------------------------------|#
#|-----------------------------------------------------------------------------+
| Language/Cast0 created by insert-implicit-casts                              |
+-----------------------------------------------------------------------------
| Description: At the begining of this section of the compiler all cast in the |
| ast are performed on known schml language types. But as the compiler imposes |
| the semantics of cast there become situations where a type is dependant on   |
| econtents of a variable. At this point casts are no longer able to be         |
| completely compiled into primitives. These casts require a sort of cast      |
| interpreter which is built later.                                            |
| In general this compiler tries to move as mainy casts into the primitive     |
| operations. Whatever casts are left at the end are then convert to           |
| applications of the cast interpreter function.
+-----------------------------------------------------------------------------|#

(define-type Cast0-Lang (Prog (List String Natural Schml-Type) C0-Expr))

(define-type C0-Expr
  (Rec E (U ;; Non-Terminals
	  (Lambda Uid* E)
	  (Letrec C0-Bnd* E)
	  (Let C0-Bnd* E)
	  (App E (Listof E))
	  (Op Schml-Primitive (Listof E))
	  (If E E E)
	  (Cast E Schml-Type Schml-Type Blame-Label)
          (Begin C0-Expr* E)
          (Repeat Uid E E E)
          ;; Monotonic
          (Mbox (Ann E (Pair Blame-Label Schml-Type)))
          (Munbox E)
          (Munbox (Ann E (Pair Blame-Label Schml-Type)))
          (Mbox-set! (Ann E (Pair Blame-Label Schml-Type)) E)
          (Mbox-set! E E)
          (Mvector E E)
          (Mvector-set! E E E)
          (Mvector-ref E E)
          ;; Guarded effects
          (Gbox E)
          (Gunbox E)
          (Gbox-set! E E)
          (Gvector E E)
          (Gvector-set! E E E)
          (Gvector-ref E E)
	  ;; Terminals
	  (Var Uid)
	  (Quote Cast-Literal))))

(define-type C0-Expr* (Listof C0-Expr))
(define-type C0-Bnd   (Pair Uid C0-Expr))
(define-type C0-Bnd*  (Listof C0-Bnd))



#| ----------------------------------------------------------------------------+
|Cast1                                                                         |
------------------------------------------------------------------------------|#

(define-type Cast1-Lang
  (Prog (List String Natural Schml-Type) C1-Expr))

(define-type C1-Expr
  (Rec E (U ;; Non-Terminals
	  (Lambda Uid* (Castable (Option Uid) E))
	  (Letrec C1-Bnd* E)
	  (Let C1-Bnd* E)
	  (App E (Listof E))
	  (Op Schml-Primitive (Listof E))
	  (If E E E)
          ;; Terminals
          (Begin C1-Expr* E)
          (Repeat Uid E E E)
	  (Var Uid)
	  (Quote Cast-Literal)
          ;; Casts with different ways of getting the same semantics
	  (Runtime-Cast E E E E)
	  (Cast E Schml-Type Schml-Type Blame-Label)
	  (Fn-Cast E Schml-Type Schml-Type Blame-Label)
          ;; FN-Type operations
          (Type-Fn-arg E E)
          (Type-Fn-return E)
          (Type-Fn-arity E)
          ;; Observations
          (Blame E)
          ;; Monotonic
          (Mbox (Ann E (Pair Blame-Label Schml-Type)))
          (Munbox E)
          (Munbox (Ann E (Pair Blame-Label Schml-Type)))
          (Mbox-set! (Ann E (Pair Blame-Label Schml-Type)) E)
          (Mbox-set! E E)
          (Mvector E E)
          (Mvector-set! E E E)
          (Mvector-ref E E)
          ;; Guarded Intermediate Representation
          (Gbox E)
          (Gunbox E)
          (Gbox-set! E E)
          (Gvector E E)
          (Gvector-set! E E E)
          (Gvector-ref E E))))

(define-type C1-Expr* (Listof C1-Expr))
(define-type C1-Bnd   (Pairof Uid C1-Expr))
(define-type C1-Bnd*  (Listof C1-Bnd))

#|-----------------------------------------------------------------------------+
| Language/Cast2 created by introduce-castable-references                      |
+-----------------------------------------------------------------------------|#

(define-type Cast2-Lang
 (Prog (List String Natural Schml-Type) C2-Expr))

(define-type C2-Expr
  (Rec E (U ;; Non-Terminals
	  (Lambda Uid* (Castable (Option Uid) E))
	  (Letrec C2-Bnd* E)
	  (Let C2-Bnd* E)
	  (App E (Listof E))
          (Op Schml-Primitive (Listof E))
	  (If E E E)
          ;; Terminals
          (Begin C2-Expr* E)
          (Repeat Uid E E E)
	  (Var Uid)
          (Type Schml-Type)
	  (Quote Cast-Literal)
          ;; Casts with different ways of getting the same semantics
	  (Runtime-Cast E E E E)
	  (Cast E Schml-Type Schml-Type Blame-Label)
	  (Fn-Cast E Schml-Type Schml-Type Blame-Label)
          ;; FN-Type operations
          (Type-Fn-arg E E)
          (Type-Fn-return E)
          (Type-Fn-arity E)
          (Type-GRef-to E)
          ;; Observations
          (Blame E)
          ;; Guarded Representation
          (GRep E))))

(define-type C2-Expr* (Listof C2-Expr))
(define-type C2-Bnd (Pairof Uid C2-Expr))
(define-type C2-Bnd* (Listof C2-Bnd))


(define-type (GRep A)
  (U (GRep-Value A)
     (GRep-Effect A)))

(define-type (GRep-Value A)
  (U (GRep-proxied? A)
     (UGbox A)
     (UGbox-ref A)
     (UGvect A A)
     (UGvect-ref A A)
     (Gproxy A A A A)
     (Gproxy-for A)
     (Gproxy-from A)
     (Gproxy-to A)
     (Gproxy-blames A)))

(define-type (GRep-Effect A)
  (U (UGbox-set! A A)
     (UGvect-set! A A A)))


#|-----------------------------------------------------------------------------+
| Language/Cast3 created by interpret-casts                                    |
+-----------------------------------------------------------------------------|#


(define-type Cast3-Lang
  (Prog (List String Natural Schml-Type) C3-Expr))

(define-type C3-Expr
  (Rec E (U ;; Non-Terminals
	  (Lambda Uid* (Castable (Option Uid) E))
	  (Letrec C3-Bnd* E)
	  (Let C3-Bnd* E)
	  (App E (Listof E))
          (Op Schml-Primitive (Listof E))
	  (If E E E)
          (Begin C3-Expr* E)
          (Repeat Uid E E E)
          ;; closure operations
          (Fn-Caster E)
          ;; Type operations
          (Type-tag E)
          (Type-Fn-arg E E)
          (Type-Fn-return E)
          (Type-Fn-arity E)
          (Type-GRef-to E)
          (Type-GVect-to E)
          ;; Dyn operations
          (Dyn-tag E)
          (Dyn-immediate E)
          (Dyn-type E)
          (Dyn-value E)
          (Dyn-make E E)
          ;; Observational Operations
          (Blame E)
          (Observe E Schml-Type)
          ;; Terminals
          (Type Schml-Type)
          (Tag Tag-Symbol)
	  (Var Uid)
          (GRep E)
	  (Quote Cast-Literal))))

(define-type C3-Expr* (Listof C3-Expr))
(define-type C3-Bnd   (Pair Uid C3-Expr))
(define-type C3-Bnd*  (Listof C3-Bnd))

(define-type Tag-Symbol (U 'Int 'Bool 'Unit 'Fn 'Atomic 'Boxed 'GRef 'GVect))

#|-----------------------------------------------------------------------------+
| Language/Cast created by label-lambdas                    |
+-----------------------------------------------------------------------------|#

(define-type Cast4-Lang
  (Prog (List String Natural Schml-Type) C4-Expr))

(define-type C4-Expr
  (Rec E (U ;; Non-Terminals
          (Letrec C4-Bnd-Lambda* E)
	  (Let C4-Bnd-Data* E)
	  (App E (Listof E))
          (Op Schml-Primitive (Listof E))
	  (If E E E)
          (Begin C4-Expr* E)
          (Repeat Uid E E E)
          ;; closure operations
          (Fn-Caster E)
          ;; Type operations
          (Type-tag E)
          (Type-Fn-arg E E)
          (Type-Fn-return E)
          (Type-Fn-arity E)
          (Type-GRef-to E)
          (Type-GVect-to E)
          ;; Dyn operations
          (Dyn-tag E)
          (Dyn-immediate E)
          (Dyn-type E)
          (Dyn-value E)
          (Dyn-make E E)
          ;; Observational Operations
          (Blame E)
          (Observe E Schml-Type)
          ;; Terminals
          (Type Schml-Type)
          (Tag Tag-Symbol)
	  (Var Uid)
          (GRep E)
	  (Quote Cast-Literal))))

(define-type C4-Expr* (Listof C4-Expr))
(define-type C4-Bnd-Lambda* (Listof C4-Bnd-Lambda))
(define-type C4-Bnd-Lambda  (Pairof Uid C4-Lambda))
(define-type C4-Bnd-Data* (Listof C4-Bnd-Data))
(define-type C4-Bnd-Data  (Pairof Uid C4-Expr))
(define-type C4-Lambda (Lambda Uid* (Castable (Option Uid) C4-Expr)))


#|-----------------------------------------------------------------------------+
| Language/Cast5 created by label-lambdas                    |
+-----------------------------------------------------------------------------|#

(define-type Cast5-Lang
  (Prog (List String Natural Schml-Type) C5-Expr))

(define-type C5-Expr
  (Rec E (U ;; Non-Terminals
          (Letrec C5-Bnd-Lambda* E)
	  (Let C5-Bnd-Data* E)
	  (App E (Listof E))
          (Op Schml-Primitive (Listof E))
	  (If E E E)
          (Begin C5-Expr* E)
          (Repeat Uid E E E)
          ;; closure operations
          (Fn-Caster E)
          ;; Type operations
          (Type-tag E)
          (Type-Fn-arg E E)
          (Type-Fn-return E)
          (Type-Fn-arity E)
          (Type-GRef-to E)
          (Type-GVect-to E)
          ;; Dyn operations
          (Dyn-tag E)
          (Dyn-immediate E)
          (Dyn-type E)
          (Dyn-value E)
          (Dyn-make E E) ;; This is bad and I do not like it
          ;; Observational Operations
          (Blame E)
          (Observe E Schml-Type)
          ;; Terminals
          (Type Schml-Type)
          (Tag Tag-Symbol)
	  (Var Uid)
          (GRep E)
	  (Quote Cast-Literal))))

(define-type C5-Expr* (Listof C5-Expr))
(define-type C5-Bnd-Lambda* (Listof C5-Bnd-Lambda))
(define-type C5-Bnd-Lambda  (Pairof Uid C5-Lambda))
(define-type C5-Bnd-Data* (Listof C5-Bnd-Data))
(define-type C5-Bnd-Data  (Pairof Uid C5-Expr))
(define-type C5-Lambda (Lambda Uid* (Free (Option Uid) Uid* C5-Expr)))


#|-----------------------------------------------------------------------------+
| Language/Cast6 created by convert-closures                                   |
+-----------------------------------------------------------------------------|#

(define-type Cast6-Lang
  (Prog (List String Natural Schml-Type) C6-Expr))

(define-type C6-Expr
  (Rec E (U ;; Non-Terminals
          (LetP C6-Bnd-Procedure* (LetC C6-Bnd-Closure* E))
	  (Let C6-Bnd-Data* E)
	  (App (Pair E E) (Listof E))
          (Op Schml-Primitive (Listof E))
	  (If E E E)
          (Begin C6-Expr* E)
          (Repeat Uid E E E)
          ;; closure operations
          (Closure-code E)
          (Closure-caster E)
          (Closure-ref Uid Uid)
          ;; FN-Type operations
          (Type-tag E)
          (Type-Fn-arg E E)
          (Type-Fn-return E)
          (Type-Fn-arity E)
          (Type-GRef-to E)
          (Type-GVect-to E)
          ;; Dyn operations
          (Dyn-tag E)
          (Dyn-immediate E)
          (Dyn-type E)
          (Dyn-value E)
          (Dyn-make E E) ;; This is bad and I do not like it
          ;; Observational Operations
          (Blame E)
          (Observe E Schml-Type)
          ;; Terminals
          (Type Schml-Type)
          (Tag Tag-Symbol)
	  (Var Uid)
          (GRep E)
	  (Quote Cast-Literal)
          (Code-Label Uid))))

(define-type C6-Expr* (Listof C6-Expr))
(define-type C6-Procedure
  (Procedure Uid Uid* Uid (Option Uid) Uid* C6-Expr))
(define-type C6-Closure (Closure-Data C6-Expr C6-Expr C6-Expr*))
(define-type C6-Bnd-Procedure (Pairof Uid C6-Procedure))
(define-type C6-Bnd-Procedure* (Listof C6-Bnd-Procedure))
(define-type C6-Bnd-Closure (Pairof Uid C6-Closure))
(define-type C6-Bnd-Closure* (Listof C6-Bnd-Closure))
(define-type C6-Bnd-Data (Pairof Uid C6-Expr))
(define-type C6-Bnd-Data* (Listof C6-Bnd-Data))



#|-----------------------------------------------------------------------------+
| Language/Cast7 created by convert-closures                                   |
+-----------------------------------------------------------------------------|#

(define-type Cast7-Lang
  (Prog (List String Natural Schml-Type) C7-Value))

(define-type C7-Value
  (Rec V (U ;; Non-Terminals
          (LetP C7-Bnd-Procedure* (LetC C7-Bnd-Closure* V))
	  (Let C7-Bnd-Data* V)
	  (App (Pair V V) (Listof V))
          (Op Schml-Primitive (Listof V))
	  (If V V V)
          (Begin C7-Effect* V)
          (Repeat Uid V V V)
          ;;closure operations
          ;;(Closure-ref V V)
          (Fn-Caster V)
          ;; FN-Type operations
          (Type-tag V)
          (Type-Fn-arg V V)
          (Type-Fn-return V)
          (Type-Fn-arity V)
          (Type-GRef-to V)
          ;; Dyn operations
          (Dyn-tag V)
          (Dyn-immediate V)
          (Dyn-type V)
          (Dyn-value V)
          (Dyn-make V V) ;; This is bad and I do not like it
          ;; Observational Operations
          (Blame V)
          (Observe V Schml-Type)
          ;; Terminals
          (Type Schml-Type)
          (Tag Tag-Symbol)
	  (Var Uid)
          (GRep V)
	  (Quote Cast-Literal))))

(define-type C7-Effect
  (Rec E
   (U (LetP C7-Bnd-Procedure* (LetC C7-Bnd-Closure* E))
      (Let C7-Bnd-Data* E)
      (Begin C7-Effect* No-Op)
      (Repeat Uid C7-Value C7-Value E)
      (App (Pair C7-Value C7-Value) (Listof C7-Value))
      (If C7-Value E E)
      No-Op
      (GRep-Effect C7-Value))))

(define-type C7-Value* (Listof C7-Value))
(define-type C7-Effect* (Listof C7-Effect))
(define-type C7-Procedure
  (Procedure Uid Uid* Uid (Option Uid) Uid* C7-Value))
(define-type C7-Closure (Closure-Data Uid (Option Uid) (Listof Uid)))
(define-type C7-Bnd-Procedure (Pairof Uid C7-Procedure))
(define-type C7-Bnd-Procedure* (Listof C7-Bnd-Procedure))
(define-type C7-Bnd-Closure (Pairof Uid C7-Closure))
(define-type C7-Bnd-Closure* (Listof C7-Bnd-Closure))
(define-type C7-Bnd-Data (Pairof Uid C7-Value))
(define-type C7-Bnd-Data* (Listof C7-Bnd-Data))

#|-----------------------------------------------------------------------------+
| The Constants for the representation of casts                                |
+-----------------------------------------------------------------------------|#
;; The Representation of functional types is an array
(define FN-ARITY-INDEX 0)
(define FN-RETURN-INDEX 1)
(define FN-FMLS-OFFSET 2)

;; The representation of tagged structure types
;; My thought is that types can be allocated statically
;; so there doesn't really need to be much though put
;; it may even be worth not tagging them and just laying
;; the types explicitly;
(define TYPE-TAG-MASK #b111)
(define TYPE-FN-TAG #b000)
(define TYPE-GREF-TAG #b001)
(define TYPE-GVECT-TAG #b010)
;; Hypothetical extensions to type tags
;; Though more organization could le
;;(define TYPE-GVECT-TAG #b010)
;;(define TYPE-MREF-TAG #b011)
;;(define TYPE-MVECT-TAG #b100)
;;(define TYPE-IARRAY-TAG #b101)
;;(define TYPE-MU-TAG #b110)

(define TYPE-ATOMIC-TAG #b111) ;; This should be TYPE-IMDT-TAG
;; Immediate types are tagged with #b111
(define TYPE-DYN-RT-VALUE #b0111)
(define TYPE-INT-RT-VALUE #b1111)
(define TYPE-BOOL-RT-VALUE #b10111)
(define TYPE-UNIT-RT-VALUE #b11111)

;; The representation of Dynamic Immediates
(define DYN-TAG-MASK  #b111)
(define DYN-IMDT-SHIFT 3)
(define DYN-BOXED-TAG #b000)
(define DYN-INT-TAG   #b001)
(define DYN-UNIT-TAG  #b010)
(define DYN-BOOL-TAG  #b111)

;; Boxed Dynamics are just a cons cell
(define DYN-BOX-SIZE 2)
(define DYN-VALUE-INDEX 0)
(define DYN-TYPE-INDEX 1)

;; Immediates
(define FALSE-IMDT #b000)
(define TRUE-IMDT #b001)
(define UNIT-IMDT #b000)
;; Unreachable Value
(define UNDEF-IMDT 0)

;; Guarded Representation
(define GREP-TAG-MASK #b111)
(define UGBOX-SIZE 1)
(define UGBOX-VALUE-INDEX 0)
(define UGBOX-TAG #b000)
(define GPROXY-TAG  #b001)
(define GPROXY-SIZE 4)
(define GPROXY-FOR-INDEX 0)
(define GPROXY-FROM-INDEX 1)
(define GPROXY-TO-INDEX 2)
(define GPROXY-BLAMES-INDEX 3)
(define UGVECT-SIZE #f)
(define UGVECT-TAG #b000)
(define UGVECT-SIZE-INDEX 0)
(define UGVECT-OFFSET 1)

;; GREF Type Representation
(define TYPE-GREF-SIZE  1)
(define GREF-TO-INDEX 0)

;; GVECT Type Representation
(define TYPE-GVECT-SIZE  1)
(define GVECT-TO-INDEX 0)

;; Closure representation
(define CLOS-CODE-INDEX 0)
(define CLOS-CSTR-INDEX 1)
(define CLOS-FVAR-OFFSET 2)

(define-type Data-Literal (U Integer String))

#|-----------------------------------------------------------------------------+
| Data0-Language created by impose-cast-semantics                              |
+-----------------------------------------------------------------------------|#
(define-type Data0-Lang
  (Prog (List String Natural Schml-Type) D0-Expr))

(define-type D0-Bnd-Code* (Listof D0-Bnd-Code))
(define-type D0-Bnd-Code (Pairof Uid D0-Code))
(define-type D0-Code (Code Uid* D0-Expr))

(define-type D0-Expr
  (Rec E (U (Labels D0-Bnd-Code* E)
            (Let D0-Bnd* E)
	    (App E (Listof E))
            (UIL-Op! E)
            (UIL-Op E)
	    (If E E E)
	    (Begin D0-Expr* E)
            (Repeat Uid E E E)
	    Halt
	    (Var Uid)
	    (Code-Label Uid)
	    (Quote D0-Literal))))

(define-type D0-Expr* (Listof D0-Expr))
(define-type D0-Bnd* (Listof D0-Bnd))
(define-type D0-Bnd  (Pairof Uid D0-Expr))
(define-type D0-Literal Data-Literal)

#|-----------------------------------------------------------------------------+
| Data1-Language created by normalize-context                                          |
+-----------------------------------------------------------------------------|#

(define-type Data1-Lang
  (Prog (List String Natural Schml-Type)
	(Labels D1-Bnd-Code*
		D1-Tail)))

(define-type D1-Bnd-Code* (Listof D1-Bnd-Code))
(define-type D1-Bnd-Code (Pairof Uid D1-Code))
(define-type D1-Code (Code Uid* D1-Tail))

(define-type D1-Tail
  (Rec T
   (U (Let D1-Bnd* T)
      (If D1-Pred T T)
      (Begin D1-Effect* T)
      (App D1-Value D1-Value*)
      (Op (U IxI->I-Prim Array-Prim) (Listof T))
      (Var Uid)
      Halt
      (Var Uid)
      (Code-Label Uid)
      (Quote D1-Literal))))

(define-type D1-Value
 (Rec V
  (U (Let D1-Bnd* V)
     (If D1-Pred V V)
     (Begin D1-Effect* V)
     (App V (Listof V))
     (Op (U IxI->I-Prim Array-Prim) (Listof V))
     Halt
     (Var Uid)
     (Code-Label Uid)
     (Quote D1-Literal))))

(define-type D1-Pred
 (Rec P
  (U (Let D1-Bnd* P)
     (If D1-Pred P P)
     (Begin D1-Effect* P)
     (Relop IxI->B-Prim D1-Value D1-Value))))

(define-type D1-Effect
 (Rec E
  (U (Let D1-Bnd* E)
     (If D1-Pred E E)
     (Begin D1-Effect* No-Op)
     (Repeat Uid D1-Value D1-Value E)
     (App D1-Value D1-Value*)
     (UIL-Op! D1-Value)
     No-Op)))


(define-type D1-Value* (Listof D1-Value))
(define-type D1-Effect* (Listof D1-Effect))

(define-type D1-Bnd  (Pairof Uid D1-Value))

(define-type D1-Bnd* (Listof D1-Bnd))

(define-type D1-Literal Data-Literal)


#|-----------------------------------------------------------------------------+
| Data2-Language created by remove-let                                          |
+-----------------------------------------------------------------------------|#

(define-type Data2-Lang
  (Prog (List String Natural Schml-Type)
	(Labels D2-Bnd-Code*
		D2-Body)))

(define-type D2-Body (Locals Uid* D2-Tail))
(define-type D2-Bnd-Code* (Listof D2-Bnd-Code))
(define-type D2-Bnd-Code (Pairof Uid D2-Code))
(define-type D2-Code (Code Uid* D2-Body))

(define-type D2-Tail
  (Rec T
   (U (If D2-Pred T T)
      (Begin D2-Effect* T)
      D2-Value)))

(define-type D2-Value
 (Rec V
  (U (If D2-Pred V V)
     (Begin D2-Effect* V)
     (App V (Listof V))
     (Op (U IxI->I-Prim Array-Prim) (Listof V))
     Halt
     (Var Uid)
     (Code-Label Uid)
     (Quote D2-Literal))))

(define-type D2-Pred
 (Rec P
  (U (If D2-Pred P P)
     (Begin D2-Effect* P)
     (Relop IxI->B-Prim D2-Value D2-Value))))

(define-type D2-Effect
 (Rec E
  (U (If D2-Pred E E)
     (Begin D2-Effect* No-Op)
     (Repeat Uid D2-Value D2-Value E)
     (App D2-Value D2-Value*)
     (UIL-Op! D2-Value)
     (Assign Uid D2-Value)
     No-Op)))


(define-type D2-Value* (Listof D2-Value))

(define-type D2-Effect* (Listof D2-Effect))

(define-type D2-Literal Data-Literal)


#|-----------------------------------------------------------------------------+
| Data3-Language created by remove-complex-opera                               |
+-----------------------------------------------------------------------------|#

(define-type Data3-Lang
  (Prog (List String Natural Schml-Type)
	(Labels D3-Bnd-Code*
		D3-Body)))

(define-type D3-Body (Locals Uid* D3-Tail))
(define-type D3-Bnd-Code* (Listof D3-Bnd-Code))
(define-type D3-Bnd-Code (Pairof Uid D3-Code))
(define-type D3-Code (Code Uid* D3-Body))

(define-type D3-Tail
  (Rec T
   (U (If D3-Pred T T)
      (Begin D3-Effect* T)
      (Return D3-Value))))

(define-type D3-Value
 (Rec V
  (U D3-Trivial
     Halt
     (If D3-Pred V V)
     (Begin D3-Effect* V)
     (Op (U IxI->I-Prim Array-Prim) (Listof D3-Trivial))
     (App D3-Trivial D3-Trivial*))))

(define-type D3-Pred
 (Rec P
  (U (If D3-Pred P P)
     (Begin D3-Effect* P)
     (Relop IxI->B-Prim D3-Trivial D3-Trivial))))

(define-type D3-Effect
 (Rec E
  (U (If D3-Pred E E)
     (Begin D3-Effect* No-Op)
     (Repeat Uid D3-Trivial D3-Trivial E)
     (App D3-Trivial D3-Trivial*)
     (UIL-Op! D3-Trivial)
     (Assign Uid D3-Value)
     No-Op)))

;; (TODO halt is not trivial though because we are targeting c it may be treated so)
;; remove Halt earlier
(define-type D3-Trivial
  (U (Code-Label Uid)
     (Var Uid)
     (Quote D3-Literal)))

(define-type D3-Value* (Listof D3-Value))
(define-type D3-Trivial* (Listof D3-Trivial))
(define-type D3-Effect* (Listof D3-Effect))

(define-type D3-Literal Data-Literal)



#|-----------------------------------------------------------------------------+
| Data4-Language created by flatten-assignments                                |
+-----------------------------------------------------------------------------|#

(define-type Data4-Lang
  (Prog (List String Natural Schml-Type)
	(Labels D4-Bnd-Code*
		D4-Body)))

(define-type D4-Body (Locals Uid* D4-Tail))
(define-type D4-Bnd-Code* (Listof D4-Bnd-Code))
(define-type D4-Bnd-Code (Pairof Uid D4-Code))
(define-type D4-Code (Code Uid* D4-Body))

(define-type D4-Tail
  (Rec T
   (U (If D4-Pred T T)
      (Begin D4-Effect* T)
      (Return D4-Value))))

(define-type D4-Pred
 (Rec P
  (U (If D4-Pred P P)
     (Begin D4-Effect* P)
     (Relop IxI->B-Prim D4-Trivial D4-Trivial))))

(define-type D4-Effect
 (Rec E
  (U (If D4-Pred E E)
     (Begin D4-Effect* No-Op)
     (Repeat Uid D4-Trivial D4-Trivial E)
     (UIL-Op! D4-Trivial)
     (Assign Uid D4-Value)
     No-Op)))

(define-type D4-Value
  (U D4-Trivial
     Halt
     (UIL-Op D4-Trivial)
     (App D5-Trivial D5-Trivial*)))

(define-type D4-Trivial
  (U (Code-Label Uid)
     (Var Uid)
     (Quote D4-Literal)))


(define-type D4-Trivial* (Listof D4-Trivial))
(define-type D4-Effect* (Listof D4-Effect))

(define-type D4-Literal Data-Literal)


#|-----------------------------------------------------------------------------+
| Data5-Language created by simplify-predicate                                 |
+-----------------------------------------------------------------------------|#

(define-type Data5-Lang
  (Prog (List String Natural Schml-Type)
	(Labels D5-Bnd-Code*
		D5-Body)))

(define-type D5-Body (Locals Uid* D5-Tail))
(define-type D5-Bnd-Code* (Listof D5-Bnd-Code))
(define-type D5-Bnd-Code (Pairof Uid D5-Code))
(define-type D5-Code (Code Uid* D5-Body))

(define-type D5-Tail
  (Rec T
   (U (If D5-Pred T T)
      (Begin D5-Effect* T)
      (Return D5-Value))))

(define-type D5-Pred (Relop IxI->B-Prim D5-Trivial D5-Trivial))

(define-type D5-Effect
 (Rec E
  (U (If D5-Pred (Begin D5-Effect* No-Op) (Begin D5-Effect* No-Op))
     (Repeat Uid D5-Trivial D5-Trivial (Begin D5-Effect* No-Op))
     (UIL-Op! D5-Trivial)
     (Assign Uid D5-Value)
     No-Op)))

(define-type D5-Value
  (U D5-Trivial
     Halt
     (UIL-Op D5-Trivial)
     (App D5-Trivial D5-Trivial*)
     (If D5-Pred D5-Trivial D5-Trivial)))

(define-type D5-Trivial
  (U (Code-Label Uid)
     (Var Uid)
     (Quote D5-Literal)))

(define-type D5-Trivial* (Listof D5-Trivial))
(define-type D5-Effect* (Listof D5-Effect))
(define-type D5-Value* (Listof D5-Value))

(define-type D5-Literal Data-Literal)
