#lang typed/racket


(require schml/src/helpers)
(provide (all-defined-out))

#|
   This is the structure that is passed to the compiler in order
   to store state about which options are enabled. This really shouldn't be here.
   TODO: find a more logical location for this structure...
|#

(define-type Semantics (U 'Lazy-D))
(struct Config ([semantics : Semantics]
                [exec-path : Path]
                [c-path : Path]))


#|
Commonly used and recognize language forms
In general I try to switch structs as the meaning of forms
change but I do allow the types and *syntax* of forms to
change. In general any field named annotation is not really
a usefull field but allows me to store information that may
be usefull for optimizations or keeping state.
|#

(define-forms
  (Prog annotation expression)
  (Ann value data)
  ;; Procedural abstraction
  (Lambda formals body)
  (App operator operands)
  (Var id)
  (If test then else)
  (Ascribe expression type label)
  ;; Primitive operators supported by the language
  (Op operator operands)
  ;; recursive binding
  (Letrec bindings body)
  ;; non recursive binding
  (Let bindings body)
  ;; sequence operator
  (Begin effects value)
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
  (Gvector value constructor)
  (Gvector-set! vector offset value)
  (Gvector-ref vector offset)
  ;; various imediates markers
  (Quote literal)    ;; imediate data in general
  (Code-Label value) ;; marks a uid as refering to a uid
  (Tag bits)         ;; an tag for an imediate value
  (Type type)        ;; an atomic type
  ;; Effectfull expressions
  ;; typed bindings annotations
  (Fml identifier type)
  (Bnd identifier type expression)
  ;; Different casts
  (Cast expression type-exp type-cast label)
  (Runtime-Cast expression type-exp type-cast label)
  (Fn-Cast expressiong type-exp type-cast label)
  ;;Type Operations
  (Type-Fn-ref expression index)
  (Type-tag expression)
  ;; closure operations
  (Fn-Caster expression)
  ;; Dyn operations
  (Dyn-tag expression)
  (Dyn-immediate expression)
  (Dyn-ref expression index)
  (Dyn-make expression type)
  ;; Observational Operations
  (Blame expression)
  (Observe expression type)
  ;; Non recursive binding forms
  (LetP bindings body)
  (LetC bindings body)
  ;; Lambda subforms
  (Castable caster body)
  (Bound closure variables body)
  (Free caster variables body)
  ;; Static Global Binding
  (Labels bindings body)
  ;; TODO figue out an appropriate comment about all forms here
  (Procedure this params caster bound-vars body)
  ;; represents a set of moves to initialize variables before
  (Code variables body)
  (Closure-Data code caster variables)
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
  (Gproxy for-exp from-exp to-exp blames-exp)
  (Gproxy-for expression)
  (Gproxy-from expression)
  (Gproxy-to expression)
  (Gproxy-blames expression))

(define-type Blame-Label String)

(define NO-OP (No-Op))

#| Types throughout the languages |#

;; Schml types
(define-forms
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
(define-forms
  (String-Ptr)
  (Any-Value)
  (Any-Type)
  (Void-Type)
  (Bottom-Type))

;;Constants for the types
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

(define (shallow-consistent? t g)
  (or (Dyn? t) (Dyn? g)
      (and (Int? t) (Int? g))
      (and (Bool? t) (Bool? g))
      (and (Fn? t) (Fn? g))
      (and (GRef? t) (GRef? g))
      (and (GVect? t) (GVect? g))
      (and (MRef? t) (MRef? t))
      (and (MVect? t) (MVect? t))))

(: completely-static-type? (-> Schml-Type Boolean))
(define (completely-static-type? t)
  (or (Int? t)
      (Bool? t)
      (and (Fn? t)
           (andmap completely-static-type? (Fn-fmls t))
           (completely-static-type? (Fn-ret t)))
      (and (GRef? t)
           (completely-static-type? (GRef-arg t)))
      (and (MRef? t)
           (completely-static-type? (MRef-arg t)))
      (and (GVect? t)
           (completely-static-type? (GVect-arg t)))
      (and (MVect? t)
           (completely-static-type? (MVect-arg t)))))


#| Unique Identifier
   These are currently done using state passing style in passes.
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
(define (uid-state (name : String))
  : (State Natural Uid)
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

(define-type Schml-Prim
  (U IntxInt->Int-Primitives IntxInt->Bool-Primitives
     #;Ref-Primitives
     ;;IntxNon0->Int-Primitives IntxNibble->Int-Primitives
     ;; I want to add these but they break the symplicity of the type system
     ;; I think in the meantime I will add them as IntxInt->Int and claim that
     ;; they are unsafe features that are hidden away.
     ))

(define-predicate Schml-Prim? Schml-Prim)

(define-type IntxInt->Int-Primitives (U '* '+ '- 'binary-and 'binary-or '%/ '%>> '%<<))
(define-type IxI->I-Prim IntxInt->Int-Primitives)
(define-predicate IntxInt->Int-Prim? IntxInt->Int-Primitives)

(define-type IntxInt->Bool-Primitives (U '< '<= '= '> '>=))
(define-type IxI->B-Prim IntxInt->Bool-Primitives)
(define-predicate IntxInt->Bool-Prim? IntxInt->Bool-Primitives)

(define-type IntxNon0->Int-Primitives '/)
(define-type Ix!0->I IntxNon0->Int-Primitives)
(define-predicate IntxNon0->Int-Prim? IntxNon0->Int-Primitives)

(define-type IntxNibble->Int-Primitives (U '<< '>>))
(define-type IxN->I IntxNibble->Int-Primitives)
(define-predicate IntxNibble->Int-Prim? IntxNibble->Int-Primitives)

#| Literals of the schml languages
   Only Integers and Booleans in the schml language are first
   class literal constants
|#

(define-type Schml-Literal
  (U Integer Boolean))

(define (schml-literal? x)
  (or (and (integer? x) (>= x 0) (<= x (expt 2 64)))
      (boolean? x)))

(: schml-literal->type (Schml-Literal -> (U Bool Int)))
(define (schml-literal->type x)
  (if (boolean? x)
      BOOL-TYPE
      INT-TYPE))

;; Types in the schml languages
(define-type+ Schml-Type ([Schml-Type* Listof]
			  [Schml-Type? Option])
  (Rec ST (U Int Bool Dyn
             (GRef ST)
             (GVect ST)
             (MRef ST)
             (MVect ST)
             (Fn Index (Listof ST) ST))))

(define-type Schml-Fn-Type (Fn Index Schml-Type* Schml-Type))

(define-type Atomic-Schml-Type (U Int Bool Dyn))

(define-predicate schml-type? Schml-Type)

(define-type+ Schml-Fml ([Schml-Fml* Listof])
  (Fml Uid Schml-Type))

#|-----------------------------------------------------------------------------+
| Language/Schml0
+-----------------------------------------------------------------------------|#
(define-type Schml0-Lang (Prog (List String Natural) S0-Expr))

(define-type S0-Expr
  (Rec E (Ann (U (Lambda Schml-Fml* (Ann E (Option Schml-Type)))
		 (Letrec S0-Bnd* E)
		 (Let S0-Bnd* E)
		 (App E (Listof E))
		 (Op Schml-Prim (Listof E))
		 (If E E E)
		 (Ascribe E Schml-Type (Option Blame-Label))
		 (Var Uid)
		 (Quote Schml-Literal)
                 (Begin (Listof E) E)
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
              Src)))

(define-type S0-Expr* (Listof S0-Expr))
(define-type S0-Bnd (Bnd Uid Schml-Type? S0-Expr))
(define-type S0-Bnd* (Listof S0-Bnd))

#|-----------------------------------------------------------------------------+
| Language/Schml1
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
		 (Op (Ann Schml-Prim Schml-Type*) (Listof E))
		 (If E E E)
		 (Ascribe E Schml-Type (Option Blame-Label))
		 (Var Uid)
		 (Quote Schml-Literal)
                 (Begin (Listof E) E)
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

(: schml-prim->type
   (-> Schml-Prim (Fn Index (Listof (U Int Bool)) (U Int Bool))))
(define (schml-prim->type p)
  (cond
   [(IntxInt->Bool-Prim? p) INTxINT->BOOL-TYPE]
   [(IntxInt->Int-Prim? p)  INTxINT->INT-TYPE]))

(: consistent? (Schml-Type Schml-Type . -> . Boolean))
(define (consistent? t g)
  (or (Dyn? t) (Dyn? g)
      (and (Int? t) (Int? g))
      (and (Bool? t) (Bool? g))
      (and (Fn? t) (Fn? g)
	   (= (Fn-arity t) (Fn-arity g))
	   (andmap consistent? (Fn-fmls t) (Fn-fmls g))
	   (consistent? (Fn-ret t) (Fn-ret g)))
      (and (GRef? t) (GRef? g)
           (consistent? (GRef-arg t) (GRef-arg g)))
      (and (GVect? t) (GVect? g)
           (consistent? (GVect-arg t) (GVect-arg g)))
      (and (MRef? t) (MRef? g)
           (consistent? (MRef-arg t) (MRef-arg g)))
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
   [(Dyn? t) g]
   [(Dyn? g) t]
   [(and (Int? t) (Int? g)) INT-TYPE]
   [(and (Bool? t) (Bool? g)) BOOL-TYPE]
   [(and (Fn? t) (Fn? g) (= (Fn-arity t) (Fn-arity g)))
    (Fn (Fn-arity t)
	(map join (Fn-fmls t) (Fn-fmls g))
	(join (Fn-ret t) (Fn-ret g)))]
   [(and (GRef? t) (GRef? g))
    (join (GRef-arg t) (GRef-arg g))]
   [(and (GVect? t) (GVect? g))
    (join (GVect-arg t) (GVect-arg g))]
   [(and (MRef? t) (MRef? g))
    (join (MRef-arg t) (MRef-arg g))]
   [(and (MVect? t) (MVect? g))
    (join (MVect-arg t) (MVect-arg g))]
   [else (error 'join "Types are not consistent")]))

#|-----------------------------------------------------------------------------
We are going to UIL
-----------------------------------------------------------------------------|#

(define-type UIL-Prim  (U Schml-Prim Array-Prim))
(define-type UIL-Prim! (U Array-Prim! Print-Prim! Runtime-Prim!))

(define-type UIL-Expr-Prim (U Array-Prim IxI->I-Prim))

(define-type Array-Prim (U 'Alloc 'Array-ref))
(define-type Array-Prim! 'Array-set!)
(define-type Print-Prim! (U 'Printf 'Print))
(define-type Runtime-Prim! (U 'Exit))

(define-type (UIL-Op E) (Op UIL-Prim (Listof E)))
(define-type (UIL-Op! E) (Op UIL-Prim! (Listof E)))

(define-type Cast-Fml* (Listof Cast-Fml))
(define-type Cast-Fml (Fml Uid Schml-Type))

(define-type Cast-Literal (U Schml-Literal Blame-Label Schml-Type))

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
	  (UIL-Op E)
	  (If E E E)
	  (Cast E Schml-Type Schml-Type Blame-Label)
          (Begin C0-Expr* E)
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
	  (UIL-Op E)
	  (If E E E)
          ;; Terminals
          (Begin C1-Expr* E)
	  (Var Uid)
	  (Quote Cast-Literal)
          ;; Casts with different ways of getting the same semantics
	  (Runtime-Cast E E E E)
	  (Cast E Schml-Type Schml-Type Blame-Label)
	  (Fn-Cast E Schml-Type Schml-Type Blame-Label)
          ;; FN-Type operations
	  (Type-Fn-ref E (U Index 'arity 'return))
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
| Language/Cast2 created by introduce-castable-functions                       |
+-----------------------------------------------------------------------------|#

(define-type Cast2-Lang
 (Prog (List String Natural Schml-Type) C2-Expr))

(define-type C2-Expr
  (Rec E (U ;; Non-Terminals
	  (Lambda Uid* (Castable (Option Uid) E))
	  (Letrec C2-Bnd* E)
	  (Let C2-Bnd* E)
	  (App E (Listof E))
	  (UIL-Op E)
	  (If E E E)
          ;; Terminals
          (Begin C2-Expr* E)
	  (Var Uid)
	  (Quote Cast-Literal)
          ;; Casts with different ways of getting the same semantics
	  (Runtime-Cast E E E E)
	  (Cast E Schml-Type Schml-Type Blame-Label)
	  (Fn-Cast E Schml-Type Schml-Type Blame-Label)
          ;; FN-Type operations
	  (Type-Fn-ref E (U Index 'arity 'return))
          ;; Observations
          (Blame E)
          ;; Guarded Representation
          (GRep E))))

(define-type C2-Expr* (Listof C2-Expr))
(define-type C2-Bnd (Pairof Uid C2-Expr))
(define-type C2-Bnd* (Listof C2-Bnd))


(define-type (GRep A)
  (U (GRep-proxied? A)
     (UGbox A)
     (UGbox-set! A A)
     (UGbox-ref A)
     (Gproxy A A A A)
     (Gproxy-for A)
     (Gproxy-from A)
     (Gproxy-to A)
     (Gproxy-blames A)))

(define-type (GRep-Value A)
  (U (GRep-proxied? A)
     (UGbox A)
     (UGbox-ref A)
     (Gproxy A A A A)
     (Gproxy-for A)
     (Gproxy-from A)
     (Gproxy-to A)
     (Gproxy-blames A)))

(define-type (GRep-Effect A)
  (UGbox-set! A A))


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
	  (UIL-Op E)
	  (If E E E)
          (Begin C3-Expr* E)
          ;; closure operations
          (Fn-Caster E)
          ;; FN-Type operations
	  (Type-Fn-ref E (U Index 'arity 'return))
          (Type-tag E)
          ;; Dyn operations
          (Dyn-tag E)
          (Dyn-immediate E)
          (Dyn-ref E (U 'value 'type))
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

(define-type C3-Expr* (Listof C3-Expr))
(define-type C3-Bnd   (Pair Uid C3-Expr))
(define-type C3-Bnd*  (Listof C3-Bnd))

(define-type Tag-Symbol (U 'Int 'Bool 'Fn 'Atomic 'Boxed))


#|-----------------------------------------------------------------------------+
| Language/Cast4 created by normalize-context                                  |
+-----------------------------------------------------------------------------|#


(define-type Cast4-Lang
  (Prog (List String Natural Schml-Type) C4-Value))

(define-type C4-Value
  (Rec V (U
	  (Lambda Uid* (Castable (Option Uid) V))
	  (Letrec C4-Bnd* V)
	  (Let C4-Bnd* V)
	  (App V (Listof V))
	  (UIL-Op V)
	  (If V V V)
          (Begin C4-Effect* V)
          ;; closure operations
          (Fn-Caster V)
          ;; FN-Type operations
          (Type-Fn-arity V)
	  (Type-Fn-return V)
          (Type-Fn-arg V V)
          (Type-tag V)
          ;; Dyn operations
          (IDyn V V)
          (IDyn-tag V)
          (IDyn-immediate V)
          (BDyn V V)
          (BDyn-value V)
          (BDyn-type V)
          ;; Observational Operations
          (Blame V)
          (Observe V Schml-Type)
          ;; Terminals
          (Type Schml-Type)
          (Tag Tag-Symbol)
	  (Var Uid)
          (GRep-Value V)
	  (Quote Cast-Literal))))

(define-type C4-Effect
  (Rec E
   (U (Letrec C4-Bnd* E)
      (Let C4-Bnd* E)
      (Begin C4-Effect* No-Op)
      (App C4-Value C4-Value*)
      (If C4-Value E E)
      No-Op
      (GRep-Effect C4-Value))))

(define-type C4-Value* (Listof C4-Value))
(define-type C4-Effect* (Listof C4-Effect))
(define-type C4-Bnd   (Pair Uid C4-Value))
(define-type C4-Bnd*  (Listof C4-Bnd))


#|-----------------------------------------------------------------------------+
| The Constants for the representation of casts                                |
+-----------------------------------------------------------------------------|#
;; The Representation of functional types is an array
(define FN-ARITY-INDEX 0)
(define FN-RETURN-INDEX 1)
(define FN-FMLS-OFFSET 2)

;; The representation of Immediates for types
(define TYPE-TAG-MASK #b111)
(define TYPE-FN-TAG #b000)
(define TYPE-ATOMIC-TAG #b111)
(define TYPE-DYN-RT-VALUE #b0111)
(define TYPE-INT-RT-VALUE #b1111)
(define TYPE-BOOL-RT-VALUE #b10111)

;; The representation of Dynamic Immediates
(define DYN-TAG-MASK  #b111)
(define DYN-BOXED-TAG #b000)
(define DYN-INT-TAG   #b001)
(define DYN-BOOL-TAG  #b111)
(define DYN-IMDT-SHIFT 3)

;; Boxed Dynamics are just a cons cell
(define DYN-BOX-SIZE 2)
(define DYN-VALUE-INDEX 0)
(define DYN-TYPE-INDEX 1)

;; Immediates
(define FALSE-IMDT #b000)
(define TRUE-IMDT #b001)
;; Unreachable Value
(define UNDEF-IMDT 0)
(define UNDEF-IMDT-VALUE (Quote UNDEF-IMDT))

;; Guarded Representation
(define GREP-TAG-MASK #b111)
(define UGBOX-SIZE 1)
(define UGBOX-VALUE-INDEX 0)
(define GPROXY-TAG  #b001)
(define GPROXY-SIZE 4)
(define GPROXY-FOR-INDEX 0)
(define GPROXY-FROM-INDEX 1)
(define GPROXY-TO-INDEX 2)
(define GPROXY-BLAMES-INDEX 3)





#|-----------------------------------------------------------------------------+
| The Lambda Language Family Types, Primitives, Literals, and Terminals        |
+-----------------------------------------------------------------------------|#

(define-type Lambda-Literal (U Integer String))



#|-----------------------------------------------------------------------------+
| Language/Lambda0 produced by specify-cast-representation                     |
+-----------------------------------------------------------------------------|#

(define-type Lambda0-Lang
  (Prog (List String Natural Schml-Type) L0-Expr))

(define-type L0-Expr
  (Rec E (U (Lambda Uid* (Castable (Option Uid) E))
	    (Letrec L0-Bnd* E)
	    (Let L0-Bnd* E)
	    (App E (Listof E))
	    (UIL-Op E)
	    (If E E E)
	    (Begin L0-Stmt* E)
	    (Fn-Caster E)
	    ;; Terminals
	    (Var Uid)
	    (Quote Lambda-Literal))))

(define-type L0-Stmt (UIL-Op! L0-Expr))
(define-type L0-Stmt* (Listof L0-Stmt))
(define-type L0-Bnd (Pair Uid L0-Expr))
(define-type L0-Bnd* (Listof L0-Bnd))


#|-----------------------------------------------------------------------------+
| Language/Lambda1 created by label-lambdas                                    |
+-----------------------------------------------------------------------------|#

(define-type Lambda1-Lang (Prog (List String Natural Schml-Type) L1-Expr))

(define-type L1-Expr
  (Rec E (U  ;; Non terminals
	  (Letrec L1-Bnd-Lambda* E)
	  (Let L1-Bnd-Data* E)
	  (App E (Listof E))
	  (UIL-Op E)
	  (If E E E)
	  (Begin (Listof (UIL-Op! E)) E)
	  (Fn-Caster E)
	  ;; Terminals
	  Halt
	  (Var Uid)
	  (Quote Lambda-Literal))))

(define-type L1-Stmt (UIL-Op! L1-Expr))
(define-type L1-Stmt* (Listof L1-Stmt))
(define-type L1-Bnd-Lambda* (Listof L1-Bnd-Lambda))
(define-type L1-Bnd-Lambda  (Pairof Uid L1-Lambda))
(define-type L1-Bnd-Data* (Listof L1-Bnd-Data))
(define-type L1-Bnd-Data  (Pairof Uid L1-Expr))

(define-type L1-Lambda
  (Lambda Uid* (Castable (Option Uid) L1-Expr)))


#|-----------------------------------------------------------------------------+
| Language/Lambda2 created by uncover-free                                     |
+-----------------------------------------------------------------------------|#
(define-type Lambda2-Lang (Prog (List String Natural Schml-Type) L2-Expr))

(define-type L2-Expr
  (Rec E (U ;; Non-Terminal
	  (Letrec L2-Bnd-Lambda* E)
	  (Let L2-Bnd-Data* E)
	  (App E (Listof E))
	  (UIL-Op E)
	  (If E E E)
	  (Begin L2-Stmt* E)
	  (Fn-Caster E)
	  Halt
	  (Var Uid)
	  (Quote Lambda-Literal))))

(define-type L2-Bnd-Lambda* (Listof L2-Bnd-Lambda))
(define-type L2-Bnd-Lambda (Pairof Uid L2-Lambda))
(define-type L2-Bnd-Data* (Listof L2-Bnd-Data))
(define-type L2-Bnd-Data (Pairof Uid L2-Expr))
(define-type L2-Stmt (UIL-Op! L2-Expr))
(define-type L2-Stmt* (Listof L2-Stmt))

(define-type L2-Lambda
  (Lambda Uid* (Free (Option Uid) (Listof Uid) L2-Expr)))


#|-----------------------------------------------------------------------------+
| Language/Lambda3 created by convert-closures                                 |
+-----------------------------------------------------------------------------|#
;; Intermediate language were all lambda have no free variables
;; they are explicitly passed as a structure and implicitly extracted by procedures

(define-type Lambda3-Lang
  (Prog (List String Natural Schml-Type) L3-Expr))

(define-type L3-Expr
  (Rec E (U ;; Non Terminals
	 (LetP L3-Bnd-Procedure* (LetC L3-Bnd-Closure* E))
	 (Let L3-Bnd-Data* E)
	 (App (Pair (Var Uid) (Var Uid)) (Listof E))
	 (UIL-Op E)
	 (If E E E)
	 (Begin L3-Stmt* E)
	 (Fn-Caster E)
	 ;; Terminals
	 Halt
	 (Var Uid)
	 (Quote Lambda-Literal))))

(define-type L3-Procedure
  (Procedure Uid Uid* (Option Uid) Uid* L3-Expr))
(define-type L3-Closure (Closure-Data Uid (Option Uid) (Listof Uid)))
(define-type L3-Bnd-Procedure (Pairof Uid L3-Procedure))
(define-type L3-Bnd-Procedure* (Listof L3-Bnd-Procedure))
(define-type L3-Bnd-Closure (Pairof Uid L3-Closure))
(define-type L3-Bnd-Closure* (Listof L3-Bnd-Closure))
(define-type L3-Bnd-Data (Pairof Uid L3-Expr))
(define-type L3-Bnd-Data* (Listof L3-Bnd-Data))
(define-type L3-Stmt (UIL-Op! L3-Expr))
(define-type L3-Stmt* (Listof L3-Stmt))

#|-----------------------------------------------------------------------------+
| Language/Lambda4 created by specify-representation                           |
+-----------------------------------------------------------------------------|#
;; procedures are now just routines that have an explicit
;; layout for parameters
(define-type Lambda4-Lang
  (Prog (List String Natural Schml-Type) L4-Expr))


(define-type L4-Expr
  (Rec E
       (U (Labels L4-Bnd-Code* (Let L4-Bnd-Data* E))
	  (Let L4-Bnd-Data* E)
	  (App E (Listof E))
	  (UIL-Op E)
	  (If E E E)
	  (Begin L4-Stmt* E)
	  ;; Terminals
          Halt
	  (Code-Label Uid)
	  (Var Uid)
	  (Quote Lambda-Literal))))

(define-type L4-Expr* (Listof L4-Expr))

(define-type L4-Bnd-Code* (Listof L4-Bnd-Code))
(define-type L4-Bnd-Code (Pairof Uid L4-Code))
(define-type L4-Code (Code Uid* L4-Expr))
(define-type L4-Bnd-Data* (Listof L4-Bnd-Data))
(define-type L4-Bnd-Data  (Pairof Uid L4-Expr))
(define-type L4-Stmt (UIL-Op! L4-Expr))
(define-type L4-Stmt* (Listof L4-Stmt))


;; The representation of closures is that they are arrays containing their
;; free variables.

(define CLOS-CODE-INDEX 0)
(define CLOS-CSTR-INDEX 1)
(define CLOS-FVAR-OFFSET 2)


#|-----------------------------------------------------------------------------+
| Data0-Language created by make-closures-explicit                           |
+-----------------------------------------------------------------------------|#



(define-type Data0-Lang
  (Prog (List String Natural Schml-Type)
	(Labels D0-Bnd-Code*
		D0-Expr)))

(define-type D0-Bnd-Code* (Listof D0-Bnd-Code))
(define-type D0-Bnd-Code (Pairof Uid D0-Code))
(define-type D0-Code (Code Uid* D0-Expr))

(define-type D0-Expr
  (Rec E (U (Let D0-Bnd* E)
	    (App E (Listof E))
	    (UIL-Op E)
	    (If E E E)
	    (Begin D0-Stmt* E)
	    Halt
	    (Var Uid)
	    (Code-Label Uid)
	    (Quote D0-Literal))))

(define-type D0-Expr* (Listof D0-Expr))

(define-type D0-Stmt (UIL-Op! D0-Expr))
(define-type D0-Stmt* (Listof D0-Stmt))

(define-type D0-Bnd* (Listof D0-Bnd))
(define-type D0-Bnd  (Pairof Uid D0-Expr))

(define-type D0-Literal Lambda-Literal)

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
      (Begin D1-Stmt* T)
      (Return D1-Expr))))

(define-type D1-Expr
 (Rec E
  (U (Let D1-Bnd* E)
     (If D1-Pred E E)
     (Begin D1-Stmt* E)
     (App E (Listof E))
     (Op (U IxI->I-Prim Array-Prim) (Listof E))
     Halt
     (Var Uid)
     (Code-Label Uid)
     (Quote D1-Literal))))

(define-type D1-Expr* (Listof D1-Expr))

(define-type D1-Pred
 (Rec P
  (U (Let D1-Bnd* P)
     (If D1-Pred P P)
     (Begin D1-Stmt* P)
     (Relop IxI->B-Prim D1-Expr D1-Expr))))

(define-type D1-Stmt (UIL-Op! D1-Expr))

(define-type D1-Stmt* (Listof D1-Stmt))

(define-type D1-Bnd  (Pairof Uid D1-Expr))

(define-type D1-Bnd* (Listof D1-Bnd))

(define-type D1-Literal Lambda-Literal)

#|-----------------------------------------------------------------------------+
| Data2-Language created by remove-let                                          |
+-----------------------------------------------------------------------------|#

(define-type Data2-Lang
  (Prog (List String Natural Schml-Type)
	(Labels D2-Bnd-Code*
		D2-Body)))

(define-type D2-Bnd-Code* (Listof D2-Bnd-Code))
(define-type D2-Bnd-Code (Pairof Uid D2-Code))
(define-type D2-Code (Code Uid* D2-Body))

(define-type D2-Body (Locals Uid* D2-Tail))

(define-type D2-Tail
  (Rec T
   (U (Begin D2-Stmt* T)
      (If D2-Pred T T)
      (Return D2-Expr))))

(define-type D2-Expr
 (Rec E
  (U (If D2-Pred E E)
     (Begin D2-Stmt* E)
     (App E (Listof E))
     (Op (U IxI->I-Prim Array-Prim) (Listof E))
     Halt
     (Var Uid)
     (Code-Label Uid)
     (Quote D2-Literal))))

(define-type D2-Expr* (Listof D2-Expr))

(define-type D2-Pred
 (Rec P
  (U (If P P P)
     (Begin D2-Stmt* P)
     (App D2-Expr (Listof D2-Expr))
     (Relop IxI->B-Prim D2-Expr D2-Expr))))

(define-type D2-Stmt
  (U (UIL-Op! D2-Expr)
     (Assign Uid D2-Expr)))

(define-type D2-Stmt* (Listof D2-Stmt))

(define-type D2-Literal Lambda-Literal)

#|-----------------------------------------------------------------------------+
| Data3-Language created by purify-expression-context                          |
+-----------------------------------------------------------------------------|#

(define-type Data3-Lang
  (Prog (List String Natural Schml-Type)
	(Labels D3-Bnd-Code*
		D3-Body)))

(define-type D3-Bnd-Code* (Listof D3-Bnd-Code))
(define-type D3-Bnd-Code (Pairof Uid D3-Code))
(define-type D3-Code (Code Uid* D3-Body))

(define-type D3-Body (Locals Uid* D3-Tail))

(define-type D3-Tail
  (Rec T
   (U (Begin D3-Stmt* T)
      (If D3-Pred T T)
      (Return D3-Expr))))

(define-type D3-Expr
 (Rec E
  (U (If D3-Pred E E)
     (App E (Listof E))
     (UIL-Op E)
     Halt
     (Var Uid)
     (Code-Label Uid)
     (Quote D3-Literal))))

(define-type D3-Pred
 (Rec P
  (U (App D3-Expr (Listof D3-Expr))
     (Relop IxI->B-Prim D3-Expr D3-Expr))))

(define-type D3-Expr* (Listof D3-Expr))
(define-type D3-Stmt (UIL-Op! D3-Expr))
(define-type D3-Stmt* (Listof D3-Stmt))
(define-type D3-Literal Lambda-Literal)
