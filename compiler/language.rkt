#lang typed/racket
(require schml/framework/helpers)
(provide (all-defined-out))
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
  (Lambda formals return-type body)
  (Procedure this params caster bound-vars body)
  ;; recursive binding 
  (Letrec bindings body)
  ;; non recursive binding
  (Let bindings body)
  (App operator operands)
  (Op operator operands)
  (Var id)
  (If test then else)
  (Ascribe expression type label)
  ;; various imediates markers
  (Quote literal)    ;; imediate data in general
  (Code-Label value) ;; marks a uid as refering to a uid
  (Tag bits)         ;; an tag for an imediate value
  (Type type)        ;; an atomic type
  ;; Effectfull expressions
  (Begin effects value)
  (Nop)
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
  ;; represents a set of moves to initialize variables before 
  (Code variables body)
  (Closure-Data code caster variables)
  (Halt)
  (Assign lhs rhs)
  (Locals names body)
  (Return value)
  (Relop primitive expression1 expression2)
  ;; Uil Memory Ops
  (Malloc expression)
  (Mref expression1 expression2)
  (Mset expression1 expression2 expression3)
  ;; Uil IO Primitives
  (Printf format expressions)
  (Print expression)
  (BinOp primitive expression1 expression2))

(define NO-OP (Nop))

#| Types throughout the languages |#

;; Schml types
(define-forms 
  (Int)
  (Bool)
  (Dyn)
  (Fn arity fmls ret) 
  (String-Ptr) 
  (Any-Value)
  (Any-Type)
  (Void-Type)
  (Bottom-Type))

;;Constants for the types
(define INT-TYPE (Int))
(define BOOL-TYPE (Bool))
(define DYN-TYPE (Dyn))
(define ANY-TYPE (Any-Type))
(define STRING-PTR (String-Ptr))
(define ANY-VALUE (Any-Value))
(define BOTTOM-TYPE (Bottom-Type))
(define VOID-TYPE (Void-Type))
(define INTxINT-TYPE (list INT-TYPE INT-TYPE))
(define INTxINT->BOOL-TYPE (Fn 2 INTxINT-TYPE BOOL-TYPE))
(define INTxINT->INT-TYPE (Fn 2 INTxINT-TYPE INT-TYPE))


(define (shallow-consistent? t g)
  (or (Dyn? t)
      (Dyn? g)
      (and (Int? t) (Int? g))
      (and (Bool? t) (Bool? g))
      (and (Fn? t) (Fn? g))))

#| Unique Identifier |#
(struct Uid ([prefix : String] [suffix : Natural]) #:transparent)
(define-type Uid* (Listof Uid))

(define (uid=? [u : Uid] [v : Uid])
  (= (Uid-suffix u) (Uid-suffix v)))

(define-syntax next-uid
  (syntax-rules ()
    [(_ prefix suffix) (values (Uid prefix suffix) (add1 suffix))]))

(define-syntax let-uid*
  (syntax-rules ()
    [(_ (wrong ...) others ...) (raise-syntax-error 'let-uid "next must always be an identifier")]
    [(_ next () body ...)(let () body ...)]
    [(_ next ([name0 prefix0] [name* prefix*] ...) body ...)
     (let-values ([(name0 next) (next-uid prefix0 next)])
       (let-uid* next ([name* prefix*] ...)
		 body ...))]))

(define FIRST-UID-SUFFIX 0)

#| Maybe type |#
(define-type Src srcloc)

#|-----------------------------------------------------------------------------+
| Language/Schml-Syntax
+-----------------------------------------------------------------------------|#
;; The language created by schml/read
(define-type Syntax-Lang (Prog String (Listof Stx)))

(define-type Stx (Syntaxof Any))

#|-----------------------------------------------------------------------------+
| Types shared by the Schml language family
+-----------------------------------------------------------------------------|#
(define-type Schml-Prim 
  (U IntxInt->Int-Primitives IntxInt->Bool-Primitives))

(define-predicate Schml-Prim? Schml-Prim)

(define-type IntxInt->Int-Primitives
  (U '* '+ '- 'binary-and 'binary-or '<< '>>))

(define-type IxI->I-Prim IntxInt->Int-Primitives)

(define-type IntxInt->Bool-Primitives
  (U '< '<= '= '> '>=))

(define-type IxI->B-Prim IntxInt->Bool-Primitives)

(define-predicate IntxInt->Bool-Prim? IntxInt->Bool-Primitives)

(define-predicate IntxInt->Int-Prim? IntxInt->Int-Primitives)

;; Literals of the schml languages
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
  (Rec CT (U Int Bool Dyn (Fn Index (Listof Schml-Type) Schml-Type))))

(define-type Atomic-Schml-Type (U Int Bool Dyn))

(define-predicate schml-type? Schml-Type)

(define-type+ Schml-Fml ([Schml-Fml* Listof])
  (Fml Uid Schml-Type))

#|-----------------------------------------------------------------------------+
| Language/Schml0
+-----------------------------------------------------------------------------|#
(define-type Schml0-Lang (Prog (List String Natural) S0-Expr))

(define-type S0-Expr
  (Rec E (Ann (U (Lambda Schml-Fml* (Option Schml-Type) E)
		 (Letrec S0-Bnd* E)
		 (Let S0-Bnd* E)
		 (App E (Listof E))
		 (Op Schml-Prim (Listof E))
		 (If E E E)
		 (Ascribe E Schml-Type (Option String))
		 (Var Uid)
		 (Quote Schml-Literal))
	      Src)))


(define-type S0-Bnd (Bnd Uid Schml-Type? S0-Expr))
(define-type S0-Bnd* (Listof S0-Bnd))

#|-----------------------------------------------------------------------------+
| Language/Schml1
+-----------------------------------------------------------------------------|#
(define-type Schml1-Lang
  (Prog (List String Natural Schml-Type) S1-Expr))

(define-type S1-Expr
  (Rec E (Ann (U (Lambda Schml-Fml* Schml-Type E)
		 (Letrec S1-Bnd* E)
		 (Let S1-Bnd* E)
		 (App E (Listof E))
		 (Op (Ann Schml-Prim Schml-Type*) (Listof E))
		 (If E E E)
		 (Ascribe E Schml-Type (Option String))
		 (Var Uid)
		 (Quote Schml-Literal))
	      (Pair Src Schml-Type))))

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
	   (consistent? (Fn-ret t) (Fn-ret g)))))

;; Join :: type X type -> type
;; This is the join of the least precise latice
;;     ⊑ latice example:
;;      Int --> Int
;;         /   \
;;        /     \               
;;       /       \        Joins ↑
;;Dyn --> Int Int --> Dyn    
;;       \       /        Meets ↓
;;        \     /  
;;         \   /
;;      Dyn --> Dyn
;;           |
;;          Dyn

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
   [else (error 'join "Types are not consistent")]))

#|-----------------------------------------------------------------------------
We are going to UIL
-----------------------------------------------------------------------------|#

(define-type UIL-Prim  (U Schml-Prim UIL-Primitive))
(define-type UIL-Prim! (U UIL-Primitive!))


(define-type UIL-Primitive (U 'Alloc 'Array-ref))
(define-type UIL-Primitive! (U 'Print 'Printf 'Array-set!))

(define-type (UIL-Op E) (Op UIL-Prim (Listof E)))
(define-type (UIL-Op! E) (Op UIL-Prim! (Listof E)))

(define-type Cast-Fml* (Listof Cast-Fml))
(define-type Cast-Fml (Fml Uid Schml-Type))

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
	  (Lambda Cast-Fml* Schml-Type E)
	  (Letrec C0-Bnd* E)
	  (Let C0-Bnd* E)
	  (App E (Listof E))
	  (UIL-Op E)
	  (If E E E)
	  (Cast E Schml-Type Schml-Type String)
	  ;; Terminals
	  (Var Uid) 
	  (Quote Schml-Literal))))


(define-type C0-Expr* (Listof C0-Expr))
(define-type C0-Bnd (Bnd Uid Schml-Type C0-Expr))
(define-type C0-Bnd* (Listof C0-Bnd))

(define-type Cast-Literal (U Schml-Literal String Schml-Type))


#|-----------------------------------------------------------------------------+
| Language/Cast1 created by introduce-castable-functions                       |
+-----------------------------------------------------------------------------|#

(define-type Cast1-Lang 
 (Prog (List String Natural Schml-Type) C1-Expr))

(define-type C1-Expr
  (Rec E (U ;; Non-Terminals
	  (Lambda Uid* False (Castable (Option Uid) E))
	  (Letrec C1-Bnd* E)
	  (Let C1-Bnd* E)
	  (App E (Listof E))
	  (UIL-Op E)
	  (If E E E)
          ;; Casts with different ways of getting the same semantics
	  (Runtime-Cast E E E E)
	  (Cast E Schml-Type Schml-Type String)
	  (Fn-Cast E Schml-Type Schml-Type String)
          ;; FN-Type operations
	  (Type-Fn-ref E (U Index 'arity 'return))
          ;; Observations
          (Blame E)
          ;; Terminals 
	  (Var Uid) 
	  (Quote Schml-Literal))))

(define-type C1-Expr* (Listof C1-Expr))
(define-type C1-Bnd (Pairof Uid C1-Expr))
(define-type C1-Bnd* (Listof C1-Bnd))

#|-----------------------------------------------------------------------------+
| Language/Cast2 created by introduce-castable-functions                       |
+-----------------------------------------------------------------------------|#


(define-type Cast2-Lang 
  (Prog (List String Natural Schml-Type) C2-Expr))

(define-type C2-Expr
  (Rec E (U ;; Non-Terminals
	  (Lambda Uid* False (Castable (Option Uid) E))
	  (Letrec C2-Bnd* E)
	  (Let C2-Bnd* E)
	  (App E (Listof E))
	  (UIL-Op E)
	  (If E E E)
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
	  (Quote (U Schml-Literal String)))))

(define-type C2-Expr* (Listof C2-Expr))
(define-type C2-Bnd (Pair Uid C2-Expr))
(define-type C2-Bnd* (Listof C2-Bnd))
(define-type Tag-Symbol (U 'Int 'Bool 'Fn 'Atomic 'Boxed))

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
  (Rec E (U (Lambda Uid* False (Castable (Option Uid) E))
	    (Letrec L0-Bnd* E)
	    (Let L0-Bnd* E)
	    (App E (Listof E))
	    (UIL-Op E)
	    (If E E E)
	    (Begin (Listof L0-Stmt) E)
	    (Fn-Caster E)
	    ;; Terminals 
            Halt
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
  (Lambda Uid* False (Castable (Option Uid) L1-Expr)))


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
  (Lambda Uid* False (Free (Option Uid) (Listof Uid) L2-Expr)))


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
     (UIL-Op E)
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

(define-type D1-Stmt (UIL-OP! D1-Expr))

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
     (UIL-Op E)
     Halt
     (Var Uid)
     (Code-Label Uid)
     (Quote D2-Literal))))

(define-type D2-Pred
 (Rec P
  (U (If P P P) 
     (Begin D2-Stmt* P)
     (App D2-Expr (Listof D2-Expr))
     (Relop IxI->B-Prim D2-Expr))))

(define-type D2-Expr* (Listof D2-Expr))
(define-type D2-Stmt (UIL-Op! D2-Expr))
(define-type D2-Stmt* (Listof D2-Stmt))

(define-type D2-Literal Lambda-Literal)

#|-----------------------------------------------------------------------------+
| Data3-Language created by purify-expression-context                                         |
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
     (Relop IxI->B-Prim D3-Expr))))

(define-type D3-Expr* (Listof D3-Expr))
(define-type D3-Stmt (UIL-Op! D3-Expr))
(define-type D3-Stmt* (Listof D3-Stmt))
(define-type D3-Literal Lambda-Literal)



#|-----------------------------------------------------------------------------+
| UIL0-Language created by remove-let                                          |
+-----------------------------------------------------------------------------|#
