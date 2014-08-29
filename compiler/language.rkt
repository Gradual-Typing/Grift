#lang typed/racket
(require Schml/framework/helpers)
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
  (Letrec bindings body)
  (Let bindings body)
  (App operator operands)
  (Op operator operands)
  (Var id)
  (If test then else)
  (Ascribe expression type label)
  (Quote literal)
  (Begin effects value)
  (Nop)
  (Fml identifier type)
  (Bnd identifier type expression)
  (Cast expression type-exp type-cast label)
  (When/blame who test then)
  (LetP bindings body)
  (LetC bindings body)
  (Labels bindings body)
  (Castable caster body)
  (Bound closure variables body)
  (Procedure this params caster bound-vars body)
  (Code-Label value)
  (Code variables body)
  (Free caster variables body)
  (Closure-Data code caster variables))

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

(define-type IntxInt->Bool-Primitives
  (U '< '<= '= '> '>=))

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

#|-----------------------------------------------------------------------------+
| The Cast Language Family Types, Primitives, Literals, and Terminals        |
+-----------------------------------------------------------------------------|#
(define-type Cast-Literal 
  (U Schml-Literal String Schml-Type))


(define-type Cast-Prim 
  (U Schml-Prim Type-Primitives Dyn-Primitives 'Fn-cast))

(define-type Type-Primitives
  (U 'Type:Int?
     'Type:Dyn? 
     'Type:Bool? 
     'Type:Fn? 'Type:Fn-arity 'Type:Fn-return 'Type:Fn-make 'Type:Fn-arg-ref
     'Blame))

(define-type Dyn-Primitives
  (U 'Dyn:Int? 'Dyn:Int-make 'Dyn:Int-value  
     'Dyn:Bool? 'Dyn:Bool-make 'Dyn:Bool-value 
     'Dyn:Fn?  'Dyn:Fn-make 'Dyn:Fn-value 'Dyn:Fn-type))

(define-type Cast-Type
  (Rec CT (U Dyn 
	     Int 
	     Bool 
	     Any-Type 
	     Any-Value 
	     String-Ptr 
	     Bottom-Type 
	     Void-Type 
	     (Fn Index (Listof CT) CT))))

(define-type Cast-Fml* (Listof Cast-Fml))
(define-type Cast-Fml (Fml Uid Cast-Type))

#|-----------------------------------------------------------------------------+
| Language/Cast0 created by insert-implicit-casts                              |
+-----------------------------------------------------------------------------
| Description: At the begining of this section of the compiler all cast in the |
| ast are performed on known schml language types. But as the compiler imposes |
| the semantics of cast there become situations where a type is dependant on   |
| contents of a variable. At this point casts are no longer able to be         |
| completely compiled into primitives. These casts require a sort of cast      |
| interpreter which is built later.                                            |
| In general this compiler tries to move as mainy casts into the primitive     |
| operations. Whatever casts are left at the end are then convert to           |
| applications of the cast interpreter function.
+-----------------------------------------------------------------------------|#

(define-type Cast0-Lang (Prog (List String Natural Schml-Type) C0-Expr))

(define-type C0-Expr
  (Rec E (U ;; Non-Terminals
	  (Lambda Cast-Fml* Cast-Type E)
	  (Letrec C0-Bnd* E)
	  (Let C0-Bnd* E)
	  (App E (Listof E))
	  (Op Cast-Prim (Listof E))
	  (If E E E)
	  (Cast E Schml-Type Schml-Type String)
	  ;; Terminals 
	  (Var Uid) 
	  (Quote Cast-Literal))))

(define-type C0-Expr* (Listof C0-Expr))
(define-type C0-Bnd (Bnd Uid Cast-Type C0-Expr))
(define-type C0-Bnd* (Listof C0-Bnd))

#|-----------------------------------------------------------------------------+
| Language/Cast1 created by introduce-castable-functions                       |
+-----------------------------------------------------------------------------|#

(define-type Cast1-Lang (Prog (List String Natural Schml-Type) C1-Expr))

(define-type C1-Expr
  (Rec E (U ;; Non-Terminals
	  (Lambda Cast-Fml* Cast-Type (Castable (Option Uid) E))
	  (Letrec C1-Bnd* E)
	  (Let C1-Bnd* E)
	  (App E (Listof E))
	  (Op Cast-Prim (Listof E))
	  (If E E E)
	  (Cast E E E E)
	  ;; Terminals 
	  (Var Uid) 
	  (Quote Cast-Literal))))

(define-type C1-Expr* (Listof C1-Expr))
(define-type C1-Bnd (Bnd Uid Cast-Type C1-Expr))
(define-type C1-Bnd* (Listof C1-Bnd))

#|-----------------------------------------------------------------------------+
| The Lambda Language Family Types, Primitives, Literals, and Terminals        |
+-----------------------------------------------------------------------------|#
(define-type Lambda-Prim
  (U Cast-Prim Closure-Primitives))

(define-type Closure-Primitives
  (U 'Clos:make 'Clos:ref))

(define-type Lambda-Prim!
  (U 'Clos:set!))

(define-type Lambda-Literal Cast-Literal)

(define-type Lambda-Type Cast-Type)

#|-----------------------------------------------------------------------------+
| Language/Lambda0 created by interpret-casts                                  |
+-----------------------------------------------------------------------------|#

(define-type Lambda0-Lang 
  (Prog (List String Natural Schml-Type) L0-Expr))

(define-type L0-Expr
  (Rec E (U (Lambda Uid* False (Castable (Option Uid) E))
	    (Letrec L0-Bnd* E)
	    (Let L0-Bnd* E)
	    (App E (Listof E))
	    (Op Lambda-Prim (Listof E))
	    (If E E E)
	    ;; Terminals
	    (Var Uid) 
	    (Quote Lambda-Literal))))

(define-type L0-Bnd (Pair Uid L0-Expr))
(define-type L0-Bnd* (Listof L0-Bnd))

#|-----------------------------------------------------------------------------+
| Language/Lambda1 created                                                     |
+-----------------------------------------------------------------------------|#

(define-type Lambda1-Lang (Prog (List String Natural Schml-Type) L1-Expr))

(define-type L1-Expr
  (Rec E (U  ;; Non terminals 
	  (Letrec L1-Bnd-Lambda* E)
	  (Let L1-Bnd-Data* E)
	  (App E (Listof E))
	  (Op Lambda-Prim (Listof E))
	  (If E E E)
	  ;; Terminals 
	  (Var Uid)
	  (Quote Lambda-Literal))))

(define-type L1-Bnd-Lambda* (Listof L1-Bnd-Lambda))
(define-type L1-Bnd-Lambda  (Pairof Uid L1-Lambda))
(define-type L1-Bnd-Data* (Listof L1-Bnd-Data))
(define-type L1-Bnd-Data  (Pairof Uid L1-Expr))

(define-type L1-Lambda
  (Lambda Uid* False (Castable (Option Uid) L1-Expr)))


#|-----------------------------------------------------------------------------+
| Language/Lambda2 created                                                     |
+-----------------------------------------------------------------------------|#
(define-type Lambda2-Lang (Prog (List String Natural Schml-Type) L2-Expr))

(define-type L2-Expr
  (Rec E (U ;; Non-Terminal 
	  (Letrec L2-Bnd-Lambda* E)
	  (Let L2-Bnd-Data* E)
	  (App E (Listof E))
	  (Op Lambda-Prim (Listof E))
	  (If E E E)
	  (Var Uid)
	  (Quote Lambda-Literal))))

(define-type L2-Bnd-Lambda* (Listof L2-Bnd-Lambda))
(define-type L2-Bnd-Lambda (Pairof Uid L2-Lambda))
(define-type L2-Bnd-Data* (Listof L2-Bnd-Data))
(define-type L2-Bnd-Data (Pairof Uid L2-Expr))

(define-type L2-Lambda
  (Lambda Uid* False (Free (Option Uid) (Listof Uid) L2-Expr)))


#|-----------------------------------------------------------------------------+
| Language/Lambda3 created                                                     |
+-----------------------------------------------------------------------------|#
;; Intermediate language were all lambda have no free variables
;; they are explicitly passed as a structure and implicitly extracted by procedures

(define-type Lambda3-Lang 
  (Prog (List String Natural Lambda-Type) L3-Expr))

(define-type L3-Expr
  (Rec E (U ;; Non Terminals 
	 (LetP L3-Bnd-Procedure* (LetC L3-Bnd-Closure* E))
	 (Let L3-Bnd-Data* E)
	 (App (Pair (Var Uid) (Var Uid)) (Listof E))
	 (Op Lambda-Prim (Listof E))
	 (If E E E)
	 ;; Terminals
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

;; procedures are now just routines that have an explicit
;; layout for parameters
(define-type Lambda4-Lang
  (Prog (List String Natural Lambda-Type) L4-Expr))


(define-type L4-Expr
  (Rec E 
       (U (Labels L4-Bnd-Code* (Let L4-Bnd-Data* (Begin L4-Effect* E)))
	  (Let L4-Bnd-Data* E)
	  (App E (Listof E))
	  (Op Lambda-Prim (Listof E))
	  (If E E E)
	  ;; Terminals
	  (Code-Label Uid)
	  (Var Uid)
	  (Quote Lambda-Literal))))

(define-type L4-Expr* (Listof L4-Expr))
(define-type L4-Effect 
  (U Nop
     (Op Lambda-Prim! L4-Expr*)))
(define-type L4-Effect* (Listof L4-Effect))

(define-type L4-Bnd-Code* (Listof L4-Bnd-Code))
(define-type L4-Bnd-Code (Pairof Uid L4-Code))
(define-type L4-Code (Code Uid* L4-Expr))
(define-type L4-Bnd-Data* (Listof L4-Bnd-Data))
(define-type L4-Bnd-Data  (Pairof Uid L4-Expr))
#|-----------------------------------------------------------------------------+
| The Data Language Family Types, Primitives, Literals, and Terminals        |
+-----------------------------------------------------------------------------|#
(define-type Data-Prim 
  (U Schml-Prim Closure-Primitives Type-Primitives Dyn-Primitives))

(define-type Data-Prim! (U Lambda-Prim!))

(define-type Data-Literal Lambda-Literal)

(define-type Data-Type Lambda-Type)

#|-----------------------------------------------------------------------------+
| Data1-Language created by make-closures-explicit                           |
+-----------------------------------------------------------------------------|#

(define-type Data0-Lang
  (Prog (List String Natural Data-Type) 
	(Labels D0-Bnd-Code*
		D0-Expr)))

(define-type D0-Bnd-Code* (Listof D0-Bnd-Code))
(define-type D0-Bnd-Code (Pairof Uid D0-Code))
(define-type D0-Code (Code Uid* D0-Expr))

(define-type D0-Expr
  (Rec E (U (Let D0-Bnd* E)
	    (App E (Listof E))
	    (Op Data-Prim (Listof E))
	    (If E E E)
	    (Begin D0-Effect* E)
	    (Var Uid)
	    (Code-Label Uid)
	    (Quote Data-Literal))))
(define-type D0-Expr* (Listof D0-Expr))

(define-type D0-Effect (Op Data-Prim! D0-Expr*))
(define-type D0-Effect* (Listof D0-Effect))

(define-type D0-Bnd* (Listof D0-Bnd))
(define-type D0-Bnd  (Pairof Uid D0-Expr))
