#lang typed/racket
(provide (all-defined-out))
#| 
A language form ends up just being a polymorphic record.
This allows me to make very desciptive grammars via types later on.
|#

(define-syntax (define-forms stx)
  (syntax-case stx ()
    [(_ (name fields ...) f* ...)
     (with-syntax ([(types ...) (generate-temporaries #'(fields ...))])
       #'(begin 
	   (struct (types ...) name ([fields : types] ...) #:transparent)
	   (define-forms f* ...)))]
    [(_) #'(void)]))

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
  (Fml identifier type)
  (Bnd identifier type expression)
  (Cast expression type-exp type-cast label)
  (When/blame who test then)
  (LetP bindings body)
  (LetC bindings body)
  (Bound closure variables body)
  (Code-Label value)
  (Code variables body)
  (Free variables body)
  (Closure-data code variables))

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
  (Void)
  (Bottom))

;;Constants for the types
(define INT-TYPE (Int))
(define BOOL-TYPE (Bool))
(define DYN-TYPE (Dyn))
(define ANY-TYPE (Any-Type))
(define STRING-PTR (String-Ptr))
(define ANY-VALUE (Any-Value))
(define BOTTOM-TYPE (Bottom))
(define VOID-TYPE (Void))
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


#| Maybe type |#
(define-type Src srcloc)

#|-----------------------------------------------------------------------------+
| Language/Schml-Syntax
+-----------------------------------------------------------------------------|#
;; The language created by schml/read
(define-type Syntax-Lang (Prog String (Listof Stx)))

(define-type Stx (Syntaxof Any))

#|-----------------------------------------------------------------------------+
| Language/Schml
+-----------------------------------------------------------------------------|#
(define-type Schml0-Lang (Prog (List String Natural) S0-Expr))

(define-type S0-Expr
  (Rec E (Ann (U (Lambda S0-Fml* (Option Schml-Type) E)
		  (Letrec S0-Bnd* E)
		  (Let S0-Bnd* E)
		  (App E (Listof E))
		  (Op Schml-Prim (Listof E))
		  (If E E E)
		  (Ascribe E Schml-Type (Option String))
		  (Var Uid)
		  (Quote Schml-Literal))
	       Src)))

(define-type S0-Fml (Fml Uid Schml-Type))
(define-type S0-Fml* (Listof S0-Fml))
(define-type S0-Bnd (Bnd Uid (Option Schml-Type) S0-Expr))
(define-type S0-Bnd* (Listof S0-Bnd))

;; primitives of the Schml languages
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

(define (Schml-Literal? x)
  (or (and (integer? x) (>= x 0) (<= x (expt 2 64)))
      (boolean? x)))
	   
(: schml-literal->type (Schml-Literal -> (U Bool Int)))
(define (schml-literal->type x)
  (if (boolean? x)
      BOOL-TYPE
      INT-TYPE))

;; Types in the schml languages
(define-type Schml-Type
  (Rec CT (U Int Bool Dyn (Fn Natural (Listof Schml-Type) Schml-Type))))

#|-----------------------------------------------------------------------------+
| Language/Schml1
+-----------------------------------------------------------------------------|#
(define-type Schml1-Lang
  (Prog (List String Natural Schml-Type) S1-Expr))

(define-type S1-Expr
  (Rec E (Ann (U (Lambda S0-Fml* (Option Schml-Type) E)
		 (Letrec S0-Bnd* E)
		 (Let S0-Bnd* E)
		 (App E (Listof E))
		 (Op Schml-Prim (Listof E))
		 (If E E E)
		 (Ascribe E Schml-Type (Option String))
		 (Var Uid)
		 (Quote Schml-Literal))
	      (Pair Src Schml-Type))))

(define-type S1-Fml S0-Fml)
(define-type S1-Fml* S0-Fml*)
(define-type S1-Bnd (Bnd Uid Schml-Type S1-Expr))
(define-type S1-Bnding (Listof S1-Bnd))

(: schml-prim->type 
   (-> Schml-Prim (Fn Natural (Listof (U Int Bool)) (U Int Bool))))
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
| The Lambda Language Family Types, Primitives, Literals, and Terminals        |
+-----------------------------------------------------------------------------|#
(define-type Cast-Literal 
  (U Schml-Literal String Schml-Type))


(define-type Cast-Prim 
  (U Schml-Prim Type-Primitives Dyn-Primitives))

(define-type Type-Primitives
  (U 'Type:Int?
     'Type:Dyn? 'Type:Bool? 
     'Type:Fn? 'Type:Fn-arg 'Type:Fn-ret 'Type:Fn))

(define-type Dyn-Primitives
  (U 'Dyn:Int?  'Dyn:Bool? 'Dyn:Fn? 'Dyn:Fn! 
     'Dyn:Int!  'Dyn:Bool! 'Dyn:Fn  
     'Dyn:Int   'Dyn:Bool  'Dyn:FnT))

(define-type Cast-Type
  (Rec LT (U Schml-Type Any-Type Any-Value String-Ptr Bottom Void 
	     (Fn Natural (Listof LT) LT))))

(define-type Cast-Fml* (Listof Cast-Fml))
(define-type Cast-Fml (Fml Uid Cast-Type))
#|-----------------------------------------------------------------------------+
| Language/Cast1 created by insert-implicit-casts                              |
+-----------------------------------------------------------------------------|#

(define-type Cast0-Lang (Prog (List String Natural Schml-Type) C0-Expr))

(define-type C0-Expr
  (Rec E (Ann (U
	       ;; Non-Terminals
	       (Lambda Cast-Fml* Cast-Type E)
	       (Letrec C0-Bnd* E)
	       (Let C0-Bnd* E)
	       (App E (Listof E))
	       (Op Cast-Prim (Listof E))
	       (If E E E)
	       (Cast E Schml-Type Schml-Type String)
	       ;; Terminals 
	       (Var Uid) 
	       (Quote Cast-Literal))
	      Cast-Type)))

(define-type C0-Bnd (Bnd Uid Cast-Type C0-Expr))
(define-type C0-Bnd* (Listof C0-Bnd))


#|-----------------------------------------------------------------------------+
| The Lambda Language Family Types, Primitives, Literals, and Terminals        |
+-----------------------------------------------------------------------------|#
(define-type Lambda-Prim
  (U Cast-Prim Closure-Primitives))

(define-type Closure-Primitives
  (U 'Clos 'Clos:set 'Clos:ref 'Clos:code))

(define-type Lambda-Literal Cast-Literal)

(define-type Lambda-Type Cast-Type)

(define-type Lambda-Fml (Fml Uid Lambda-Type))

(define-type Lambda-Fml* (Listof Lambda-Fml))

#|-----------------------------------------------------------------------------+
| Language/Lambda0 created by interpret-casts                                  |
+-----------------------------------------------------------------------------|#

(define-type Lambda0-Lang 
  (Prog (List String Natural Schml-Type) L0-Expr))

(define-type L0-Expr
  (Rec E (Ann (U (Lambda Lambda-Fml* Lambda-Type E)
		 (Letrec L0-Bnd* E)
		 (Let L0-Bnd* E)
		 (App E (Listof E))
		 (Op Lambda-Prim (Listof E))
		 (If E E E)
		 ;; Terminals
		 (Var Uid) 
		 (Quote Lambda-Literal))
	      Lambda-Type)))

(define-type L0-Bnd (Bnd Uid Lambda-Type L0-Expr))
(define-type L0-Bnd* (Listof L0-Bnd))

#|-----------------------------------------------------------------------------+
| Language/Lambda1 created                                                     |
+-----------------------------------------------------------------------------|#

(define-type Lambda1-lang (Prog (List String Natural Schml-Type) L1-Expr))

(define-type L1-Expr
  (Rec E (Ann 
	  (U  ;; Non terminals 
	   (Letrec L1-Bnd-Lambda* E)
	   (Let L1-Bnd-Data* E)
	   (App E (Listof E))
	   (Op Lambda-Prim E)
	   (If E E E)
	   ;; Terminals 
	   (Var Uid)
	   (Quote Lambda-Literal))
	  Lambda-Type)))

(define-type L1-Bnd-Lambda* (Listof L1-Bnd-Lambda))
(define-type L1-Bnd-Lambda  (Bnd Uid Lambda-Type L1-Lambda))
(define-type L1-Bnd-Data* (Listof L1-Bnd-Data))
(define-type L1-Bnd-Data  (Bnd Uid Lambda-Type L1-Expr))

(define-type L1-Lambda
  (Lambda Lambda-Fml* Lambda-Type L1-Expr))


#|-----------------------------------------------------------------------------+
| Language/Lambda2 created                                                     |
+-----------------------------------------------------------------------------|#
(define-type Lambda2-Lang (Prog (List String Natural Schml-Type) L2-Expr))

(define-type L2-Expr
  (Rec E (Ann 
	  (U ;; Non-Terminal 
	   (Letrec L2-Bnd-Lambda* E)
	   (Let L2-Bnd-Data* E)
	   (App E (Listof E))
	   (Op Lambda-Prim (Listof E))
	   (If E E E)
	   (Var Uid)
	   (Quote Lambda-Literal))
	  Lambda-Type)))

(define-type L2-Bnd-Lambda* (Listof L2-Bnd-Lambda))
(define-type L2-Bnd-Lambda (Bnd Uid Lambda-Type L2-Lambda))
(define-type L2-Bnd-Data* (Listof L2-Bnd-Data))
(define-type L2-Bnd-Data (Bnd Uid Lambda-Type L2-Expr))

(define-type L2-Lambda 
  (Lambda Lambda-Fml* Lambda-Type (Free (Listof Uid) L2-Expr)))


#|-----------------------------------------------------------------------------+
| Language/Lambda3 created                                                     |
+-----------------------------------------------------------------------------|#
;; Intermediate language were all lambda have no free variables
;; they are explicitly passed as a structure and implicitly extracted by procedures

(define-type Lambda3-Lang 
  (Prog (List String Natural Lambda-Type) L3-Expr))

(define-type L3-Expr
  (Rec E
       (Ann 
	(U ;; Non Terminals 
	 (LetP L3-Bnd-Lambda* (LetC L3-Bnd-Closure* E))
	 (Let L3-Bnd-Data* E)
	 (App (Pair (Var Uid) (Var Uid)) (Listof E))
	 (Op Lambda-Prim (Listof E))
	 (If E E E)
	 ;; Terminals
	 (Var Uid)
	 (Quote Lambda-Literal))
	Lambda-Type)))

(define-type L3-Lambda
  (Lambda Lambda-Fml* (Bound Uid (Listof Uid) L3-Expr)))

(define-type L3-Closure (Closure-data Uid (Listof Uid)))

(define-type L3-Bnd-Lambda* (Listof L3-Bnd-Lambda))
(define-type L3-Bnd-Lambda (Bnd L3-Lambda Lambda-Type))
(define-type L3-Bnd-Closure* (Listof L3-Bnd-Closure))
(define-type L3-Bnd-Closure (Bnd Uid Lambda-Type L3-Closure))
(define-type L3-Bnd-Data* (Listof L3-Bnd-Data))
(define-type L3-Bnd-Data (Bnd Uid Lambda-Type L3-Expr))

;; procedures are now just routines that have an explicit
;; layout for parameters
(define-type Lambda4-Lang
  (Prog (List String Natural Lambda-Type) L4-Expr))


(define-type L4-Expr
  (Rec E
       (Ann
	(U ;; Non-Terminals 
	 (LetC L4-Bnd-Code* E)
	 (Let L4-Bnd-Data* E)
	 (App E (Listof E))
	 (Op Lambda-Prim (Listof E))
	 (If E E E)
	 ;; Terminals
	 (Var Uid)
	 (Quote Lambda-Literal))
	Lambda-Type)))

(define-type L4-Bnd-Code* (Listof L4-Bnd-Code))
(define-type L4-Bnd-Code (Bnd Uid Lambda-Type L4-Code))
(define-type L4-Code (Code Lambda-Fml* L4-Expr))
(define-type L4-Bnd-Data* (Listof L4-Bnd-Data))
(define-type L4-Bnd-Data  (Bnd Uid Lambda-Type L4-Expr))
#|-----------------------------------------------------------------------------+
| The Data Language Family Types, Primitives, Literals, and Terminals        |
+-----------------------------------------------------------------------------|#
(define-type Data-Prim
  (U Lambda-Prim))

(define-type Data-Literal Lambda-Literal)

(define-type Data-Type Lambda-Type)

#|-----------------------------------------------------------------------------+
| Language/Lambda1 created by make-closures-explicit                           |
+-----------------------------------------------------------------------------|#

(define-type Data0-Lang
  (Prog (List String Natural Data-Type) 
	(LetC D0-Bnd-Code*
	      D0-Expr)))

(define-type D0-Bnd-Code* (Listof (Bnd Uid Data-Type D0-Code)))

(define-type D0-Code
  (Code D0-Fml* D0-Expr))

(define-type D0-Fml* (Listof D0-Fml))
(define-type D0-Fml  (Fml Uid Data-Type))

(define-type D0-Expr
  (Rec E (Ann (U (Let D0-Bnd* E)
		 (App E (Listof E))
		 (Op Data-Prim (Listof E))
		 (If E E E)
		 (Var Uid)
		 (Code-Label Uid)
		 (Quote Data-Literal))
	      Data-Type)))

(define-type D0-Bnd* (Listof D0-Bnd))
(define-type D0-Bnd  (Bnd Uid Data-Type D0-Expr))
