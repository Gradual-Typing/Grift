#lang typed/racket
(provide (all-defined-out))
#| 
Commonly used and recognize language forms 
In general I try to switch structs as the meaning of forms
change but I do allow the types and *syntax* of forms to
change. In general any field named annotation is not really
a usefull field but allows me to store information that may
be usefull for optimizations of keeping state around.
|#

(struct (T U) Prog ([annotation : T]
		    [expression : U])
	#:transparent)

(struct (T U V W) Lambda 
        ([formals : (Listof T)]
         [return-type : U]
	 [body : V]
	 [annotation : W])
        #:transparent)

(struct (T U V) Letrec 
        ([bindings : (Listof T)]
         [body : U]
	 [annotation : V])
        #:transparent)

(struct (T U V) Let 
        ([bindings : (Listof T)]
         [body : U]
	 [annotation : V])
        #:transparent)

(struct (T U V) App 
        ([operator : T]
         [operands : (Listof U)]
	 [annotation : V])
        #:transparent)

(struct (T U V) Op 
        ([primitive : T]
         [operands : (Listof U)]
	 [annotation : V])
        #:transparent)

(struct (T U) If 
        ([test : T]
         [then : T]
         [else : T]
	 [annotation : U])
        #:transparent)

(struct (T U V W) Ascribe 
        ([expression : T]
         [type : U]
         [label : V]
	 [annotation : W])
        #:transparent)

(struct (T U V W) Cast
	([expression : T]
	 [exp-type : U]
	 [cast-type : V]
	 [label : W])
	#:transparent)
	
(struct (U) Var
        ([ident : Uvar]
	 [annotation : U])
        #:transparent)

(struct (T U) Quote
	([literal : T]
	 [annotation : U])
	#:transparent)

(struct (T U V) When/blame ([who : T]
			    [test : U]
			    [expression : U]
			    [annotation : V])
	#:transparent)

(struct (T U V) Letproc ([bindings : T]
			 [body : U]
			 [annotation : V])
	#:transparent)

(struct (T) Code-Label ([identifier : Uvar]
			[type : T])
	#:transparent)


#| I am making a more primitive match because current patern
   matching seems to erase types |#

#| General stuff pertaining to Primitives |#

(define-type IntxInt->Int-Primitives
  (U '* '+ '- 'binary-and 'binary-or '<< '>>))

(define-type IntxInt->Bool-Primitives
  (U '< '<= '= '> '>=))

(define-predicate IntxInt->Bool-Prim? IntxInt->Bool-Primitives)
(define-predicate IntxInt->Int-Prim? IntxInt->Int-Primitives)

(define-type Type-Primitives
  (U 'Type:Int? 'Type:Dyn? 'Type:Bool? 
     'Type:Fn? 'Type:Fn-arg 'Type:Fn-ret 'Type:Fn))

(define-type Dyn-Primitives
  (U 'Dyn:Int?  'Dyn:Bool? 'Dyn:Fn? 'Dyn:Fn! 
     'Dyn:Int!  'Dyn:Bool! 'Dyn:Fn  
     'Dyn:Int   'Dyn:Bool  'Dyn:FnT))

(define-type Closure-Primitives
  (U 'Clos 'Clos:set 'Clos:ref 'Clos:code))

#| Bindings and Formals Forms|#
(struct (T) Fml ([identifier : Uvar] [type : T])
	#:transparent)
(struct (T U) Bnd ([identifier : Uvar]
                   [type : U]
                   [expression : T])
	#:transparent)

#| General stuff pertaining to types |#
;; Atomic types
(struct Int ()
	#:transparent)
(define INT-TYPE (Int))
(struct Bool ()
	#:transparent)
(define BOOL-TYPE (Bool))
(struct Dyn ()
	#:transparent)
(define DYN-TYPE (Dyn))
(struct Value-Ptr ()
	#:transparent)
(define VALUE-PTR (Value-Ptr))
(struct Type-Ptr ()
	#:transparent)
(define TYPE-PTR (Type-Ptr))
(struct String-Ptr ()
	#:transparent)
(define STRING-PTR (String-Ptr))

(struct (T U) Fn ([arity : T][fmls : U] [ret : V])
	#:transparent)

(define INTxINT-TYPE (list INT-TYPE INT-TYPE))
(define INTxINT->BOOL-TYPE (Fn 2 INTxINT-TYPE BOOL-TYPE))
(define INTxINT->INT-TYPE (Fn 2 INTxINT-TYPE INT-TYPE))

(define (shallow-consistent? t g)
  (or (Dyn? t)
      (Dyn? g)
      (and (Int? t) (Int? g))
      (and (Bool? t) (Bool? g))
      (and (Fn? t) (Fn? g))))

#| Unique Variables |#
(struct Uvar
  ([prefix : String]
   [suffix : Natural])
  #:transparent)

(define (uvar=? [u : Uvar] [v : Uvar])
  (= (Uvar-suffix u) (Uvar-suffix v)))

#| Maybe type |#

(define-type (Maybe T) (U T False)) 
(define-type Src srcloc)


#| The Various Languages that are compiled |#

;; The language created by schml/read
(define-type Stx (Syntaxof Any))
(struct: Stx-Prog
	 ([name : String]
	  [syntax* : (Listof Stx)]))

;; The language created by schml/parse
(define-type Schml (Prog (List String Natural) S0-Expr))

(define-type Schml-Prim 
  (U IntxInt->Int-Primitives IntxInt->Bool-Primitives))

(define-predicate Schml-Prim? Schml-Prim)

(define-type Schml-Literal
  (U Integer Boolean))

(define-type Schml-Type
  (Rec CT (U Int Bool Dyn 
	     (Fn Natural (Listof Schml-Type) Schml-Type))))

(define-type S0-Expr
  (Rec S0 (U (Lambda (Fml Schml-Type) (Maybe Schml-Type) S0 Src)
	     (Letrec (Bnd S0 (Maybe Schml-Type)) S0 Src)
	     (Let (Bnd S0 (Maybe Schml-Type)) S0 Src)
	     (App S0 S0 Src)
	     (Op Schml-Prim S0 Src)
	     (If S0 Src)
	     (Ascribe S0 Schml-Type (Maybe Label) Src)
	     (Var Src)
	     (Quote Schml-Literal Src))))



#| The type of allowed literals |#

(define (Schml-Literal? x)
  (or (and (integer? x) (>= x 0) (<= x (expt 2 64)))
      (boolean? x)))
	   
(: schml-literal->type (Schml-Literal -> (U Bool Int)))
(define (schml-literal->type x)
  (if (boolean? x)
      BOOL-TYPE
      INT-TYPE))




;; The language created by schml/type-check
(define-type Schml1
  (Prog (List String Natural Schml-Type) S1-Expr))

(define-type Src.Type (Pair srcloc Typed-Type))

(define-type Schml-Expr
  (Rec S1 (U (Lambda (Fml Schml-Type) Schml-Type S1 Src.Type)
	     (Letrec (Bnd S1 Schml-Type) S1 Src.Type)
	     (Let (Bnd S1 Schml-Type) S1 Src.Type)
	     (App S1 S1 Src.Type)
	     (Op (Pair Schml-Prim (Listof Schml-Type)) S1 Src.Type)
	     (If S1 Src.Type)
	     (Ascribe S1 Schml-Type (Maybe Label) Src.Type)
	     (Var Src.Type)
	     (Quote Schml-Literal Src.Type))))


(: schml-prim->schml-type 
   (-> Schml-Prim (Fn (Listof (U Int Bool)) (U Int Bool))))
(define (schml-prim->schml-type p)
  (cond
   [(IntxInt->Bool-Prim? p) INTxINT->BOOL-TYPE]
   [(IntxInt->Int-Prim? p)  INTxINT->INT-TYPE]))

(: consistent? (Schml-Type Schml-Type . -> . Boolean))
(define (consistent? t g)
  (or (Dyn? t) (Dyn? g)
      (and (Int? t) (Int? g))
      (and (Bool? t) (Bool? g))
      (function-consistent? t g)))

(: function-consistent? (Schml-Type Schml-Type . -> . Boolean))
(define (function-consistent? t g)
  (and (Fn? t) (Fn? g)
       (= (Fn-arity t) (Fn-arity g))
       (andmap consistent? (Fn-fmls t) (Fn-fmls g))
       (consistent? (Fn-ret t) (Fn-ret g))))

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
    [(and (Fn? t) (Fn? g) 
	  (= (Fn-arity t) (Fn-arity g)))
     (Fn (Fn-arity t)
	   (map join (Fn-fmls t) (Fn-fmls g))
	   (join (Fn-ret t) (Fn-ret g)))]
    [else (error 'join "Types are not consistent")]))

;; The language created by schml/insert-implicit-casts

(define-type Cast-Literal 
  (U Schml-Literal 'Int 'Dyn 'Bool String))
   
(define-type Cast-Prim (U Schml-Prim Cast-Primitives))
(define-type Cast-Type
  (Rec LT (U Schml-Type Type-Ptr Value-Ptr Label-Ptr Bottom 
	     (Fn (Listof LT) LT))))

(define-type Cast-Form
  (Rec CF (U (Lambda (Fml Cast-Type) Cast-Type CF Cast-Type)
	     (Letrec (Bnd CF Cast-Type) CF Cast-Type)
	     (Let (Bnd CF Cast-Type) CF Cast-Type)
	     (App CF CF Cast-Type)
	     (Op Cast-Prim CF Cast-Type)
	     (If CF Cast-Type)
	     (Var Cast-Type)
	     (Cast CF Schml-Type Schml-Type Label)
	     (Quote Cast-Literal Cast-Type))))

(struct Cast-Prog ([name : String]
		   [next-uvar : Natural]
		   [expression : Cast-Form]
		   [type : Cast-Type])
	#:transparent)

;; The language created by cast/interp-casts

(define-type Lambda-Prim
  (U Cast-Prim Closure-Primitives))

(define-type Lambda-Literal Cast-Literal)

(define-type Lambda-Type Cast-Type)

(define-type Lambda-Form
  (Rec LF 
       (U (Lambda (Fml Lambda-Type) Lambda-Type LF Lambda-Type)
	  (Letrec (Bnd LF Lambda-Type) LF Lambda-Type)
	  (Let (Bnd LF Lambda-Type) LF Lambda-Type)
	  (App LF LF Lambda-Type)
	  (Op Lambda-Prim LF Lambda-Type)
	  (If LF Lambda-Type)
	  (Var Lambda-Type)
	  (When/blame LF Lambda-Type)
	  (Quote Lambda-Literal Lambda-Type))))

(struct Lambda-Prog ([name : String]
		     [next-uvar : Natural]
		     [expression : Lambda-Form]
		     [type : Lambda-Type])
	#:transparent)

;; These forms are 
(struct (T U) Free ([free-vars : T]
		    [expression : U])
	#:transparent)

(struct (T U) Letclos ([bindings : T]
			 [body : U])
	#:transparent)

(struct (T U) Closure-data ([code : T]
			    [free : U]))

(struct (T U V W X) Procedure ([closure : T]
			       [formals : U]
			       [clos-layout : V]
			       [body : W]
			       [annotation : X])
	#:transparent)


;; Intermediate language were all letrecs only bind
;; unassigned lambdas
;; This is created by closure/purify-letrec
;; L1 intermediate language
(define-type L1-Prim Lambda-Prim)
(define-type L1-Literal Lambda-Literal)
(define-type L1-Type Lambda-Type)

(define-type L1-Lambda 
  (Lambda (Fml L1-Type) L1-Type L1-Form L1-Type))

(define-type L1-Form
  (Rec L1F 
       (U (Letrec (Bnd L1-Lambda L1-Type) L1F L1-Type)
	  (Let (Bnd L1F L1-Type) L1F L1-Type)
	  (App L1F L1F L1-Type)
	  (Op L1-Prim L1F L1-Type)
	  (If L1F L1-Type)
	  (Var L1-Type)
	  (When/blame L1F L1-Type)
	  (Quote L1-Literal L1-Type))))

(struct L1-Prog ([name : String]
		 [next-uvar : Natural]
		 [expression : L1-Form]
		 [type : Lambda-Type])
	#:transparent)

;; Intermediate language were all lambda have a subform that
;; keeps a list of free variables
;; This is created by closure/purify-letrec
;; L2 intermediate language
(define-type L2-Prim Lambda-Prim)
(define-type L2-Literal Lambda-Literal)
(define-type L2-Type Lambda-Type)

(define-type L2-Lambda 
  (Lambda (Fml L2-Type) 
	  L2-Type 
	  (Free (Listof Uvar) L2-Form) 
	  L2-Type))

(define-type L2-Form
  (Rec L2F 
       (U (Letrec (Bnd L2-Lambda L2-Type) L2F L2-Type)
	  (Let (Bnd L2F L2-Type) L2F L2-Type)
	  (App L2F L2F L2-Type)
	  (Op L2-Prim L2F L2-Type)
	  (If L2F L2-Type)
	  (Var L2-Type)
	  (When/blame L2F L2-Type)
	  (Quote L2-Literal L2-Type))))

(struct L2-Prog ([name : String]
		 [next-uvar : Natural]
		 [expression : L2-Form]
		 [type : Lambda-Type])
	#:transparent)

;; Intermediate language were all lambda have no free variables
;; they are explicitly passed as a structure and implicitly extracted by procedures

(define-type Lambda3 
  (Prog (List String Natural L3-Type) L3-Expr))

(define-type L3-Prim Lambda-Prim)
(define-type L3-Literal Lambda-Literal)
(define-type L3-Type Lambda-Type)

(define-type L3-Procedure
  (Procedure Uvar (Listof (Fml L3-Type)) (Listof Uvar) L3-Expr L3-Type))
(define-type L3-Closure (Closure-data Uvar (Listof Uvar)))

(define-type L3-Expr
  (Rec L3E
       (U (Letproc (Listof (Bnd L3-Procedure L3-Type))
		   (Letclos (Listof (Bnd L3-Closure L3-Type))
			    L3E)
		   L3-Type)
	  (Let (Bnd L3E L3-Type) L3E L3-Type)
	  (App (Pair (Var L3-Type) (Var L3-Type)) L3E L3-Type)
	  (Op L3-Prim L3E L3-Type)
	  (If L3E L3-Type)
	  (Var L3-Type)
	  (When/blame L3E L3-Type)
	  (Quote L3-Literal L3-Type))))

;; procedures are now just routines that have an explicit
;; layout for parameters
(define-type Lambda4
  (Prog (List String Natural L4-Type) L4-Expr))

(define-type L4-Prim Lambda-Prim)
(define-type L4-Literal Lambda-Literal)
(define-type L4-Type Lambda-Type)

(define-type L4-Code
  (Code (Listof (Fml L4-Type)) L4-Expr L4-Type))

(define-type L4-Expr
  (Rec L4E
       (U (Letcode (Listof (Bnd L4-Code L4-Type))
		   L4E
		   L4-Type)
	  (Let (Bnd L4E L4-Type) L4E L4-Type)
	  (App L4E L4E L4-Type)
	  (Op L4-Prim L4E L4-Type)
	  (If L4E L4-Type)
	  (When/blame L4E L4-Type)
	  (Var L4-Type)
	  (Code-Label l t)
	  (Quote L4-Literal L4-Type))))

;; The data language as created by closures/make-closures-explicit

(define-type Data
  (Prog (List String Natural Data-Type) 
	(letcode (Bnd D0-Code Data-Type) D0-Expr)))

(define-type Data-Type Lambda-Type)
(define-type Data-Literal Lambda-Literal)
(define-type Data-Prim Lambda-Prim)

(define-type D0-Code
  (Code (Listof (Fml D0-Type)) D0-Expr D0-Type))

(define-type D0-Expr
  (Rec D0E
       (U (Let (Bnd D0E D0-Type) D0E D0-Type)
	  (App D0E D0E D0-Type)
	  (Op D0-Prim D0E D0-Type)
	  (If D0E D0-Type)
	  (When/blame D0E D0-Type)
	  (Var D0-Type)
	  (Code-Label l t)
	  (Quote D0-Literal D0-Type))))
