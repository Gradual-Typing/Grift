#lang typed/racket

(require Schml/language/types
	 Schml/language/core-forms)

(provide (all-defined-out)) 

(define-type ST (Pair srcloc Valid-Type))

(define-type Valid-Type
  (Rec AT (U Int Bool Dyn (Fn/a (Listof AT) AT))))

(define-type Typed-Form
  (Rec TF (U (Lambda (Fml Valid-Type) Valid-Type TF ST)
	     (Letrec (Bnd TF Valid-Type) TF ST)
	     (Let (Bnd TF Valid-Type) TF ST)
	     (App TF ST)
	     (Op Prim TF ST)
	     (If TF ST)
	     (Ascribe TF Valid-Type (Maybe Label) ST)
	     (Var ST)
	     (Quote Literal ST))))

(struct Typed-Prog ([name : String]
		    [next-uvar : Natural]
		    [expression : Typed-Form]
		    [type : Valid-Type]))


(define IntxInt-Type (list Int-Type Int-Type))
(define IntxInt->Bool-Type (Fn/a 2 IntxInt-Type Bool-Type))
(define IntxInt->Int-Type (Fn/a 2 IntxInt-Type Int-Type))

(: prim->type 
   (-> Prim (Fn/a (Listof (U Int Bool)) (U Int Bool))))
(define (prim->type p)
  (cond
   [(IntxInt->Bool? p) IntxInt->Bool-Type]
   [(IntxInt->Int? p)  IntxInt->Int-Type]))

(: consistent? (Valid-Type Valid-Type . -> . Boolean))
(define (consistent? t g)
  (or (Dyn? t) (Dyn? g)
      (and (Int? t) (Int? g))
      (and (Bool? t) (Bool? g))
      (function-consistent? t g)))

(: function-consistent? (Valid-Type Valid-Type . -> . Boolean))
(define (function-consistent? t g)
  (and (Fn/a? t) (Fn/a? g)
       (= (Fn/a-arity t) (Fn/a-arity g))
       (andmap consistent? (Fn/a-fml t) (Fn/a-fml g))
       (consistent? (Fn/a-ret t) (Fn/a-ret g))))

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

(: join (Valid-Type Valid-Type . -> . Valid-Type))
(define (join t g)
  (cond
    [(Dyn? t) g]
    [(Dyn? g) t]
    [(and (Int? t) (Int? g)) Int-Type]
    [(and (Bool? t) (Bool? g)) Bool-Type]
    [(and (Fn/a? t) (Fn/a? g) 
	  (= (Fn/a-arity t) (Fn/a-arity g)))
     (Fn/a (Fn/a-arity t)
	   (map join (Fn/a-fml t) (Fn/a-fml g))
	   (join (Fn/a-ret t) (Fn/a-ret g)))]
    [else (error 'join "Types are not consistent")]))

