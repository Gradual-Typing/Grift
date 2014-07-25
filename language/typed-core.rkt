#lang typed/racket

(require Schml/language/types
	 Schml/language/core-forms)

(provide (all-defined-out)
	 Core-Type) 

(define-type ST (Pair srcloc Core-Type))

(define-type Valid-Type
  (Rec AT (U Int Bool Dyn (Fn (Listof AT) AT))))

(define-type Typed-Core
  (Rec TC (U (Lambda (Fml Core-Type) Core-Type TC ST)
	     (Letrec (Bnd TC Core-Type) TC ST)
	     (Let (Bnd TC Core-Type) TC ST)
	     (App TC ST)
	     (Op Prim TC ST)
	     (If TC ST)
	     (Ascribe TC Core-Type Maybe-Label ST)
	     (Var ST)
	     (Quote Literal ST))))

(struct Typed-Prog ([name : String]
		    [next-uvar : Natural]
		    [expression : Typed-Core]
		    [type : Core-Type]))
