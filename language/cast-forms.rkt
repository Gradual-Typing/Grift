#lang typed/racket

(require Schml/language/types
	 Schml/language/typed-forms)

(provide (all-defined-out)
	 Valid-Type) 

(define-type VT Valid-Type)
(define-type CF Cast-Form)

(define-type Cast-Form
  (Rec CF (U (Lambda (Fml IP) IP CF IP)
	     (Letrec (Bnd CF IP) CF IP)
	     (Let (Bnd CF IP) CF IP)
	     (App CF IP)
	     (Op Prim CF IP)
	     (If CF IP)
	     (Var IP)
	     (Cast CF VT VT Label)
	     (Quote RT-Literal IP))))

(struct Cast-Prog ([name : String]
		   [next-uvar : Natural]
		   [expression : Cast-Form]
		   [type : IP])
	#:transparent)

