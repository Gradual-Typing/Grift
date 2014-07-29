#lang typed/racket

(require Schml/language/types
	 Schml/language/typed-forms)

(provide (all-defined-out)
	 (rename-out [Valid-Type Type])) 

(define-type Cast-Form
  (Rec CF (U (Lambda (Fml Valid-Type) Valid-Type CF Type)
	     (Letrec (Bnd CF Valid-Type) CF Type)
	     (Let (Bnd CF Valid-Type) CF Type)
	     (App CF Type)
	     (Op Prim CF Type)
	     (If CF Type)
	     (Cast CF Type Type Label)
	     (Var Type)
	     (Quote Literal Type))))

(struct Cast-Prog ([name : String]
		   [next-uvar : Natural]
		   [expression : Typed-Form]
		   [type : Valid-Type]))

