#lang typed/racket

(require Schml/language/types)

(provide (all-defined-out))

(define-type Core-Type
  (Rec CT (U Int Bool Dyn (Fn (Listof CT) (Listof CT)))))

(define-type Core-Form
  (Rec CF (U (Lambda (Fml Core-Type) (Maybe Core-Type) CF srcloc)
	     (Letrec (Bnd CF (Maybe Core-Type)) CF srcloc)
	     (Let (Bnd CF (Maybe Core-Type)) CF srcloc)
	     (App CF srcloc)
	     (Op Prim CF srcloc)
	     (If CF srcloc)
	     (Ascribe CF Core-Type (Maybe Label) srcloc)
	     (Var srcloc)
	     (Quote Literal srcloc))))

(struct Core-Prog 
        ([name : String]
         [next-uvar : Natural]
         [expression : Core-Form]))


