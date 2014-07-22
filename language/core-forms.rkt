#lang typed/racket

(require Schml/language/types)
(provide (all-defined-out))

(define-type Literal (U Integer Boolean))
(define-predicate Literal? (U Integer Boolean))

(define-type Core-Type
  (Rec CT (U Int Bool Dyn (Fn (Listof CT) (Listof CT)))))

(define-type Maybe-Type (U Core-Type False))

(define-type Maybe-Label (U String False))

(define-type Primitive-id
  (U '* '+ '- 'binary-and 'binary-or '<< '>> '< '<= '= '> '>=))
(define-predicate primitive-id? Primitive-id)

(define-type Core-Form
  (Rec CF
   (U 
    (Lambda (Fml Core-Type) Maybe-Type CF srcloc)
    (Letrec (Bnd CF Maybe-Type) CF srcloc)
    (Let (Bnd CF Maybe-Type) CF srcloc)
    (App CF srcloc)
    (Op Primitive-id CF srcloc)
    (If CF srcloc)
    (Ascribe CF Core-Type Maybe-Label srcloc)
    (Var srcloc)
    (Quote Literal srcloc))))

(struct Core-Prog 
        ([name : String]
         [next-uvar : Natural]
         [expression : Core-Form]))


