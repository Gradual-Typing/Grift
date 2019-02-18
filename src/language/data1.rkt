#lang typed/racket/base/no-check
(require "forms.rkt"
         "primitives.rkt"
         "data0.rkt")
(provide (all-defined-out)
         (all-from-out "forms.rkt" "primitives.rkt"))

#|-----------------------------------------------------------------------------+
| Data1-Language created by normalize-context                                          |
+-----------------------------------------------------------------------------|#

(define-type Data1-Lang
  (Prog (List String Natural Grift-Type)
        (GlobDecs Uid*
                    (Labels D1-Bnd-Code*
                            D1-Tail))))

(define-type D1-Bnd-Code* (Listof D1-Bnd-Code))
(define-type D1-Bnd-Code (Pairof Uid D1-Code))
(define-type D1-Code (Code Uid* D1-Tail))

(define-type D1-Tail
  (Rec T
   (U (If D1-Pred T T)
      (Switch D1-Value (Switch-Case* T) T)
      (Begin D1-Effect* T)
      (App-Code D1-Value D1-Value*)
      (Op UIL-Expr-Prim D1-Value*)
      (Var Uid)
      (Global String)
      Halt Success Stack-Alloc
      (Var Uid)
      (Code-Label Uid)
      (Quote D1-Literal))))

(define-type D1-Value
 (Rec V
      (U (If D1-Pred V V)
         (Switch V (Switch-Case* V) V)
         (Begin D1-Effect* V)
         (App-Code V (Listof V))
         (Op UIL-Expr-Prim (Listof V))
         Halt
         Stack-Alloc
         (Var Uid)
         (Global String)
         (Code-Label Uid)
         (Quote D1-Literal))))

(define-type D1-Pred
 (Rec P
      (U (If D1-Pred P P)
         (Switch D1-Value (Switch-Case* P) P)
         (Begin D1-Effect* P)
         (Relop UIL-Pred-Prim D1-Value D1-Value))))

(define-type D1-Effect
 (Rec E
      (U (If D1-Pred E E)
         (Switch D1-Value (Switch-Case* E) E)
         (Begin D1-Effect* No-Op)
         (Repeat Uid D1-Value D1-Value #f #f E)
         Break-Repeat
         (App-Code D1-Value D1-Value*)
         (UIL-Op! D1-Value)
         (Assign Id D1-Value)
         No-Op)))


(define-type D1-Value* (Listof D1-Value))
(define-type D1-Effect* (Listof D1-Effect))

(define-type D1-Bnd  (Pairof Uid D1-Value))

(define-type D1-Bnd* (Listof D1-Bnd))

(define-type D1-Literal Data-Literal)
