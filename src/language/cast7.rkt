#lang typed/racket/base
(require "forms.rkt")
(provide (all-defined-out)
         (all-from-out "forms.rkt"))

#|-----------------------------------------------------------------------------+
| Language/Cast7 created by convert-closures                                   |
+-----------------------------------------------------------------------------|#

(define-type Cast7-Lang
  (Prog (List String Natural Grift-Type) C7-Value))

(define-type C7-Value
  (Rec V (U ;; Non-Terminals
          (LetP C7-Bnd-Procedure* (LetC C7-Bnd-Closure* V))
	  (Let C7-Bnd-Data* V)
	  (App (Pair V V) (Listof V))
          (Op Grift-Primitive (Listof V))
	  (If V V V)
          (Begin C7-Effect* V)
          (Repeat Uid V V V)
          ;;closure operations
          ;;(Closure-ref V V)
          (Fn-Caster V)
          ;; FN-Type operations
          (Type-tag V)
          (Type-Fn-arg V V)
          (Type-Fn-return V)
          (Type-Fn-arity V)
          (Type-GRef-to V)
          ;; Dyn operations
          (Dyn-tag V)
          (Dyn-immediate V)
          (Dyn-type V)
          (Dyn-value V)
          (Dyn-make V V) ;; This is bad and I do not like it
          ;; Observational Operations
          (Blame V)
          (Observe V Grift-Type)
          ;; Terminals
          (Type Grift-Type)
          (Tag Tag-Symbol)
	  (Var Uid)
          (GRep V)
	  (Quote Cast-Literal))))

(define-type C7-Effect
  (Rec E
   (U (LetP C7-Bnd-Procedure* (LetC C7-Bnd-Closure* E))
      (Let C7-Bnd-Data* E)
      (Begin C7-Effect* No-Op)
      (Repeat Uid C7-Value C7-Value E)
      (App (Pair C7-Value C7-Value) (Listof C7-Value))
      (If C7-Value E E)
      No-Op
      (GRep-Effect C7-Value))))

(define-type C7-Value* (Listof C7-Value))
(define-type C7-Effect* (Listof C7-Effect))
(define-type C7-Procedure
  (Procedure Uid Uid* Uid (Option Uid) Uid* C7-Value))
(define-type C7-Closure (Closure-Data Uid (Option Uid) (Listof Uid)))
(define-type C7-Bnd-Procedure (Pairof Uid C7-Procedure))
(define-type C7-Bnd-Procedure* (Listof C7-Bnd-Procedure))
(define-type C7-Bnd-Closure (Pairof Uid C7-Closure))
(define-type C7-Bnd-Closure* (Listof C7-Bnd-Closure))
(define-type C7-Bnd-Data (Pairof Uid C7-Value))
(define-type C7-Bnd-Data* (Listof C7-Bnd-Data))
