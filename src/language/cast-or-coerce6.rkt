#lang typed/racket/base
(require "forms.rkt")

(provide (all-defined-out)
         (all-from-out "forms.rkt"))
#|-----------------------------------------------------------------------------+
| Language/Cast3 created by interpret-casts                                    |
+-----------------------------------------------------------------------------|#

(define-type Cast-or-Coerce5-Lang
 (Prog (List String Natural Schml-Type) CoC6-Expr))

(define-type CoC6-Expr
  (Rec E (U
          ;; Closure-Operations
          (LetP CoC6-Bnd-Procedure* (LetC CoC6-Bnd-Closure* E))
          (Closure-code E)
          (Closure-caster E)
          (Closure-ref Uid Uid)
          (App (Pair E E) (Listof E))
          ;; Code Labels
          (Code-Label Uid)
          (Labels CoC6-Bnd-Code* E)
          (App-Code E (Listof E))
          ;; Functions as an interface
          (Lambda Uid* (Castable (Option Uid) E))
          (Fn-Caster E)
          (App-Fn E (Listof E))
          ;; Our Lovely Function Proxy Representation
          (App-Fn-or-Proxy Uid E (Listof E))
          (Fn-Proxy (List Index Uid) E E)
          (Fn-Proxy-Huh E)
          (Fn-Proxy-Closure E)
          (Fn-Proxy-Coercion E)
          ;; Coercions
          (Quote-Coercion Schml-Coercion)
          (Compose-Coercions E E)
          (Id-Coercion-Huh E)
          (Fn-Coercion-Huh E)
          (Make-Fn-Coercion Uid E E E)
          (Compose-Fn-Coercion Uid E E)
          (Fn-Coercion (Listof E) E)
          (Fn-Coercion-Arg E E)
          (Fn-Coercion-Return E)
          (Ref-Coercion E E)
          (Ref-Coercion-Huh E)
          (Ref-Coercion-Read E)
          (Ref-Coercion-Write E)
          (Sequence-Coercion E E)
          (Sequence-Coercion-Huh E)
          (Sequence-Coercion-Fst E)
          (Sequence-Coercion-Snd E)
          (Project-Coercion E E)
          (Project-Coercion-Huh E)
          (Project-Coercion-Type E)
          (Project-Coercion-Label E)
          (Inject-Coercion E)
          (Inject-Coercion-Type E)
          (Inject-Coercion-Huh E)
          (Failed-Coercion E)
          (Failed-Coercion-Huh E)
          (Failed-Coercion-Label E)
          ;;Type operations
          (Type Schml-Type)
          (Type-Dyn-Huh E)
          (Type-Fn-Huh E)
          (Type-Fn-arity E)
          (Type-Fn-arg E E)
          (Type-Fn-return E)
          (Type-GRef-Huh E)
          (Type-GRef-Of  E)
          (Type-GVect-Huh E)
          (Type-GVect-Of E)
          ;; Tags are exposed before specify This is bad
          ;; TODO fix this after the deadline
          (Type-Tag E)
          (Tag Tag-Symbol)
          ;;(Type-Ctr-Case E Type-Ctr-Case-Clause* E)
          ;;(Dyn-Case E Type-Ctr-Clause* E)
          ;; Dynamic as a value
          (Dyn-tag E)
          (Dyn-immediate E)
          (Dyn-type E)
          (Dyn-value E)
          (Dyn-make E E)
          ;; binding 
	  (Let CoC6-Bnd-Data* E)
          (Var Uid)
          ;; Controll Flow
          (If E E E)
          (Begin CoC6-Expr* E)
          (Repeat Uid E E E)
          ;;Primitives
          (Op Schml-Primitive (Listof E))
          (Quote Cast-Literal)
          ;; Casts with different ways of getting the same semantics
	  ;;(Cast E (Twosome Schml-Type Schml-Type Blame-Label))
          ;;(Cast E (Coercion Schml-Coercion))
          ;;(Interpreted-Cast E (Twosome E E E))
          ;;(Interpreted-Cast E (Coercion E))
          ;; Observations
          (Blame E)
          (Observe E Schml-Type)
          ;; Unguarded-Representation
          (Unguarded-Box E)
          (Unguarded-Box-Ref E)
          (Unguarded-Box-Set! E E)
          (Unguarded-Vect E E)
          (Unguarded-Vect-Ref E E)
          (Unguarded-Vect-Set! E E E)
          (Guarded-Proxy-Huh E)
          (Guarded-Proxy E (Twosome E E E))
          (Guarded-Proxy E (Coercion E))
          (Guarded-Proxy-Ref E)
          (Guarded-Proxy-Source E)
          (Guarded-Proxy-Target E)
          (Guarded-Proxy-Blames E)
          (Guarded-Proxy-Coercion E))))



(define-type CoC6-Expr* (Listof CoC6-Expr))
(define-type CoC6-Code (Code Uid* CoC6-Expr))
(define-type CoC6-Bnd-Code (Pairof Uid CoC6-Code))
(define-type CoC6-Bnd-Code* (Listof CoC6-Bnd-Code))
(define-type CoC6-Bnd-Data  (Pairof Uid CoC6-Expr))
(define-type CoC6-Bnd-Data* (Listof CoC6-Bnd-Data))

(define-type CoC6-Procedure
  (Procedure Uid Uid* Uid (Option Uid) Uid* CoC6-Expr))
(define-type CoC6-Closure (Closure-Data CoC6-Expr CoC6-Expr CoC6-Expr*))
(define-type CoC6-Bnd-Procedure (Pairof Uid CoC6-Procedure))
(define-type CoC6-Bnd-Procedure* (Listof CoC6-Bnd-Procedure))
(define-type CoC6-Bnd-Closure (Pairof Uid CoC6-Closure))
(define-type CoC6-Bnd-Closure* (Listof CoC6-Bnd-Closure))
