#lang typed/racket/base
(require "forms.rkt" "primitives.rkt")

(provide (all-defined-out)
         (all-from-out "forms.rkt" "primitives.rkt"))
#|-----------------------------------------------------------------------------+
| Language/Cast3 created by interpret-casts                                    |
+-----------------------------------------------------------------------------|#



(define-type Cast-or-Coerce6-Lang
  (Prog (List String Natural Grift-Type)
        (Let-Static* CoC6-Bnd-Type* CoC6-Bnd-Crcn* CoC6-Expr)))

(define-type CoC6-Expr
  (Rec E (U

          (Construct CoC6-Gen-Data CoC6-Gen-Ctor (Listof E))
          (Access CoC6-Gen-Data CoC6-Gen-Access E (Option E))
          (Check CoC6-Gen-Data CoC6-Gen-Pred E (Listof E))
          (LetP CoC6-Bnd-Procedure* (LetC CoC6-Bnd-Closure* E))
          (LetP CoC6-Bnd-Procedure* E)
          (Closure-code E)
          (Closure-caster E)
          (Closure-ref Uid Uid)
          (App-Closure E E (Listof E))
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
          (Hybrid-Proxy Uid E E)
          (Hybrid-Proxy-Huh E)
          (Hybrid-Proxy-Closure E)
          (Hybrid-Proxy-Coercion E)
          (Fn-Proxy Index E E)
          (Fn-Proxy-Huh E)
          (Fn-Proxy-Closure E)
          (Fn-Proxy-Coercion E)
          ;; Coercions
          (Quote-Coercion Immediate-Coercion)
          (HC E E E E E E)
          (HC-Inject-Huh E)
          (HC-Project-Huh E)
          (HC-Identity-Huh E)
          (HC-Label E)
          (HC-T1 E)
          (HC-T2 E)
          (HC-Med E)
          ;(Compose-Coercions E E)
          (Id-Coercion-Huh E)
          (Fn-Coercion-Huh E)
          (Make-Fn-Coercion Uid E E E)
          (Compose-Fn-Coercion Uid E E)
          (Fn-Coercion (Listof E) E)
          (Fn-Coercion-Arity E)
          (Fn-Coercion-Arg E E)
          (Fn-Coercion-Return E)
          (Fn-Coercion-Return-Set! E E)
          (Fn-Coercion-Arg-Set! E E E)
          (Id-Fn-Coercion E)
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
          (Type Immediate-Type)
          (Type-Dyn-Huh E)
          (Type-Fn-Huh E)
          (Type-Fn-arity E)
          (Type-Fn-arg E E)
          (Type-Fn-return E)
          (Type-GRef-Huh E)
          (Type-GRef-Of  E)
          (Type-GVect-Huh E)
          (Type-GVect-Of E)
          (Atomic-Type-Huh E)
          ;;(Type-Ctr-Case E Type-Ctr-Case-Clause* E)
          ;; binding 
	  (Let CoC6-Bnd-Data* E)
          (Var Uid)
          ;; Controll Flow
          (If E E E)
          (Switch E (Switch-Case* E) E)
          (Begin CoC6-Expr* E)
          (Repeat Uid E E Uid E E)
          Break-Repeat
          ;;Primitives
          (Op Grift-Primitive (Listof E))
          No-Op
          (Quote Cast-Literal)
          ;; Casts with different ways of getting the same semantics
	  ;;(Cast E (Twosome Grift-Type Grift-Type Blame-Label))
          ;;(Cast E (Coercion Grift-Coercion))
          ;;(Interpreted-Cast E (Twosome E E E))
          ;;(Interpreted-Cast E (Coercion E))
          ;; Observations
          (Blame E)
          (Observe E Grift-Type)
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
          (Guarded-Proxy-Coercion E)
          (Unguarded-Vect-length E)
          ;; Monotonic references
          (Mbox E Immediate-Type)
          (Mbox-val-set! E E)
          (Mbox-val-ref E)
          (Mbox-rtti-set! E E)
          (Mbox-rtti-ref E)
          (Make-GLB-Two-Fn-Types Uid E E)
          (Make-GLB-Two-Tuple-Types Uid E E)
          (MRef-Coercion-Huh E)
          (MRef-Coercion-Type E)
          (MRef-Coercion E)
          (Type-GRef E) ;; glb need to create new types in runtime
          (Type-GVect E)
          (Type-MRef E)
          (Type-MRef-Huh E)
          (Type-MRef-Of E)
          (Error E)
          (Mvector E E Immediate-Type)
          (Mvector-length E)
          (Mvector-val-ref E E)
          (Mvector-val-set! E E E)
          (Mvector-rtti-ref E)
          (Mvector-rtti-set! E E)
          (Type-MVect E)
          (Type-MVect-Huh E)
          (Type-MVect-Of E)
          (MVect-Coercion-Huh E)
          (MVect-Coercion-Type E)
          (MVect-Coercion E)
          ;;
          (Create-tuple (Listof E))
          (Copy-Tuple E E)
          (Tuple-proj E E)
          (Tuple-Coercion-Huh E)
          (Tuple-Coercion-Num E)
          (Tuple-Coercion-Item E E)
          (Tuple-Coercion-Item-Set! E E E)
          (Id-Tuple-Coercion E)
          (Coerce-Tuple Uid E E)
          (Coerce-Tuple-In-Place Uid E E E)
          (Cast-Tuple Uid E E E E)
          (Cast-Tuple-In-Place Uid E E E E E)
          (Type-Tuple-Huh E)
          (Type-Tuple-num E)
          (Type-Tuple-item E E)
          (Make-Tuple-Coercion Uid E E E)
          (Compose-Tuple-Coercion Uid E E)
          (Mediating-Coercion-Huh E))))



(define-type CoC6-Expr* (Listof CoC6-Expr))
(define-type CoC6-Code (Code Uid* CoC6-Expr))
(define-type CoC6-Bnd-Code (Pairof Uid CoC6-Code))
(define-type CoC6-Bnd-Code* (Listof CoC6-Bnd-Code))
(define-type CoC6-Bnd-Data  (Pairof Uid CoC6-Expr))
(define-type CoC6-Bnd-Data* (Listof CoC6-Bnd-Data))

(define-type CoC6-Procedure
  (Procedure Uid Uid* Uid (Option Uid) Uid* CoC6-Expr))
(define-type CoC6-Closure
  (Closure-Data CoC6-Expr (Option CoC6-Expr) CoC6-Expr*))
(define-type CoC6-Bnd-Procedure (Pairof Uid CoC6-Procedure))
(define-type CoC6-Bnd-Procedure* (Listof CoC6-Bnd-Procedure))
(define-type CoC6-Bnd-Closure (Pairof Uid CoC6-Closure))
(define-type CoC6-Bnd-Closure* (Listof CoC6-Bnd-Closure))
(define-type CoC6-Bnd-Type  (Pairof Uid Compact-Type))
(define-type CoC6-Bnd-Type* (Listof CoC6-Bnd-Type))
(define-type CoC6-Bnd-Crcn  (Pairof Uid Compact-Coercion))
(define-type CoC6-Bnd-Crcn* (Listof CoC6-Bnd-Crcn))

(define-type CoC6-Gen-Data
  (U Dyn))
(define-type CoC6-Gen-Ctor
  (U Dyn-Repr-Ctor))
(define-type CoC6-Gen-Access
  (U Dyn-Repr-Access))
(define-type CoC6-Gen-Pred
  (U Dyn-Repr-Pred))



;; TODO Many of these forms static forms are identical accrose passes
;; should we lift them into a seperate file so they can be used
;; over and over.
