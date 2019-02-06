#lang typed/racket/base/no-check
(require "forms.rkt" "primitives.rkt" "lambda0.rkt")

(provide (all-defined-out)
         (all-from-out "forms.rkt" "primitives.rkt"))

#|------------------------------------------------------------------------------
Cast-or-Coerce3.1-Lang is the product of hoist-types
------------------------------------------------------------------------------|#

(define-type Cast-or-Coerce3.1-Lang
  (Prog (List String Natural Grift-Type)
    (Static* (List
              Bnd-Mu-Type*
              Bnd-Type*
              Bnd-Mu-Crcn*
              Bnd-Crcn*
              CoC3.1-Bnd*) 
             CoC3.1-Expr)))

(define-type (Castable-Lambda E) (CLambda E))

(define-type (Gen-Data-Forms E)
  (U (Construct Gen-Data Gen-Ctor (Listof E))
     (Access Gen-Data Gen-Access E (Option E))
     (Check Gen-Data Gen-Pred E (Listof E))))

(define-type (Code-Forms E)
  (U (Code-Label Uid)
     (Labels (Bnd* (Code Uid* E)) E)
     (App-Code E (Listof E))))

(define-type (Fn-Proxy-Forms E)
  (U (App-Fn-or-Proxy Uid E (Listof E))
     (Fn-Proxy (List Index Uid) E E)
     (Fn-Proxy-Huh E)
     (Fn-Proxy-Closure E)
     (Fn-Proxy-Coercion E)))

(define-type (Coercion-Operation-Forms E)
  (U
   (Sequence-Coercion E E)
   (Sequence-Coercion-Huh E)
   (Sequence-Coercion-Fst E)
   (Sequence-Coercion-Snd E)

   (Project-Coercion E E)
   (Project-Coercion-Huh E)
   (Project-Coercion-Type E)
   (Project-Coercion-Label E)

   (Inject-Coercion E)
   (Inject-Coercion-Huh E)
   (Inject-Coercion-Type E)

   (Mediating-Coercion-Huh E)
     
   (Make-Fn-Coercion Uid E E E)
   (Id-Coercion-Huh E)
   (Id-Fn-Coercion E) 
   (Fn-Coercion-Huh E)
   (Fn-Coercion (Listof E) E)
   (Fn-Coercion-Arity E)
   (Fn-Coercion-Arg E E)
   (Fn-Coercion-Return E)
   (Fn-Coercion-Return-Set! E E)
   (Fn-Coercion-Arg-Set! E E E)

   (Ref-Coercion E E E)
   (Ref-Coercion-Huh E)
   (Ref-Coercion-Read E)
   (Ref-Coercion-Write E)
   (Ref-Coercion-Ref-Huh E)

   (MRef-Coercion E)
   (MRef-Coercion-Huh E)
   (MRef-Coercion-Type E)

   (MVect-Coercion-Huh E)
   (MVect-Coercion-Type E)
   (MVect-Coercion E)

   (Failed-Coercion E)
   (Failed-Coercion-Huh E)
   (Failed-Coercion-Label E)

   (Make-Tuple-Coercion Uid E E E)
   (Id-Tuple-Coercion E)
   (Tuple-Coercion-Huh E)
   (Tuple-Coercion-Num E)
   (Tuple-Coercion-Item E E)
   (Tuple-Coercion-Item-Set! E E E)
   
   Make-Mu-Coercion
   (Mu-Coercion-Huh E)
   (Mu-Coercion-Body-Set! E E)
   (Mu-Coercion-Body E)))

(define-type (Type-Operation-Forms E)
  (U (Type-Dyn-Huh E)
     (Type-Fn-Huh E)
     (Type-Fn-arity E)
     (Type-Fn-arg E E)
     (Type-Fn-return E)
     (Type-GRef-Huh E)
     (Type-GRef-Of  E)
     (Type-GVect-Huh E)
     (Type-GVect-Of E)
     (Type-GRef E) 
     (Type-GVect E)
     (Type-MRef E)
     (Type-MRef-Huh E)
     (Type-MRef-Of E)
     (Type-MVect E)
     (Type-MVect-Huh E)
     (Type-MVect-Of E)
     (Type-Tuple-Huh E)
     (Type-Tuple-num E)
     (Type-Tuple-item E E)
     (Type-Mu-Huh E)
     (Type-Mu-Body E)))

(define-type (Tag-Forms E) 
  ;; Tags are exposed before specify This is bad
  ;; TODO fix this after the deadline
  (U (Type-Tag E)
     (Tag Tag-Symbol)))

(define-type (Control-Flow-Forms E)
  (U (If E E E)
     (Switch E (Switch-Case* E) E)
     (Begin (Listof E) E)
     (Repeat Uid E E Uid E E)
     Break-Repeat))

(define-type (Unguarded-Forms E)
  (U (Unguarded-Box E)
     (Unguarded-Box-On-Stack E)
     (Unguarded-Box-Ref E)
     (Unguarded-Box-Set! E E)
     (Unguarded-Vect E E)
     (Unguarded-Vect-Ref E E)
     (Unguarded-Vect-Set! E E E)
     (Unguarded-Vect-length E)))

(define-type (Guarded-Proxy-Forms E)
  (U (Guarded-Proxy-Huh E)
     (Guarded-Proxy E (U (Twosome E E E) (Coercion E)))
     (Guarded-Proxy-Ref E)
     (Guarded-Proxy-Source E)
     (Guarded-Proxy-Target E)
     (Guarded-Proxy-Blames E)
     (Guarded-Proxy-Coercion E)))

(define-type (Monotonic-Forms-w/o-Constructors E)
  (U (Mbox-val-set! E E)
     (Mbox-val-ref E)
     (Mbox-rtti-set! E E)
     (Mbox-rtti-ref E) 
     (Mvector-length E)
     (Mvector-val-ref E E VectorAccessMode)
     (Mvector-val-set! E E E VectorAccessMode)
     (Mvector-rtti-ref E)
     (Mvector-rtti-set! E E) 
     (Make-GLB-Two-Fn-Types Uid E E)
     (Make-GLB-Two-Tuple-Types Uid E E)))

(define-type (Monotonic-Forms E T)
  (U (Mvector E E T)
     (Mbox E T)
     (Monotonic-Forms-w/o-Constructors E)))

(define-type (Hyper-Coercion-Operation-Forms E)
  (U (HC E E E E E E)
     (HC-Inject-Huh E)
     (HC-Project-Huh E)
     (HC-Identity-Huh E)
     (HC-Label E)
     (HC-T1 E)
     (HC-T2 E)
     (HC-Med E)))


(define-type (Tuple-Operation-Forms E)
  (U (Create-tuple (Listof E)) 
     (Tuple-proj E E)
     (Copy-Tuple E E)
     ;; FIXME These forms shouldn't exist anymore
     (Coerce-Tuple Uid E E)
     (Coerce-Tuple-In-Place Uid E E E E E)
     (Cast-Tuple Uid E E E E)
     (Cast-Tuple-In-Place Uid E E E E E E E)))

(define-type (Castable-Lambda-Forms E)
  (U (Lambda Uid* (Castable (Option Uid) E))
     (Fn-Caster E)
     (App-Fn E (Listof E))))

(define-type CoC3.1-Expr
  (Rec E
       (U (Castable-Lambda-Forms E)
          (Fn-Proxy-Forms E)
          (Letrec (Bnd* E) E)
          (Let (Bnd* E) E)
          (Var Uid) 
          (Global String)
          (Assign Id E)
          (Gen-Data-Forms E)
          (Code-Forms E)
          (Quote-Coercion Immediate-Coercion)
          (Coercion-Operation-Forms E)
          (Hyper-Coercion-Operation-Forms E)
          (Type Immediate-Type)
          (Type-Operation-Forms E)
          (Control-Flow-Forms E)
          ;;Primitives
          (Op Grift-Primitive (Listof E))
          (Quote Cast-Literal)
          No-Op
          ;; Observations
          (Blame E)
          (Observe E Grift-Type)
          (Unguarded-Forms E)
          (Guarded-Proxy-Forms E)
          (Monotonic-Forms E Immediate-Type)
          (Error E)
          (Tuple-Operation-Forms E))))

(define-type CoC3.1-Code (Code Uid* CoC3.1-Expr))
(define-type CoC3.1-Lambda (Castable-Lambda CoC3.1-Expr))
(define-type CoC3.1-Expr* (Listof CoC3.1-Expr))
(define-type CoC3.1-Bnd (Pairof Uid CoC3.1-Expr))
(define-type CoC3.1-Bnd* (Listof CoC3.1-Bnd))
(define-type CoC3.1-Bnd-Lambda (Pairof Uid CoC3.1-Lambda))
(define-type CoC3.1-Bnd-Lambda* (Listof CoC3.1-Bnd-Lambda))
(define-type CoC3.1-Bnd-Code (Pairof Uid CoC3.1-Code))
(define-type CoC3.1-Bnd-Code* (Listof CoC3.1-Bnd-Code))
(define-type CoC3.1-Bnd-Type (Pairof Uid Compact-Type))
(define-type CoC3.1-Bnd-Type* (Listof CoC3.1-Bnd-Type))
(define-type CoC3.1-Bnd-Crcn (Pairof Uid Compact-Coercion))
(define-type CoC3.1-Bnd-Crcn* (Listof CoC3.1-Bnd-Crcn))
          
