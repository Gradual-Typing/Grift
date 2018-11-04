#lang typed/racket/base
(require "forms.rkt" "primitives.rkt"
         "lambda0.rkt"
         "cast-or-coerce3.1.rkt"
         "cast-or-coerce4.rkt")

(provide (all-defined-out)
         (all-from-out "cast-or-coerce3.1.rkt")
         (all-from-out "forms.rkt" "primitives.rkt"))

#|------------------------------------------------------------------------------
Cast-or-Coerce3.2-Lang is the product of purify-letrec
------------------------------------------------------------------------------|#

(define-type Cast-or-Coerce3.2-Lang
  (Prog (List String Natural Grift-Type)
    (Static* (List
              Bnd-Mu-Type*
              Bnd-Type*
              Bnd-Mu-Crcn*
              Bnd-Crcn*
              CoC3.2-Bnd*) 
             CoC3.2-Expr)))

(define-type CoC3.2-Expr
  (Rec E
       (U (Named-Castable-Lambda-Forms E)
          (Castable-Lambda E)
          (Fn-Proxy-Forms E)
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
          (Op Grift-Primitive (Listof E))
          (Quote Cast-Literal)
          No-Op
          (Blame E)
          (Observe E Grift-Type)
          (Unguarded-Forms E)
          (Guarded-Proxy-Forms E)
          (Monotonic-Forms E Immediate-Type)
          (Error E)
          (Tuple-Operation-Forms E))))
     

(define-type CoC3.2-Code (Code Uid* CoC3.2-Expr))
(define-type CoC3.2-Lambda (Castable-Lambda CoC3.2-Expr))
(define-type CoC3.2-Expr* (Listof CoC3.2-Expr))
(define-type CoC3.2-Bnd (Pairof Uid CoC3.2-Expr))
(define-type CoC3.2-Bnd* (Listof CoC3.2-Bnd))
(define-type CoC3.2-Bnd-Code (Pairof Uid CoC3.2-Code))
(define-type CoC3.2-Bnd-Code* (Listof CoC3.2-Bnd-Code))
(define-type CoC3.2-Bnd-Lambda (Pairof Uid CoC3.2-Lambda))
(define-type CoC3.2-Bnd-Lambda* (Listof CoC3.2-Bnd-Lambda))
(define-type CoC3.2-Bnd-Type (Pairof Uid Compact-Type))
(define-type CoC3.2-Bnd-Type* (Listof CoC3.2-Bnd-Type))
(define-type CoC3.2-Bnd-Crcn (Pairof Uid Compact-Coercion))
(define-type CoC3.2-Bnd-Crcn* (Listof CoC3.2-Bnd-Crcn))

(define-type CoC3.2-Gen-Data
  (U Dyn))
(define-type CoC3.2-Gen-Ctor
  (U Dyn-Repr-Ctor))
(define-type CoC3.2-Gen-Access
  (U Dyn-Repr-Access))
(define-type CoC3.2-Gen-Pred
  (U Dyn-Repr-Pred))          
