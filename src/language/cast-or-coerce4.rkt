#lang typed/racket/base
(require "forms.rkt" "primitives.rkt")

(provide (all-defined-out)
         (all-from-out "forms.rkt" "primitives.rkt"))
#|-----------------------------------------------------------------------------+
| Language/Cast3 created by interpret-casts                                    |
+-----------------------------------------------------------------------------|#

(define-type Cast-or-Coerce4-Lang
  (Prog (List String Natural Grift-Type)
    (Static* (List CoC4-Bnd-Mu-Type*
                   CoC4-Bnd-Type*
                   CoC4-Bnd-Mu-Crcn*
                   CoC4-Bnd-Crcn*
                   CoC4-Bnd-Data*)
             CoC4-Expr)))

(require "cast-or-coerce3.1.rkt")

(define-type (Named-Castable-Lambda-Forms E)
  (U (Letrec (Bnd* (Castable-Lambda E)) E)
     (Fn-Caster E)
     (App-Fn E (Listof E))))

(define-type CoC4-Expr
  (Rec E (U
          (Gen-Data-Forms E)
          (Code-Forms E)
          (Named-Castable-Lambda-Forms E)
          (Fn-Proxy-Forms E)
          (Quote-Coercion Immediate-Coercion)
          (Hyper-Coercion-Operation-Forms E)
          (Coercion-Operation-Forms E)
          (Type Immediate-Type)
          (Type-Operation-Forms E)
          (Let (Bnd* E) E)
          (Var Uid)
          (Global String)
          (Assign Id E)
          (Control-Flow-Forms E)
          (Op Grift-Primitive (Listof E))
          No-Op
          (Quote Cast-Literal)
          (Blame E)
          (Observe E Grift-Type)
          (Unguarded-Forms E)
          (Guarded-Proxy-Forms E)
          (Monotonic-Forms E Immediate-Type)
          (Error E)
          (Tuple-Operation-Forms E)
          )))

(define-type CoC4-Expr* (Listof CoC4-Expr))
(define-type CoC4-Code (Code Uid* CoC4-Expr))
(define-type CoC4-Bnd-Code (Pairof Uid CoC4-Code))
(define-type CoC4-Bnd-Code* (Listof CoC4-Bnd-Code))
(define-type CoC4-Lambda (Lambda Uid* (Castable (Option Uid) CoC4-Expr)))
(define-type CoC4-Bnd-Lambda  (Pairof Uid CoC4-Lambda))
(define-type CoC4-Bnd-Lambda* (Listof CoC4-Bnd-Lambda))
(define-type CoC4-Bnd-Data  (Pairof Uid CoC4-Expr))
(define-type CoC4-Bnd-Data* (Listof CoC4-Bnd-Data))
(define-type CoC4-Bnd-Type  (Pairof Uid Compact-Type))
(define-type CoC4-Bnd-Type* (Listof CoC4-Bnd-Type))
(define-type CoC4-Bnd-Crcn  (Pairof Uid Compact-Coercion))
(define-type CoC4-Bnd-Crcn* (Listof CoC4-Bnd-Crcn))
(define-type CoC4-Bnd-Mu-Type  (Pairof Uid Mu-Compact-Type))
(define-type CoC4-Bnd-Mu-Type* (Listof CoC4-Bnd-Mu-Type))
(define-type CoC4-Bnd-Mu-Crcn  (Pairof Uid Mu-Compact-Coercion))
(define-type CoC4-Bnd-Mu-Crcn* (Listof CoC4-Bnd-Mu-Crcn))
