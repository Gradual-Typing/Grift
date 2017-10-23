#lang typed/racket/base
(require "forms.rkt"
         "primitives.rkt"
         "data1.rkt")
(provide (all-defined-out)
         (all-from-out "forms.rkt" "primitives.rkt"))

#|-----------------------------------------------------------------------------+
| Data2-Language created by remove-let                                          |
+-----------------------------------------------------------------------------|#

(define-type Data2-Lang
  (Prog (List String Natural Grift-Type)
        (GlobDecs Uid*
                    (Labels D2-Bnd-Code*
                            D2-Body))))

(define-type D2-Body (Locals Uid* D2-Tail))
(define-type D2-Bnd-Code* (Listof D2-Bnd-Code))
(define-type D2-Bnd-Code (Pairof Uid D2-Code))
(define-type D2-Code (Code Uid* D2-Body))

(define-type D2-Tail D1-Tail)

(define-type D2-Value D1-Value)

(define-type D2-Pred D1-Pred)

(define-type D2-Effect D1-Effect)

(define-type D2-Value* (Listof D2-Value))

(define-type D2-Effect* (Listof D2-Effect))

(define-type D2-Literal Data-Literal)

