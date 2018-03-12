#lang typed/racket

;; This module defines macros that construct pieces of our AST and uses
;; constants from constants-and-codes.rkt module.


(require (for-syntax racket/syntax)
         "forms.rkt"
         "../casts/constants-and-codes.rkt")

(provide (all-defined-out))

(define-syntax-rule (ref-coercion$ c1 c2) (Ref-Coercion c1 c2 COERCION-REF-REF-FLAG))
(define-syntax-rule (vec-coercion$ c1 c2) (Ref-Coercion c1 c2 COERCION-REF-VEC-FLAG))
