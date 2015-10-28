#lang racket/base

(require "../../src/macros.rkt"
         "../../src/errors.rkt")

(provide (all-defined-out)
         (all-from-out "../../src/errors.rkt"))

;; Defined as a syntax-rule so that the raise-blame stack frame
;; is omitted in the stack trace.
(define-syntax-rule (raise-blame l)
  (raise (exn:schml:type:dynamic l (current-continuation-marks))))

(define-syntax-rule (raise-repeat-type-unenforced value exp)
  (error 'schml-compiler
         (concat-string-literal
          "schml didn't enforce repeat typing rule in ~a\n"
          "\tvalue ~a is not of type Integer")
         exp value))

(define-syntax-rule (raise-delta-types-unenforced prim val*)
  (error 'schml-compiler
         (concat-string-literal
          "schml didn't enforce ~a typing rule.\n"
          "\t in ~a")
         prim (format "~a" (cons prim val*))))

(define-syntax-rule (raise-insanity who s ...)
  (error 'who "this should never happen ~a" `(,s ...)))
