#lang typed/racket
;; This is seperated out of code generator so that when that file is
;; compiled the paths of the runtime are available to invoke the c
;; compiler also.

(require racket/runtime-path)
(provide (all-defined-out))

(: runtime.o-path Path)
(define-runtime-path runtime.o-path "runtime.o")

(: runtime.c-path Path)
(define-runtime-path runtime.c-path "../runtime.c")
