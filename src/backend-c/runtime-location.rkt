#lang typed/racket
;; This is seperated out of code generator so that when that file is
;; compiled the paths of the runtime are available to invoke the c
;; compiler also.

(require racket/runtime-path (for-syntax racket))
(provide (all-defined-out))

;; primitive functions implemented in c
(: runtime.o-path Path)
(: runtime.c-path Path)
(: runtime.h-path Path)
(define-runtime-path runtime.o-path "runtime/runtime.o")
(define-runtime-path runtime.c-path "runtime/runtime.c")
(define-runtime-path runtime.h-path "runtime/runtime.h")

;; hashcons implementation in c
(: hashcons.o-path Path)
(: hashcons.c-path Path)
(: hashcons.h-path Path)
(define-runtime-path hashcons.o-path "runtime/hashcons.o")
(define-runtime-path hashcons.c-path "runtime/hashcons.c")
(define-runtime-path hashcons.h-path "runtime/hashcons.h")

(: boehm-gc.h-path Path)
(: boehm-gc.a-path Path)
(define-runtime-path boehm-gc-dir "runtime/boehm-gc")
(define-runtime-path boehm-gc-install "runtime/boehm-gc-install")
(define-runtime-path boehm-gc.h-path "runtime/boehm-gc-install/include/gc/gc.h")
(define-runtime-path boehm-gc.a-path "runtime/boehm-gc-install/lib/libgc.a")

(: none-gc.h-path Path)
(: none-gc.o-path Path)
(: none-gc.c-path Path)
(define-runtime-path none-gc.h-path "runtime/nonegc.h")
(define-runtime-path none-gc.o-path "runtime/nonegc.o")
(define-runtime-path none-gc.c-path "runtime/nonegc.c")


