#lang typed/racket
;; This is seperated out of code generator so that when that file is
;; compiled the paths of the runtime are available to invoke the c
;; compiler also.

(require racket/runtime-path (for-syntax racket))
(provide (all-defined-out))

;; primitive functions implemented in c
(: runtime.o-path Path)
(: runtime.h-path Path)
(define-runtime-path runtime.o-path "runtime/runtime.o")
(define-runtime-path runtime.h-path "runtime/runtime.h")

(: debug_runtime.o-path Path)
(: profile_runtime.o-path Path)
(define-runtime-path debug_runtime.o-path "runtime/debug_object/runtime.o")
(define-runtime-path profile_runtime.o-path "runtime/profile_object/runtime.o")

;; hashcons implementation in c
(: hashcons.o-path Path)
(: hashcons.c-path Path)
(: hashcons.h-path Path)
(define-runtime-path hashcons.o-path "runtime/hashcons.o")
(define-runtime-path hashcons.c-path "runtime/hashcons.c")
(define-runtime-path hashcons.h-path "runtime/hashcons.h")

(: cast_queue.o-path Path)
(: cast_queue.c-path Path)
(: cast_queue.h-path Path)
(define-runtime-path cast_queue.o-path "runtime/cast_queue.o")
(define-runtime-path cast_queue.c-path "runtime/cast_queue.c")
(define-runtime-path cast_queue.h-path "runtime/cast_queue.h")

(: suspended_cast.o-path Path)
(: suspended_cast.c-path Path)
(: suspended_cast.h-path Path)
(define-runtime-path suspended_cast.o-path "runtime/suspended_cast.o")
(define-runtime-path suspended_cast.c-path "runtime/suspended_cast.c")
(define-runtime-path suspended_cast.h-path "runtime/suspended_cast.h")

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

(define-runtime-path constants.h-path "runtime/constants.h")
(define-runtime-path cast-profiler.h-path "runtime/castprofiler.h")
(define-runtime-path cast-profiler.c-path "runtime/castprofiler.c")
(define-runtime-path cast-profiler.o-path "runtime/castprofiler.o")


(define-runtime-path runtime-entry.c-path "runtime/entry.c")
(define runtime-entry-point : (Parameterof (Option Path))
  (make-parameter #f))
