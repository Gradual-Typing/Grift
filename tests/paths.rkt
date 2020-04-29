#lang typed/racket

(require/typed pkg/lib [pkg-directory (String . -> . (Option Path))])

(require racket/runtime-path)

(provide (all-defined-out))

;; This is currently only used for testing
;; but it determines the root directory for paths
(: tests-path Path)
(define-runtime-path tests-path ".")

(: test-suite-path Path)
(define-runtime-path test-suite-path "suite/")

(: test-tmp-path Path)
(define-runtime-path test-tmp-path "tmp/")
