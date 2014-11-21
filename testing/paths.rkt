#lang typed/racket/base

(require/typed pkg/lib [pkg-directory (String . -> . (Option Path))])
(provide (all-defined-out))

;; This is currently only used for testing
;; but it determines the root directory for paths
(: schml-path Path)
(define schml-path (or (pkg-directory "schml") (current-directory)))

(: test-path Path)
(define test-path (build-path schml-path "testing"))

(: test-suite-path Path)
(define test-suite-path (build-path test-path "suite"))

(: test-tmp-path Path)
(define test-tmp-path (build-path test-path "tmp"))
