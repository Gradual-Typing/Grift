#lang racket
(require pkg/lib)
(provide (all-defined-out))

(define schml-path (pkg-directory "Schml"))
(define testing-path (build-path schml-path "testing"))
(define test-suite-path (build-path testing-path "suite"))
