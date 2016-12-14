#lang racket/base

#|
This is a micro compiler that takes a path, reads the contents
of that file, parses the syntax into an ast, type-checks the
ast, and finally converts that ast into an equivalent ast
of the cast calculus.
|#

(require "./read.rkt"
         "./syntax-to-schml0.rkt"
         "./type-check.rkt"
         "./insert-casts.rkt")
(provide reduce-to-cast-calculus)


(module* typed typed/racket/base
  (require "../language/cast0.rkt")
  (provide (all-from-out "../language/cast0.rkt"))
  (require/typed/provide (submod "..")
    [reduce-to-cast-calculus (Path -> Cast0-Lang)]))

(define (reduce-to-cast-calculus path)
  (let* ((stx-lang (read path))
	 (s0 (syntax->schml0 stx-lang))
         (s1 (type-check s0)))
    (insert-casts s1)))


