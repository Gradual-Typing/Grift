#lang typed/racket/base

#|
This is a micro compiler that takes a path, reads the contents
of that file, parses the syntax into an ast, type-checks the
ast, and finally converts that ast into an equivalent ast
of the cast calculus.
|#

(require "../language/cast0.rkt")
(provide (all-from-out "../language/cast0.rkt"))

(module untyped racket/base
  (require "./insert-casts.rkt"
           "./read.rkt"
           "./syntax-to-grift0.rkt"
           "./type-check.rkt")
  (provide reduce-to-cast-calculus)
  (define (reduce-to-cast-calculus path)
    (insert-casts (type-check (syntax->grift0 (read path))))))

(require/typed/provide 'untyped
  [reduce-to-cast-calculus ((U String Path) -> Cast0-Lang)])



