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
    [reduce-to-cast-calculus (Path -> Cast0-Lang)])
  (module+ test
    (: test0 : -> C0-Expr)
    (define (test0)
      (Let `((,(Uid "l" 0)
              . ,(Lambda (list (Uid "f" 2))
                   (Lambda (list (Uid "n" 3))
                     (Dyn-Fn-App (Var (Uid "f" 2))
                                 (list (Var (Uid "n" 3)))
                                 (list (Int))
                                 "blah"))))
             (,(Uid "m" 1)
              . ,(Lambda (list (Uid "n" 4))
                   (Cast (Var (Uid "n" 4)) (Twosome (Dyn) (Int) "Mistake")))))
        (App (App (Var (Uid "l" 0))
                  (list (Cast (Var (Uid "m" 1))
                              (Twosome (Fn 1 (list (Dyn)) (Int))
                                       (Dyn)
                                       "blah2"))))
             (list (Quote 5)))))))

(define (reduce-to-cast-calculus path)
  (let* ((stx-lang (read path))
	 (s0 (syntax->schml0 stx-lang))
         (s1 (type-check s0)))
    (insert-casts s1)))


