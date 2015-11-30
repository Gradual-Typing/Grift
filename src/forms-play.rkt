#lang racket

(require "forms-untyped.rkt")
(require (only-in "forms-typed.rkt" Prog Letrec App Lambda Ascribe Quote))

(define (test-expr e)
  (if (Var? e)
      (Var (Uid "foo" 1))
      (form-map ptable e)))

(define (test-bnd* b*)
  (map (lambda (p) (cons (Uid "foo" 1) (form-map ptable (cdr p)))) b*))

(define (test-fml* f*)
  (map (lambda (f) (Uid "foo" 1)) f*))

(define ptable (hash 'expression test-expr
                     'formals test-bnd*
                     'bindings test-fml*))

(define test
  (Prog '()
   (Letrec `((,(Quote 1) . ,(App (Var (Uid "bar" 5)) (list (Var (Uid "baz" 4))))))
           (App (Lambda '(,(Uid "fing" 5)) (Var (Uid "barf" 10)))
                (list (Ascribe (Var (Uid "bing" 10)) (Quote 'Int) "Hi"))))))

(define test-map
  (form-map ptable test))