#lang racket

(provide snoc)
(define (snoc xs x)
  (foldr cons (list x) xs))

(module+ test
  (require rackunit)
  (check-equal? (snoc '(1 2 3) 4) '(1 2 3 4))
  (check-equal? (snoc '() 1) '(1)))
