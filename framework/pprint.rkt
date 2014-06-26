#lang racket
(require (planet dherman/pprint:4) 
                        racket/generic 
                        racket/format)
(provide (all-from-out (planet dherman/pprint:4) 
                        racket/generic 
                        racket/format)
         (all-defined-out))

(define gen-indent-size 4)
(define prog-indent-size 2)
(define lambda-indent-size 2)
(define let-indent-size 2)
(define begin-indent-size 2)

(define (doc-list a)
  (h-append lparen a rparen))
(define (format->doc x) (text (~a x)))

(define-generics pretty
  (->doc pretty)
  #:fallbacks
  [(define (->doc pretty) (text (~a pretty)))])


