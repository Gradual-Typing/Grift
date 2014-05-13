#lang racket


(provide (all-defined-out))

(struct File (name stx*)
        #:methods gen:custom-write
        [(define (write-proc x port mode)
           (fprintf port "(File:")
           (fprintf port "~a\n" (File-name x))
           (fprintf port "~a)" (File-stx* x)))])

;; For now I am hijacking the racket syntax system
(define (self-evaluating? x)
  (or (boolean? x) (number? x)))


;; The definitive typecheck for if an object is syntax to the compiler
(define (lang-syntax? x)
  (define (stx*? x)
    (or (null? x) (and (pair? x) (syntax? (car x)) (stx*? (cdr x)))))
  (and (File? x)
       (string? (File-name x))
       (stx*? (File-stx* x))))
