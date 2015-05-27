#lang typed/racket/base

#| 
This macro was created by Andrew Kent and is intended to sidestep some of the
problems that typed-racket has interacting with rackunit. 
|#

(provide (all-defined-out))

(define-syntax ann-chk
  (syntax-rules ()
    [(_ exp) (let ([exp-val exp])
               (if exp-val
                   (void)
                   (printf
                    "\n----------\nFAILURE\nExptected: ~a\nContext:\n  ~a:~a:~a\n----------\n"
                    "any non-false value"
                    (syntax-source #'exp)
                    (syntax-line #'exp)
                    (syntax-column #'exp))))]
    [(_ actual expected)
     (let ([exp1-val actual]
           [exp2-val expected])
       (if (not (eqv? exp1-val exp2-val))
           (printf
            "\n----------\nFAILURE (equal?)\nExptected: ~a\nActual: ~a\nContext:\n  ~a:~a:~a\n----------\n"
            exp1-val
            exp2-val
            (syntax-source #'actual)
            (syntax-line #'actual)
            (syntax-column #'actual))
           (void)))]))

(define-syntax-rule (chk~ exp)
  (let ([exp-val exp])
    (if exp-val
        (printf
         "\n----------\nFAILURE\nExptected: ~a\nActual: ~a\nContext:\n  ~a:~a:~a\n----------\n"
         #f
         exp-val
         (syntax-source #'exp)
         (syntax-line #'exp)
         (syntax-column #'exp))
        (void))))

(define-syntax-rule (chk-eq? exp1 exp2)
  (let ([exp1-val exp1]
        [exp2-val exp2])
    (if (not (eq? exp1-val exp2-val))
        (printf
         "\n----------\nFAILURE (eq?)\nExptected: ~a\nActual: ~a\nContext:\n  ~a:~a:~a\n----------\n"
         exp1-val
         exp2-val
         (syntax-source #'exp1)
         (syntax-line #'exp1)
         (syntax-column #'exp1))
        (void))))

(define-syntax-rule (chk-eqv? exp1 exp2)
  (let ([exp1-val exp1]
        [exp2-val exp2])
    (if (not (eqv? exp1-val exp2-val))
        (printf
         "\n----------\nFAILURE (eqv?)\nExptected: ~a\nActual: ~a\nContext:\n  ~a:~a:~a\n----------\n"
         exp1-val
         exp2-val
         (syntax-source #'exp1)
         (syntax-line #'exp1)
         (syntax-column #'exp1))
                (void))))
