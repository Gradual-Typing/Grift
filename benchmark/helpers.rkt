#lang racket

(require "../src/compile.rkt" math/statistics)

(provide (all-defined-out))


(struct benchmark-result (name opts trial-times) #:prefab)

(define (benchmark-results-ref brs name opts)
  (define (this-one? x)
    (and (benchmark-result? x)
         (equal? (benchmark-result-name x) name)
         (equal? (benchmark-result-opts x) opts)))
  (cond
    [(not (list? brs)) (error 'benchmark-results-ref)]
    [(findf this-one? brs) => benchmark-result-trial-times]
    [else (error 'benchmark-result-ref "invalid index ~a ~a" name opts)]))

(define (benchmark-results-stats brs)
  (for/list ([br brs])
    (define trials (benchmark-result-trial-times br))
    (list (benchmark-result-name br) (benchmark-result-opts br)
          (exact->inexact (mean trials))
          (exact->inexact (stddev trials)))))

(define (benchmark-results-stats-ref brss test opts)
  (define (this-one? x)
    (and (pair? x) (equal? test (car x))
         (pair? (cdr x)) (equal? opts (cadr x))))
  (cond
    [(not (list? brss)) (error 'benchmark-results-ref)]
    [(findf this-one? brss) => cddr]
    [else (error 'benchmark-result-ref "invalid index ~a ~a" test opts)]))
