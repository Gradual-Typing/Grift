#lang racket/base

(provide delta initialize-state)

(require racket/match
         racket/fixnum  
         "./errors.rkt")



;; given a primitive and a list of arguments to the primitive
;; it will return the result of the primitive
(define (delta p v*)
  (case p
    [(=)
     (match v*
       [(list (? integer? m) (? integer? n)) (= m n)]
       [(list m n) (eq? m n)]
       [other (raise-delta-types-unenforced p v*)])]
    [(+ - * < > >= <= %/)
     (match v*
       [(list (? integer? m) (? integer? n))
        (case p 
          [(+) (+ m n)]
          [(-) (- m n)]
          [(*) (* m n)]

          [(<) (< m n)]
          [(>) (> m n)]
          [(>=) (>= m n)]
          [(<=) (<= m n)]
          [(%/)
           (when (= n 0)
             (error 'interp-div0 "Raise a better error here"))
           (quotient m n)]
          [else (error 'delta "this should never happen" p)])]
       [otherwise (raise-delta-types-unenforced p v*)])]
    [(%>> %<< binary-and binary-or)
     (match v*
       [(list (? fixnum? m) (? fixnum? n))
        (case p
          [(%>>) (fxrshift m n)]
          [(%<<) (fxlshift m n)]
          [(binary-and) (fxand m n)]
          [(binary-or) (fxior m n)]
          [else (error 'delta "this should never happen" p)])]
       [otherwise (raise-delta-types-unenforced p v*)])]
    [(timer-start timer-stop timer-report)
     (match v*
       [(list)
        (case p
          [(timer-start)  (timer-start)]
          [(timer-stop)   (timer-stop)]
          [(timer-report) (timer-report)]
          [else (error 'delta "this should never happen" p)])]
       [otherwise (raise-delta-types-unenforced p v*)])]
    [else (error 'delta "this should never happen" p)]))

(define (initialize-state)
  (set! timer-started? #f)
  (set! timer-stopped? #f)
  (set! start-time 0.0)
  (set! stop-time 0.0))

(define start-time ;;: Real
  0.0)
(define stop-time  ;;: Real
  0.0)

(define timer-started? ;;: Boolean
  #f)

(define timer-stopped?  ;;: Boolean
  #f)

(define (timer-start)
  (set! timer-started? #t)
  (set! start-time (current-inexact-milliseconds))
  '())

(define (timer-stop)
  (set! stop-time  (current-inexact-milliseconds))
  (set! timer-stopped? #t)
  '())

(define (timer-report)
  (when (not timer-started?)
    (raise (exn:schml:type:dynamic "timer not started" (current-continuation-marks))))
  (when (not timer-stopped?)
    (raise (exn:schml:type:dynamic "timer not stopped" (current-continuation-marks))))
  '())
