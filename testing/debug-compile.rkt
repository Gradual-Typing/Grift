#lang typed/racket/base

(require schml/compiler/compile)
(require schml/compiler/helpers)


(traces '(All))

(: cc (-> String Boolean))
(define (cc t)
  (compile (build-path "/Users/akuhlens/Work/schml/testing/suite/" t)
           #:exec-path (build-path "test")
           #:c-path    (build-path "test.c")
           #:log-path  (build-path "test.log.txt")))

(module+ main
  (let ([args (current-command-line-arguments)])
    (cond
     [(= 0 (vector-length args)) (display "please specify what file to run!\n")]
     [(< 1 (vector-length args)) (display "please only specify one file to run!\n")]
     [(cc (vector-ref args 0)) (display "success :)\n")]
     [else (display "success :)\n")])))
 

