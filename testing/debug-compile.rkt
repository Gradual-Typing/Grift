#lang typed/racket/base

(require schml/compiler/compile
         schml/compiler/helpers)

(require/typed schml/testing/paths [test-tmp-path Path])

(traces '(All))

(: cc (-> String Boolean))
(define (cc t)
  (compile (build-path "/Users/akuhlens/Work/schml/testing/suite/" t)
           #:exec-path (build-path test-tmp-path "d")
           #:c-path    (build-path test-tmp-path "d.c")
           #:log-path  (build-path test-tmp-path "d.log.txt")))

(module+ main
  (unless (directory-exists? test-tmp-path)
    (make-directory test-tmp-path))
  (let ([args (current-command-line-arguments)])
    (cond
     [(= 0 (vector-length args)) (display "please specify what file to compile!\n")]
     [(< 1 (vector-length args)) (display "please only specify one file to compile!\n")]
     [(cc (vector-ref args 0)) (display "success :)\n")]
     [else (display "success :)\n")])))
 

