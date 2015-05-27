#lang typed/racket/base

(require schml/src/compile
         schml/src/helpers
         schml/testing/test-compile)

(require/typed schml/testing/paths
               [test-tmp-path Path]
               [test-suite-path Path])

(define log (open-output-file (build-path test-tmp-path "d.log.txt")
                              #:exists 'replace))

(: cc (-> String Boolean))
(define (cc path)
  (let ([path (build-path path)])
    (parameterize ([current-log-port log]
                   [traces '(All Vomit)])
      (test-compile "debug" path (debug))
      #t)))

(module+ main
  (unless (directory-exists? test-tmp-path)
    (make-directory test-tmp-path))
  (let ([args (current-command-line-arguments)])
    (cond
     [(= 0 (vector-length args)) (display "please specify what file to compile!\n")]
     [(< 1 (vector-length args)) (display "please only specify one file to compile!\n")]
     [(cc (vector-ref args 0)) (display "success :)\n")]
     [else (display "success :)\n")])))
