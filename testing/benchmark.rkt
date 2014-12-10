#lang typed/racket/base

(require schml/src/compile
         schml/src/helpers
         schml/testing/paths
         schml/testing/values
         racket/port)

(: run-and-log (-> Any))
(define (run-and-log) (display "Test!\n"))

(: correct? (-> Config Test-Value Boolean))
(define (correct? configuration expected-out)
  (value=? (observe (envoke-compiled-program #:config configuration)) expected-out))

(: benchmark (-> Positive-Integer Path Config Test-Value Void))
(define (benchmark runs path config expected)
  (compile/conf path config)
  (if (correct? config expected)
      (for [(i (in-range 0 runs))] (run-and-log))
      (logf "[Error] ~a gave unexpected output\n" (path->string path))))


(module+ main
  (call-with-output-file (build-path test-tmp-path "benchmarks-log.txt")
   (lambda (log)
     (parameterize ([current-log-port log])
       (benchmark 5
                  (build-path test-suite-path "fact5.schml")
                  (Config 'Lazy-D
                          (build-path test-tmp-path "b.out")
                          (build-path test-tmp-path "b.c"))
                  (integ 120))))))
