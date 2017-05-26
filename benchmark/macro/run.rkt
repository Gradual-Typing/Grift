#!racket
#lang typed/racket

;; for each benchmark
;; shell script that compiles, and runs benchmark n-times adding
;; compile, run, check-output, parse-output, record-time
;; compile / run implemented in shell
(: benchmark : path number -> Void)
(define (do-benchmark b number-of-runs)
  (define times (benchmark-times b))
  (define current-number-of-times (count-times times))
  (define needed-number-of-times (- number-of-runs (benchmark-times b)))
  (for ([i (in-range 0 needed-number-of-times)])
    (define time (compile/run-benchmark b))
    (save-time times time)))

(define-syntax-rule (call-with-temporary-file f)
  (let ([tmp-path (make-temporary-file)])
    (f tmp-path)
    (delete-file tmp-path)))

(define (compile/run-benchmark b input-type)
  (define in-path (benchmark-input b in))
  (define args (benchmark-args b in))
  (define out-path (make-tmp-file (benchmark-tmp-fmt b) #f (error-output-dir)))
  (define err-path (make-tmp-file (benchmark-tmp-fmt b) #f (error-output-dir)))
  (d)
  (call-with-temporary-file
   (lambda (p)
     ;; Compile/Run file capturing in temp files
     (with-output-to-file p
       (lambda ()
         (system (benchmark-compile/run-script b in err out))))
     ;; Parse / Temporary file
     (lambda (benchmark-output)
       (call-with-input-file (benchmark-regexp b)
         (lambda (rx)
           (match (regexp-match rx p)
               [(list _ time-str) time-str]
               [other
                (error 'compile/run-benchmark
                       "parse error: ~s ~s"
                       benchmark-rx benchmark-output)]))))
     ))
  (system (benchmark-compile/run-script b)))

(define (parse-output))


