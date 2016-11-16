#lang racket 

(require math/statistics
	 racket/runtime-path)

(define langs '("c" "gambit" "racket"
		"static/coercions"
		"static/casts"
	 	"dynamic/coercions"
		"dynamic/casts"))

(define runs 10)

(define-runtime-path this-dir ".")
(define time-rx #px"time \\(sec\\) (\\d+.\\d+)")

(define (read-runtimes-from-port str)
 (for/list ([m (regexp-match* time-rx str)])
  (match (regexp-match time-rx m)
   [(list _ (app string->number (? number? seconds))) 
    seconds]
   [other (error 'todo "~a" other)])))

(for ([l langs])
  (define err-path (build-path this-dir l "logs/n-body.err"))
  (define err-out (file->string err-path))
  (define runtimes (read-runtimes-from-port err-out))
  (unless (= (length runtimes) runs)
      (error 'avg-n-body "~a" runtimes))
   (define avg (mean runtimes))
   (define std (stddev runtimes))
  (printf "~a:\n avg=~a\n std=~a\n" l avg std))
 
  