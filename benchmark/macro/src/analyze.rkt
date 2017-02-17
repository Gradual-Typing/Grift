#lang racket 

(require math/statistics 
	 racket/runtime-path
         "shared.rkt"
	 file/glob)

(define langs '("c" "gambit" "racket"
		"schml/static/coercions"
		"schml/static/casts"
	 	"schml/dynamic/coercions"
		"schml/dynamic/casts"))

(define (print-stat-layout-to-port p)
 (define data-layout "#language, mean(sec), stddev(sec)\n")
  (display data-layout p)
  (display data-layout))

;; TODO This needs to be result driven instead of input driven
(define number-of-runs (make-parameter #f))
(command-line
  #:once-each
  [("--assert-runs") runs
    "check the number of runs"
    (let ([runs? (string->number runs)])
      (unless runs? (error '--assert-runs "invalid: ~v" runs))
      (number-of-runs runs?))]
 #:args ()
 (display (glob "{c,gambit,racket,schml}/{,static/,dynamic/}*.err"))
 (unless (directory-exists? "stats")
  (make-directory "stats"))
 (for* ([input (in-glob "inputs/*")])
   (printf "~a\n" input)
   (define-values (_1 filename _2) (split-path input))
   (define benchmark (path->string filename))
  (call-with-output-file      
   #:exists 'replace
   (general-stats-path benchmark)
   (lambda (p)
    (print-stat-layout-to-port p)
    (for* ([lang (in-list langs)])
      (define err-output-path
            (build-path root-dir lang "out" (string-append benchmark ".err")))
      (define err-output (file->string err-output-path))
      (define runtimes (parse-runtimes err-output))
      (when (and (number-of-runs)
      	    	 (not (= (length runtimes) (number-of-runs))))
	  (error 'avg-n-body "~a" runtimes))
      (define avg (mean runtimes))
      (define std (stddev runtimes))
      (printf "~a\n  mean(sec)=~a\n  sdev(sec)=~a\n~a\n\n"
        lang avg std runtimes)
      (display (format "~a, ~a, ~a\n" lang avg std) p))))))

  
