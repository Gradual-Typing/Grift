#lang racket

;; There is a very small amount of functionality
;; coming from csv-reader consider going without
;; and hand coding a solution once this works
(require file/glob
	 math/statistics
	 "shared.rkt"
         racket/runtime-path)

(module+ main
  (command-line
   #:once-each
   [("--assert-runs") runs
    "check the number of runs"
    (let ([runs? (string->number runs)])
      (unless runs? (error '--assert-runs "invalid: ~v" runs))
      (number-of-runs runs?))]
   [("--assert-lattice-samples") samples
    "check the number of runs"
    (let ([samples? (string->number samples)])
      (unless samples? (error '--assert-samples "invalid: ~v" samples)) 
      (number-of-lattice-samples samples?))]
   [("--baseline") baseline
    "The configuration to use as the baseline for slowdown"
    (slowdown-baseline baseline)]
   #:args () (analyze-lattice)))


;; All results written into ./lattice/<benchmark>/
(define-runtime-path output-dir "lattice")

;; the configuration to be used as the baseline for slowdown calculations
(define slowdown-baseline (make-parameter "gambit"))

;; when not false, a check occurs to ensure the number of runs
;; extracted from logs match the expected number of runs.
(define number-of-runs (make-parameter #f))

;; when not false, does nothing right now
(define number-of-lattice-samples (make-parameter #f))


;; - extract timing info from the output logs of schml/partial
;; - build plots based on this data
;; - summarize findings
;; Generates some output but all real output is save in the outputs-dir
(define (analyze-lattice) 
  (define partial-benchmark-stats
    (for/list ([f (in-glob "schml/partial/*/out/*/*.err")])
      ;; Bind the variable portions of the path
      (define rx #px"schml/partial/(\\w+)/out/((\\w|-)+)/(\\d)+.err")
      (define-values (rel-path impl benchmark file)
        (match (regexp-match rx f)
          [(list r i b _ f) (values r i b f)]
          [other (error 'analyze-lattice "unmatched: ~a" other)]))
      (define src-file-path
	(build-path "schml/partial/src"
		    benchmark
		    (string-append file ".schml")))
      (define percent-typed
	(match (regexp-match #px";; (\\d+\\.\\d+) %"
	       		     (file->string src-file-path))
          [(list _ (app string->number (? number? n)))
           (exact->inexact n)]
          [other (error 'percent-typed "unmatched: ~v" other)]))
      (define baseline-mean-time
        (let ([baseline (slowdown-baseline)])
          (define (err)
            (error 'lattice-analysis
                   "couldn't fine stats to caculate slowdowns: ~v"
                   baseline))
          (stats-mean-time-ref benchmark baseline err)))
      
      (define time-measurements (parse-runtimes (file->string rel-path)))
      (define number-of-measurements (length time-measurements))
      (unless (or (not (number-of-runs))
      	      	  (= (number-of-runs) number-of-measurements))
	(error 'analyze-lattice "incorrect number of runs parsed: ~a"
	       rel-path))
      (define mean-time  (mean time-measurements))
      (define sdev-time  (stddev time-measurements))
      (define slowdown   (/ mean-time baseline-mean-time))
      (list benchmark impl file percent-typed
	    number-of-measurements
	    mean-time sdev-time slowdown)))

  (display (list-of-lists->csv
            (cons `("#benchmark, implementation, instance, percent-typed, runs, mean(sec), stddev, slowdown")
                  partial-benchmark-stats)))
  (newline)
  
  ;; map seen benchmarks to a map of implementaions to data points
  (define structured-stats ;; (Listof (Pair Benchmark (Listof (Pair Impl Inst-Data))))
    (for/list ([benchmark (list->set (map car partial-benchmark-stats))])
      (define (this-benchmark? x) (equal? (car x) benchmark))
      (define these-benchmarks
        (map cdr (filter this-benchmark? partial-benchmark-stats)))
      (define implementations
        (for/list ([implementation (list->set (map car these-benchmarks))])
          (define (this-implementation? x) (equal? (car x) implementation))
          (cons implementation
                (map cdr (filter this-implementation? these-benchmarks)))))
      (cons benchmark implementations)))

  (print structured-stats)
  
  ;; foreach benchmark create plots and summary statistics
  (for* ([benchmark structured-stats])
    (match-define (cons bm-name bm-data) benchmark)
    (plot-cumulative bm-name bm-data)
    (plot-scatter bm-name bm-data)
    (summarize bm-name bm-data)))

(define (plot-cumulative benchmark data)
  (define output-path (build-path output-dir benchmark "cumulative.png"))
  (define ((write-data/find-max implementation) data)
    (define implementation-path
      (build-path output-dir benchmark implementation "cumulative.csv"))
    (define slowdowns 
      (match data
        [(list (list _ ... sd*) ...) sd*]))
      (define sort-uniq-slowdowns (sort (uniques slowdowns) <))
      (define max-slowdown (last sort-uniq-slowdowns))
      (define slowdowns/counts
        (for/list ([sd sort-uniq-slowdowns])
          (list sd (length (filter ((curry equal?) sd) slowdowns)))))
      (make-containing-directory implementation-path)
      (call-with-output-file #:exists 'replace
        implementation-path
        (lambda (p) (list-of-lists->csv slowdowns/counts p)))
      (values implementation-path max-slowdown))
    
  (define-values (type-base-path coercion-path max-slowdown)
    (match data
      [(list-no-order
        (cons "coercions" (app (write-data/find-max "coercions") cpath cmax))
        (cons "casts"     (app (write-data/find-max "casts")     tbpath tbmax))
        _ ...)
       (values tbpath cpath (max cmax tbmax))]
      [other (error 'path-extraction "invalid: ~a" other)]))
  
  (run-gnuplot-script 'cumulative-performance-lattice.gp
                      `((tDataFile . ,(path->string type-base-path))
                        (cDataFile . ,(path->string coercion-path))
                        (oFile . ,(path->string output-path))
                        (titleString . ,benchmark)
                        (yMax . ,max-slowdown))))



(define (plot-scatter benchmark data)
  (define output-path (build-path output-dir benchmark "scatter.png"))
  (define ((write-data-file implementation) implementation-data)
    (define implementation-path
      (build-path output-dir benchmark implementation "instance-data.csv"))
    (make-containing-directory implementation-path)
    (call-with-output-file #:exists 'replace
      implementation-path
      (lambda (p) (list-of-lists->csv implementation-data p)))
    implementation-path)

  (define-values (type-base-path coercions-path)
    (match data
      [(list-no-order
        (cons "coercions" (app (write-data-file "coercions") cpath))
        (cons "casts"     (app (write-data-file "casts")     tbpath))
        _ ...)
       (values tbpath cpath)]
      [other (error 'path-extraction "invalid: ~a" other)]))
  
  (run-gnuplot-script 'scatter-performance-lattice.gp
                      `((tDataFile . ,(path->string type-base-path))
                        (cDataFile . ,(path->string coercions-path))
                        (oFile . ,(path->string output-path))
                        (titleString . ,benchmark))))

(define (summarize benchmark data)
  (define title benchmark)
  (define-values (type-nodes less-precise)
    (values "" ""))
  (define ((err x))
    (error 'stats-table "unfound ~v" x))
  (define stats-table
    (file->stats-table (general-stats-path benchmark)))
  (define (mean-ref x) (car (hash-ref stats-table x (err x))))
  (define c-avg (mean-ref "c"))
  (define gambit-avg (mean-ref "gambit"))
  (define static/type-base-avg (mean-ref "schml/static/casts"))
  (define static/coercions-avg (mean-ref "schml/static/coercions"))
  (define dynamic/type-base-avg (mean-ref "schml/dynamic/casts"))
  (define dynamic/coercions-avg (mean-ref "schml/dynamic/coercions"))
  (define type-base-static/dynamic
    (/ dynamic/type-base-avg static/type-base-avg))
  (define coercions-static/dynamic
    (/ dynamic/coercions-avg static/coercions-avg))
  (define-values (type-base-min type-base-max type-base-avg
                  coercions-min coercions-max coercions-avg)
    (match data
      [(list-no-order
        (cons "coercions" (list (list _ ... c-sd*) ...))
        (cons "casts"     (list (list _ ... tb-sd*) ...))
        _ ...)
       (values (apply min tb-sd*) (apply max tb-sd*) (mean tb-sd*)
               (apply min c-sd*) (apply max c-sd*) (mean c-sd*))]
      [other (error 'path-extraction "invalid: ~a" other)]))
  (with-output-to-file #:exists 'replace
    (build-path output-dir benchmark "summary.tex")
    (lambda ()
      (printf
       #<<EndOfString
\begin{tabular}{|l|l|l|}
    \hline
    \textbf{~a} & 
      \multicolumn{2}{l|}{(~a type nodes)}                  \\ \hline
    lattice size & \multicolumn{2}{l|}{~a B}                \\ \hline
    \multicolumn{3}{|l|}{}                                  \\ \hline
    Clang                       & \multicolumn{2}{l|}{~ax}  \\ \hline
    Gambit-C                    & \multicolumn{2}{l|}{~ax}  \\ \hline
    \multicolumn{3}{|l|}{}                                  \\ \hline
    typed/untyped ratio         & ~ax  & ~ax                \\ \hline
    min. slowdown               & ~ax  & ~ax                \\ \hline
    max. slowdown               & ~ax  & ~ax                \\ \hline
    mean slowdown               & ~ax  & ~ax                \\ \hline
  \end{tabular}
\end{table}
EndOfString
       title type-nodes less-precise
       c-avg
       gambit-avg
       type-base-static/dynamic coercions-static/dynamic
       type-base-min            coercions-min
       type-base-max            coercions-max
       type-base-avg            coercions-avg)))
  )






  






