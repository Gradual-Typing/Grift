#lang racket

;; This benchmark uses the benchmark package for racket
(require racket/runtime-path
         racket/list
         math/statistics
         benchmark
         "../../../../../src/compile.rkt"
         "../../../../helpers.rkt")

;; runtime-paths are relative to this file when it is compiled
(define-runtime-path data-dir "data")
(define-runtime-path out-dir "output")
(define-runtime-path tmp-dir "tmp")
(define-runtime-path src-dir "src")

;; These keys are what are iterated over to control the benchmark
(define tests     '(call overhead))
(define compilers '(gambit coercions twosomes))
(define dynamic-operations? '(#f #t))

;; where is your gambit
(define gambit
  (or (find-executable-path "gambit")
      (find-executable-path "gsc")
      (error 'gambit "couldn't locate the gambit compiler")))

(module+ main
  ;; 10 miliseconds is the smallest time we accept for this test
  (define epsilon-parameter (make-parameter 10))
  (define iterations-parameter (make-parameter (* (expt 10 7) 1)))
  (define runs-parameter (make-parameter 100))
  (mem-dflt (expt 1024 3))
  (command-line
   #:once-each
   [("-i" "--iterations") number-of-iterations
    "number of iterations in timing loop"
    (iterations-parameter
     (or (string->exact-integer number-of-iterations)
         (error 'dynamic-function-call-benchmark "bad iterations argument")))]
   [("-r" "--runs") number-of-runs
    "number of trials of each benchmark"
    (runs-parameter
     (or (string->exact-integer number-of-runs)
         (error 'dynamic-function-call-benchmark "bad runs argument")))]
   [("-m" "--memory") memory-start-size
    "size in bytes of grift and gambit starting memory"
    (mem-dflt
     (or (string->exact-integer memory-start-size)
         (error 'dynamic-function-call-benchmark "bad memory argument")))]
   [("-e" "--epsilon") epsilon
    "smallest time in milliseconds that will be recorded without error"
    (epsilon-parameter
     (or (string->number epsilon)
         (error 'dynamic-function-call-benchmark "bad epsilon argument")))]
   #:args ()
   (dynamic-function-call-plot-and-latex!
    (run-dynamic-function-calls-benchmark
     (iterations-parameter)
     (runs-parameter)
     (epsilon-parameter)))))

(define (run-dynamic-function-calls-benchmark iterations number-of-runs epsilon)
  ;; Check that required source code exists
  (unless (directory-exists? src-dir)
    (error 'src-dir "no source directory found ~a" (path->string src-dir)))
  (for* ([test tests] [compiler compilers])
    (define src-path (make-src-path test compiler))
    (unless (file-exists? src-path)
      (error 'src-path "no source file found at ~a" (path->string src-path))))
  
  ;; Initialize directories if not yet done
  (unless (directory-exists? data-dir)
    (make-directory data-dir))
  (unless (directory-exists? out-dir)
    (make-directory out-dir))
  (unless (directory-exists? tmp-dir)
    (make-directory tmp-dir))
  
  (run-benchmarks
   tests
   `(,compilers ,dynamic-operations?)
   (run-benchmark iterations)
   #:skip dynamic-function-app-skip?
   #:build build-benchmark
   #:extract-time (parse-output iterations epsilon)
   #:num-trials number-of-runs
   #:results-file (build-path data-dir "funcall")))

(define (dynamic-function-app-skip? name compiler dynamic-ops?)
  (and (equal? compiler 'gambit) dynamic-ops?))



(define (dynamic-function-call-plot-and-latex! brs)
  (define brss (benchmark-results-stats brs))
  (pretty-print brss)
  (define brs-no-overhead
    (for*/fold ([brs-no '()]) ([c compilers] [d dynamic-operations?])
      (cond
        [(dynamic-function-app-skip? 'call c d) brs-no]
        [else
         (match-define (list mean-overhead _)
           (benchmark-results-stats-ref brss 'overhead (list c d)))
         (define br-wo (benchmark-results-ref brs 'call (list c d)))
         (define (subtract-overhead x) (- x mean-overhead))
         (define br-no
           (benchmark-result 'call (list c d) (map subtract-overhead br-wo)))
         (cons br-no brs-no)])))
  (define brs-no-stats (benchmark-results-stats brs-no-overhead))
  (pretty-print brs-no-stats))



(define (string->exact-integer x)
  (cond
    [(string->number x) => 
     (lambda (n) (and (exact-integer? n) n))]
    [else (error 'string->exact-integer "failed ~v" x)]))







(define (make-exe-path test compiler dyn-ops)
  (build-path
   tmp-dir
   (case compiler
     [(gambit) (format "s~a" test)]
     [(twosomes coercions) (format "~a" test)])))

(define (make-src-path test compiler)
  (build-path
   src-dir
   (case compiler
     [(gambit) (format "s~a.scm" test)]
     [(twosomes coercions) (format "~a.grift" test)])))

(define (build-benchmark test compiler dyn-ops)
  (define out-path (make-exe-path test compiler dyn-ops))
  (define src-path (make-src-path test compiler))
  (case compiler
    [(gambit)
     (define out-file (path->string out-path))
     (define src-file (path->string src-path))
     (unless (system* gambit
                      ;; set minimum heap size to same as grift
                      (format "-:m~a" (/ (mem-dflt) 1024))
                      ;; Compile to a standalone executable optimized
                      "-exe" "-cc-options" "-O3"            
                      "-o" out-file 
                      src-file)
       (error 'build-benchmarks/gambit
              "failed to compile gambit program: ~a" src-file))]
    [(twosomes coercions)
     (define (grift-compile rep dyn-ops)
       (define (help p m s) (format "~a.~a.~a" p m s))
       (define tmp-c-path
         (build-path tmp-dir (help test compiler "c")))
       (define tmp-a-path
         (build-path tmp-dir (help test compiler "s")))
       (compile src-path
                #:output out-path
                #:keep-c tmp-c-path
                #:keep-a tmp-a-path
                #:cast-rep rep
                #:mem (mem-dflt)
                #:cc-opt "-w -O3"
                #:dyn-ops dyn-ops))
     (define cast-rep (case compiler
                        [(twosomes) 'Twosomes]
                        [(coercions) 'Coercions]))
     (grift-compile cast-rep dyn-ops)]))

(define ((run-benchmark iterations) test compiler dyn-ops)
  (define exe-path (make-exe-path test compiler dyn-ops))
  (case compiler
    [(gambit)
     (unless (system* (path->string exe-path) (number->string iterations))
       (error 'run-benchmark "failed to run ~a" exe-path))]
    [else
     (with-input-from-string (~a iterations)
       (lambda ()
         (unless (system* (path->string exe-path))
           (error 'run-benchmark "failed to run ~a" exe-path))))]))


(define grift-spec #px"time \\(sec\\): (\\d+.\\d+)\nInt : 42")

(define gambit-spec #px"(\\d+) ms real time")

(define ((parse-output iterations epsilon) str)
  (define (->number tmp)
    (string->number
     (or (and (string? tmp) tmp)
         (and (bytes->string/utf-8 tmp))
         (error '->number "failed to parse number ~v" tmp))))
  
  (define grift-result? (regexp-match grift-spec str))
  (define gambit-result? (regexp-match gambit-spec str))

  (define run-result-in-milliseconds
    (cond
      [grift-result? (* (expt 10 3) (->number (cadr grift-result?)))] 
      [gambit-result? (->number (cadr gambit-result?))]
      [else
       (error 'parse-output
              "failed to parse: ~v ~v ~v"
              str gambit-result? grift-result?)]))

  (unless (> run-result-in-milliseconds epsilon)
    (error 'timer-epsilon
           "not within set timing epsilon: ~a ms"
           run-result-in-milliseconds))

  (define run-result-in-nanoseconds
    (* run-result-in-milliseconds (expt 10 6)))
  
  (/ run-result-in-nanoseconds iterations))

