#lang racket

;; This benchmark uses the benchmark package for racket
(require racket/runtime-path
         racket/list
         math/statistics
         benchmark
         racket/date
         "../../../../../src/compile.rkt"
         "../../../../helpers.rkt")

;; runtime-paths are relative to this file when it is compiled
(define-runtime-path data-dir "data")
(define-runtime-path out-dir "output")
(define-runtime-path tmp-dir "tmp")
(define-runtime-path src-dir "src")

;; These keys are what are iterated over to control the benchmark
(define tests               '(Ref RefOverhead))
(define compilers           '(Gambit Coercions Twosomes))
(define dynamic-operations? '(#f #t))
(define dynamic-loop?       '(#f #t))

(define (benchmark-configuration->string t c d? l?)
  (case c
    [(Gambit) (string-append (test->string t) (compiler->string c))]
    [(Twosomes Coercions)
     (string-append (test->string t)
                    (compiler->string c)
                    (dynamic-operations?->string d?)
                    (dynamic-loop?->string l?))]))

(define test->string symbol->string)
(define compiler->string symbol->string)
(define (dynamic-operations?->string x)
  (if x "WithDynOps" "WithoutDynOps"))
(define (dynamic-loop?->string x)
  (if x "DynLoop" "StaticLoop"))

;; where is your gambit
(define gambit
  (or (find-executable-path "gambit")
      (find-executable-path "gsc")
      (error 'gambit "couldn't locate the gambit compiler")))

(define epsilon-parameter (make-parameter 10))
(define iterations-parameter (make-parameter (* (expt 10 7) 1)))
(define runs-parameter (make-parameter 100))



(module+ main
  ;; 10 miliseconds is the smallest time we accept for this test
  (mem-dflt (expt 1024 3))
  (command-line
   #:once-each
   [("-i" "--iterations") number-of-iterations
    "number of iterations in timing loop"
    (iterations-parameter
     (or (string->exact-integer number-of-iterations)
         (error 'dynamic-write-read-benchmark "bad iterations argument")))]
   [("-r" "--runs") number-of-runs
    "number of trials of each benchmark"
    (runs-parameter
     (or (string->exact-integer number-of-runs)
         (error 'dynamic-write-read-benchmark "bad runs argument")))]
   [("-m" "--memory") memory-start-size
    "size in bytes of grift and gambit starting memory"
    (mem-dflt
     (or (string->exact-integer memory-start-size)
         (error 'dynamic-write-read-benchmark "bad memory argument")))]
   [("-e" "--epsilon") epsilon
    "smallest time in milliseconds that will be recorded without error"
    (epsilon-parameter
     (or (string->number epsilon)
         (error 'dynamic-write-read-benchmark "bad epsilon argument")))]
   #:args ()
   (dynamic-write-read-print-latex!
    (run-dynamic-write-reads-benchmark))))

(define (run-dynamic-write-reads-benchmark [iterations (iterations-parameter)]
                                           [number-of-runs (runs-parameter)]
                                           [epsilon    (epsilon-parameter)])
  ;; Check that required source code exists
  (unless (directory-exists? src-dir)
    (error 'src-dir "no source directory found ~a" src-dir))
  (for* ([test tests] [compiler compilers] [dyn-loop? dynamic-loop?])
    (define src-path (make-src-path test compiler dyn-loop?))
    (unless (file-exists? src-path)
      (error 'src-path "no source file found at ~a" src-path)))
  
  ;; Initialize directories if not yet done
  (unless (directory-exists? data-dir)
    (make-directory data-dir))
  (unless (directory-exists? out-dir)
    (make-directory out-dir))
  (unless (directory-exists? tmp-dir)
    (make-directory tmp-dir))
  
  (run-benchmarks
   tests
   `(,compilers ,dynamic-operations? ,dynamic-loop?)
   (run-benchmark iterations)
   #:skip dynamic-function-app-skip?
   #:build build-benchmark
   #:extract-time (parse-output iterations epsilon)
   #:num-trials number-of-runs
   #:results-file (build-path data-dir "funcall")))

(define (dynamic-function-app-skip? name compiler dynamic-ops? dynamic-loop?)
  (and (equal? compiler 'Gambit) dynamic-ops? dynamic-loop?))

(define (dynamic-write-read-print-latex! brs
                                         [iterations (iterations-parameter)]
                                         [number-of-runs (runs-parameter)]
                                         [epsilon    (epsilon-parameter)])
  (define brss (benchmark-results-stats brs))
  (pretty-print brss)
  (define brs-no-overhead
    (for*/fold ([brs-no '()]) ([c compilers] [d dynamic-operations?] [l dynamic-loop?])
      (cond
        [(dynamic-function-app-skip? 'Ref c d l) brs-no]
        [else
         (match-define (list mean-overhead _)
           (benchmark-results-stats-ref brss 'RefOverhead (list c d l)))
         (define br-wo (benchmark-results-ref brs 'Ref (list c d l)))
         (define (subtract-overhead x) (- x mean-overhead))
         (define br-no
           (benchmark-result 'call (list c d l)
                             (map subtract-overhead br-wo)))
         (cons br-no brs-no)])))
  (define brs-no-stats (benchmark-results-stats brs-no-overhead))

  (define date-str
    (parameterize ([date-display-format 'iso-8601])
      (date->string (current-date))))
  
  (define tex-file (build-path out-dir (string-append date-str "-dynamic-refs" ".tex")))
  (define log-file (build-path out-dir (string-append date-str "-dynamic-refs" ".txt")))

  (define (new-command name output)
    (format "newcommand{~a}{~a}\n" name output))
  (define (stat->tex s)
    (match-define (list name (list c d? l?) mean sdev) s)
    (define stat-name (benchmark-configuration->string name c d? l?))
    (string-append
     (new-command (format "dyn~aMeanNS" stat-name)
                  (format "~a ns" (~r mean #:precision '(= 0))))
     (new-command (format "dyn~aSdevNS" stat-name)
                  (format "~a ns" (~r sdev #:precision '(= 0))))))

  (define (stat->string s)
    (match-define (list name (list c d? l?) mean sdev) s)
    (format "dynamic ~a (ns) mean: ~a sdev: ~a\n"
            (benchmark-configuration->string name c d? l?)
            (~r mean #:precision '(= 0))
            (~r sdev #:precision '(= 0))))
  
  (call-with-output-file tex-file #:exists 'replace
    (lambda (tex)
      (display (new-command "dynRefIterations" (number->string iterations)) tex)
      (display (new-command "dynRefRuns" (number->string number-of-runs)) tex)
      (display (new-command "dynRefEpsilon" (number->string epsilon))  tex)
      (call-with-output-file log-file #:exists 'replace
        (lambda (log)
          (display (string-append "Iterations:" (number->string iterations) "\n") log)
          (display (string-append "Runs:      " (number->string number-of-runs) "\n") log)
          (display (string-append "Epsilon:   " (number->string epsilon) "\n") log)
          (for ([stat brs-no-stats])
            (display (stat->tex stat) tex)
            (display (stat->string stat) log))))))
  (pretty-print brs-no-stats))



(define (string->exact-integer x)
  (cond
    [(string->number x) => 
     (lambda (n) (and (exact-integer? n) n))]
    [else (error 'string->exact-integer "failed ~v" x)]))


(define (make-exe-path test compiler dyn-ops? dyn-loop?)
  (build-path tmp-dir (benchmark-configuration->string test compiler dyn-ops? dyn-loop?)))

(define (make-src-path test-name compiler dynamic-loop?)
  (define dl-str (dynamic-loop?->string dynamic-loop?))
  (build-path
   src-dir
   (case compiler
     [(Gambit)  (format "~a.scm" test-name)]
     [(Twosomes Coercions) (format "~a~a.grift" test-name dl-str)])))

(define (build-benchmark test compiler dyn-ops? dyn-loop?)
  (define out-path (make-exe-path test compiler dyn-ops? dyn-loop?))
  (define src-path (make-src-path test compiler dyn-loop?))
  (case compiler
    [(Gambit)
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
    [(Twosomes Coercions)
            (define (help p m s) (format "~a.~a.~a" p m s))
       (define tmp-c-path
         (build-path tmp-dir (help test compiler "c")))
       (define tmp-a-path
         (build-path tmp-dir (help test compiler "s")))
       (compile src-path
                #:output out-path
                #:keep-c tmp-c-path
                #:keep-a tmp-a-path
                #:cast-rep compiler
                #:mem (mem-dflt)
                #:cc-opt "-w -O3"
                #:dyn-ops dyn-ops?)]))

(define ((run-benchmark iterations) test compiler dyn-ops dyn-loop?)
  (define exe-path (make-exe-path test compiler dyn-ops dyn-loop?))
  (case compiler
    [(Gambit)
     (unless (system* (path->string exe-path) (number->string iterations))
       (error 'run-benchmark "failed to run ~a" exe-path))]
    [else
     (with-input-from-string (~a iterations)
       (lambda ()
         (unless (system* (path->string exe-path))
           (error 'run-benchmark "failed to run ~a" exe-path))))]))

(define grift-spec #px"time \\(sec\\): (\\d+.\\d+)\nDynamic : \\?")

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

