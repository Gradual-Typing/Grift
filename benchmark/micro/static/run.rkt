#lang racket

(require
 benchmark
 racket/list
 math/statistics
 benchmark
 racket/runtime-path
 #; syntax/location
 "../../helpers.rkt"
 "../helpers.rkt"
 "../code-templates.rkt"
 "../../../src/compile.rkt"
 "../../../src/backend-c/code-generator.rkt")


(define-syntax-rule (debug v ...)
  (begin (printf "~a=~v\n" 'v v) ... (newline)))

(define-runtime-path data-dir "out")
(build-dir/check data-dir)
(define-runtime-path tmp-dir "tmp/logs")
(build-dir/check tmp-dir)
(define-runtime-path src-dir "tmp/src")
(build-dir/check src-dir)
(define-runtime-path exe-dir "tmp/exe")
(build-dir/check exe-dir)

(define-runtime-path src-paths-file "src/grift.path-objects")

;; where is clang
(define clang
  (or (find-executable-path "clang")
      (error 'clang "couldn't locate")))

;; These keys are what are iterated over to control the benchmark
(define tests               '(fn-app ref-read-write))
(define compilers           '(Type-Based Coercions))
(define refs                '(Guarded Monotonic))
(define hand-coded?         '(#f #t))
(define overhead-test-repetitions
  (make-parameter
   '(0 1)
   (lambda (n)
     (cond
       [(exact-nonnegative-integer? n) (build-list n values)]
       [(and (list? n) (andmap exact-nonnegative-integer? n)) n]
       [else (error 'overhead-test-repetition "expected list or integer")]))))

(define (generate-benchmarks generate test)
  (define copy-over-it #t)
  (define src-paths (make-hash))
  (for ([n (in-list (overhead-test-repetitions))])
    (define grift-path  (configuration->grift-src-path test n))
    (define src-code (generate n))
    (write-source grift-path src-code)
    
    (define type-based-path (configuration->src-path test n 'Type-Based 'Guarded #f))
    (compile grift-path #:cast 'Type-Based #:ref 'Guarded #:keep-c type-based-path)
    
    (define type-based-c-path (configuration->src-path test n 'Type-Based 'Guarded #t))
    (copy-file type-based-path type-based-c-path copy-over-it)
    
    (define coercions-path (configuration->src-path test n 'Coercions 'Guarded #f))
    (compile grift-path #:cast 'Coercions #:ref 'Guarded #:keep-c coercions-path)
    
    (define coercions-c-path (configuration->src-path test n 'Coercions 'Guarded #t))
    (copy-file coercions-path coercions-c-path copy-over-it)

    (define type-based-mono-path (configuration->src-path test n 'Type-Based 'Monotonic #f))
    (compile grift-path #:cast 'Type-Based #:ref 'Monotonic #:keep-c type-based-mono-path)
    
    (define type-based-mono-c-path (configuration->src-path test n 'Type-Based 'Monotonic #t))
    (copy-file type-based-mono-path type-based-mono-c-path copy-over-it)
    
    (define coercions-mono-path (configuration->src-path test n 'Coercions 'Monotonic #f))
    (compile grift-path #:cast 'Coercions #:ref 'Monotonic #:keep-c coercions-mono-path)
    
    (define coercions-mono-c-path (configuration->src-path test n 'Coercions 'Monotonic #t))
    (copy-file coercions-mono-path coercions-mono-c-path copy-over-it)))

(define (configuration->string test reps compiler ref hand-coded?)
  (define hand-coded-str (if hand-coded? ".byHand" ""))
  (format "~a-x~a.~a~a~a" test reps compiler ref hand-coded-str))

(define (configuration->src-path test reps compiler ref hand-coded?)
  (define name (configuration->string test reps compiler ref hand-coded?))
  (build-path src-dir (string-append name ".c")))

(define (configuration->grift-src-path test reps)
  (build-path tmp-dir (format "~a-x~a.grift" test reps)))

(define (configuration->exe-path test reps compiler ref hand-coded?)
  (define name (configuration->string test reps compiler ref hand-coded?))
  (build-path exe-dir name))

(define (generate-fn-app-test-src n)
  (define-values (bnd* app)
    (let loop ([n n] [bnd* '()] [app (lambda (i acc) `(+ 1 acc))])
      (cond
        [(= 0 n) (values bnd* app)]
        [else
         (define fn (string->symbol (format "id~a" n)))
         (loop (sub1 n)
               (cons `[,fn : (Int -> Int) (lambda ([x : Int]) x)] bnd*)
               (lambda (i acc) `(,fn ,(app i acc))))])))
  (make-timing-loop
    #:let-bnds    bnd*
    #:acc-type    'Int 
    #:acc-init    0
    #:timed-action   app
    #:use-acc-action (lambda (acc) acc)))

(define (generate-ref-read-write-src num)
  (define-values (bnd* test use)
    (let loop ([n num]
               [bnd* '()]
               [test (lambda (i acc) `(+ 1 acc))]
               [use (lambda (acc) acc)])
      (if (= n 0)
          (values bnd* test use)
          (let ([boxn (string->symbol (format "b~a" n))]
                [redn (string->symbol (format "r~a" n))]
                [wrtn (string->symbol (format "w~a" n))])
            (loop (sub1 n)
                  `([,boxn : (GRef Int) (gbox ,n)] . ,bnd*)
                  (lambda (i acc)
                    `(let ([,redn (gunbox ,boxn)])
                       (let ([,wrtn ,(test i acc)])
                         (begin (gbox-set! ,boxn ,wrtn) ,redn))))
                  (lambda (acc) `(+ (gunbox ,boxn) ,(use acc))))))))
  (make-timing-loop
   #:let-bnds     bnd*
   #:acc-type     'Int 
   #:acc-init     0
   #:timed-action test
   #:use-acc-action use))

(define (generate-vector-read-write-src num)
  (define-values (bnd* test use)
    (let loop ([n num]
               [bnd* '()]
               [test (lambda (i acc) `(+ 1 acc))]
               [use (lambda (acc) acc)])
      (if (= n 0)
          (values bnd* test use)
          (let ([boxn (string->symbol (format "b~a" n))]
                [redn (string->symbol (format "r~a" n))]
                [wrtn (string->symbol (format "w~a" n))])
            (loop (sub1 n)
                  `([,boxn : (Vect Int) (vector 10000 iters)] . ,bnd*)
                  (lambda (i acc)
                    `(let ([j (%% ,i 10000)])
                       (let ([,redn (vector-ref ,boxn j)])
                         (let ([,wrtn ,(test i acc)])
                           (begin (vector-set! ,boxn j ,wrtn) ,redn)))))
                  (lambda (acc) `(+ (repeat [i 0 10000] [sum 0]
                                      (+ (vector-ref ,boxn i) sum))
                                    ,acc)))))))
    (make-timing-loop
     #:let-bnds     bnd*
     #:acc-type     'Int 
     #:acc-init     0
     #:timed-action test
     #:use-acc-action use))


(define (check-src-files-ready-to-run-benchmarks)
  (unless (directory-exists? src-dir)
    (error 'src-dir "no source directory found ~a" src-dir))
  (for* ([t tests]
         [n (overhead-test-repetitions)]
         [c compilers]
         [r refs]
         [hc? hand-coded?])
    (define src-path (configuration->src-path t n c r hc?))
    (unless (file-exists? src-path)
      (error 'src-path "no source file found at ~a" src-path))
    (cond
      ;; If handcoded and there ops to replace
      [(and hc? (not (= 0 n))) 
       (cond
         [(not (> (file-or-directory-modify-seconds src-path)
                  (file-or-directory-modify-seconds
                   (configuration->src-path t n c r #f))))
          (error 'src-path "hand coded version not updated: ~v" src-path)])])))

(define (run-static-benchmarks
         [iterations (iterations-parameter)]
         [number-of-runs (runs-parameter)]
         [epsilon    (epsilon-parameter)])
  (check-src-files-ready-to-run-benchmarks)
  (run-benchmarks
   tests
   `(,(overhead-test-repetitions) ,compilers ,refs ,hand-coded?)
   (run-benchmark iterations)
   #:build build-benchmark
   #:extract-time (parse-output iterations epsilon)
   #:num-trials number-of-runs
   #:results-file (build-path data-dir "funcall")))

(define (dynamic-function-app-skip? name compiler dynamic-ops?)
  (and (equal? compiler 'Gambit) dynamic-ops?))

(define (dynamic-write-read-print-latex! 
         results
         [iterations (iterations-parameter)]
         [number-of-runs (runs-parameter)]
         [epsilon    (epsilon-parameter)])
  (define brss (benchmark-results-stats results))
  (pretty-print brss)

  )


(define (cc/runtime* c-src-dir bin-dir)
  (for ([src-path (in-directory c-src-dir)])
    (when (regexp-match? #px"\\.c$" src-path)
      (define rp (find-relative-path c-src-dir src-path))
      (define cp (path-replace-extension rp #""))
      (define out-path (build-path bin-dir cp))
      (cc/runtime (path->string out-path) (path->string src-path) "-O3 -w"))))

(define (build-benchmark test reps compiler ref hand-coded?)
  (define out-path (configuration->exe-path test reps compiler ref hand-coded?))
  (define src-path (configuration->src-path test reps compiler ref hand-coded?))
  (cc/runtime (path->string out-path) (path->string src-path) "-O3 -w"))

(define ((run-benchmark iterations) test reps compiler hand-coded?)
  (define exe-path (configuration->exe-path test reps compiler hand-coded?))
  (with-input-from-string (~a iterations)
    (lambda ()
      (unless (system* (path->string exe-path))
        (error 'run-benchmark "failed to run ~a" exe-path)))))

(define grift-spec #px"time \\(sec\\): (\\d+.\\d+)\nInt : \\d")

(define ((parse-output iterations epsilon) str)
  (define (->number tmp)
    (string->number
     (or (and (string? tmp) tmp)
         (and (bytes->string/utf-8 tmp))
         (error '->number "failed to parse number ~v" tmp))))
  
  (define grift-result
    (or (regexp-match grift-spec str)
        (error 'parse-output "failed to parse: ~v" str)))

  (define run-result-in-milliseconds
    (* (expt 10 3) (->number (cadr grift-result))))
  
  (unless (> run-result-in-milliseconds epsilon)
    (error 'timer-epsilon
           "not within set timing epsilon: ~a ms"
           run-result-in-milliseconds))

  (define run-result-in-nanoseconds
    (* run-result-in-milliseconds (expt 10 6)))
  
  (/ run-result-in-nanoseconds iterations))



(define (analyze-results
         results
         [iterations (iterations-parameter)]
         [number-of-runs (runs-parameter)]
         [epsilon    (epsilon-parameter)])
  
  (define (basic-stats xs ys)
    (values (mean xs) (stddev xs)
            (mean ys) (stddev ys)
            (correlation xs ys)))
  
  (define result-stats (benchmark-results-stats results))
  (pretty-print result-stats)

  (define mean-no-overhead
    (for*/list ([t tests] [c compilers] [r refs] [hc hand-coded?])

      (define overhead-runtime*
        (benchmark-results-ref results t (list 0 c r hc)))

      (define overhead-mean (mean overhead-runtime*))
      (define overhead-sdev (stddev overhead-runtime*))
      
      (define unit-runtime*
        (benchmark-results-ref results t (list 1 c r hc)))

      (define unit-mean (mean unit-runtime*))
      (define unit-sdev (stddev unit-runtime*))

      (debug (list t c r hc) overhead-mean overhead-sdev unit-mean unit-sdev)
      (benchmark-result t (list c r hc) (list (- unit-mean overhead-mean)))))
  
  (pretty-print mean-no-overhead)
  (define (results-ref test attributes)
    (benchmark-results-ref  mean-no-overhead test attributes))
  
  (match-define (list mean-ref-rw-guarded-twosomes #;sdev-ref-rw-twosomes)
    (results-ref 'ref-read-write '(Type-Based Guarded #f)))
  
  (match-define (list mean-ref-rw-guarded-twosomes-hc #;sdev-ref-rw-twosomes-hc)
    (results-ref 'ref-read-write '(Type-Based Guarded #t)))
  
  (match-define (list mean-ref-rw-guarded-coercions #;sdev-ref-rw-coercions)
    (results-ref 'ref-read-write '(Coercions Guarded #f)))

  (match-define (list mean-ref-rw-guarded-coercions-hc #;sdev-ref-rw-coercions-hc)
    (results-ref 'ref-read-write '(Coercions Guarded #t)))

  (match-define (list mean-ref-rw-mono-twosomes #;sdev-ref-rw-twosomes)
    (results-ref 'ref-read-write '(Type-Based Monotonic #f)))
  
  (match-define (list mean-ref-rw-mono-twosomes-hc #;sdev-ref-rw-twosomes-hc)
    (results-ref 'ref-read-write '(Type-Based Monotonic #t)))
  
  (match-define (list mean-ref-rw-mono-coercions #;sdev-ref-rw-coercions)
    (results-ref 'ref-read-write '(Coercions Monotonic #f)))

  (match-define (list mean-ref-rw-mono-coercions-hc #;sdev-ref-rw-coercions-hc)
    (results-ref 'ref-read-write '(Coercions Monotonic #t)))

  (match-define (list mean-fn-app-twosomes #;sdev-fn-app-twosomes)
    (results-ref 'fn-app '(Type-Based Guarded #f)))

  (match-define (list mean-fn-app-twosomes-hc #;sdev-fn-app-twosomes-hc)
    (results-ref 'fn-app '(Type-Based Guarded #t)))

  (match-define (list mean-fn-app-coercions #;sdev-fn-app-coercions)
    (results-ref 'fn-app '(Coercions Guarded #f)))

  (match-define (list mean-fn-app-coercions-hc #;sdev-fn-app-coercions-hc)
    (results-ref 'fn-app '(Coercions Guarded #t)))

  (define (fmt x) (format "~a ns" (~r x #:precision '(= 2))))
  (with-output-to-file (build-path data-dir "results.tex")  
    #:exists 'replace
    (lambda ()
      (display
       (string-append
        (new-command "staticRefTypeBasedGuardedMeanNano"
                     (fmt mean-ref-rw-guarded-twosomes))
        
        (new-command "staticRefTypeBasedGuardedHandCodedMeanNano"
                     (fmt mean-ref-rw-guarded-twosomes-hc))
        
        (new-command "staticRefCoercionsGuardedMeanNano"
                     (fmt mean-ref-rw-guarded-coercions))
        
        (new-command "staticRefCoercionsGuardedHandCodedMeanNano"
                     (fmt mean-ref-rw-guarded-coercions-hc))

        (new-command "staticRefTypeBasedMonoMeanNano"
                     (fmt mean-ref-rw-mono-twosomes))
        
        (new-command "staticRefTypeBasedMonoHandCodedMeanNano"
                     (fmt mean-ref-rw-mono-twosomes-hc))
        
        (new-command "staticRefCoercionsMonoMeanNano"
                     (fmt mean-ref-rw-mono-coercions))
        
        (new-command "staticRefCoercionsMonoHandCodedMeanNano"
                     (fmt mean-ref-rw-mono-coercions-hc))

        (new-command "staticFnAppTypeBasedMeanNano"
                     (fmt mean-fn-app-twosomes))

        (new-command "staticFnAppTypeBasedHandCodedMeanNano"
                     (fmt mean-fn-app-twosomes-hc))

        (new-command "staticFnAppCoercionsMeanNano"
                     (fmt mean-fn-app-coercions))

        (new-command "staticFnAppCoercionsHandCodedMeanNano"
                     (fmt mean-fn-app-coercions-hc))))))

  (with-output-to-file
    (build-path data-dir "table.tex")
    #:exists 'replace
    (lambda ()
      (define (fmt-entry r s c)
        (format "~a & ~a & ~a & ~a\\% \\\\\n\\hline\n"
                r (fmt s) (fmt c) (exact-round (* 100 (/ (- s c) c)))))
      (define (fmt-block t tbs tbc cs cc)
        (string-append
         (format "\\multicolumn{4}{|l|}{ ~a }\\\\\n\\hline\n" t)
         (fmt-entry 'Type-Based tbs tbc)
         (fmt-entry 'Coercions cs cc)))
      (display
       (string-append
        "\\begin{tabular}{| l | r | r | r |}\n"
        "\\hline\n"
        "\\multicolumn{2}{c|}{Time in nanoseconds} & Overhead \\\\\n"
        "\\cline{2-4}\n"
        "& \\multicolumn{1}{c|}{Grift (S)} "
        "& \\multicolumn{1}{c|}{C} "
        "& \\multicolumn{1}{c|}{(S - C) / S} "
        "\\\\\n\\hline\n"
        (fmt-block "Function Application"
                      mean-fn-app-twosomes
                      mean-fn-app-twosomes-hc
                      mean-fn-app-coercions
                      mean-fn-app-coercions-hc)
        (fmt-block "Guarded Reference Read and Write"
                   mean-ref-rw-guarded-twosomes
                   mean-ref-rw-guarded-twosomes-hc
                   mean-ref-rw-guarded-coercions
                   mean-ref-rw-guarded-coercions-hc)
        (fmt-block "Monotonic Reference Read and Write"
                   mean-ref-rw-mono-twosomes
                   mean-ref-rw-mono-twosomes-hc
                   mean-ref-rw-mono-coercions
                   mean-ref-rw-mono-coercions-hc)
        "\\end{tabular}\n")))))




(module+ main
  ;; 10 miliseconds is the smallest time we accept for this test
  (define program-main
    (make-parameter (lambda () (error 'run.rkt "please pass the \"generate\" or \"run\" flag"))))
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
    "size in kilobytes of grift starting memory"
    (init-heap-kilobytes
     (or (string->exact-integer memory-start-size)
         (error 'dynamic-write-read-benchmark "bad memory argument")))]
   [("-e" "--epsilon") epsilon
    "smallest time in milliseconds that will be recorded without error"
    (epsilon-parameter
     (or (string->number epsilon)
         (error 'dynamic-write-read-benchmark "bad epsilon argument")))]
   #:once-any
   ["--generate" "Run the code generation phase of benchmark"
    (program-main
     (lambda ()
       (generate-benchmarks generate-fn-app-test-src 'fn-app)
       (generate-benchmarks generate-vector-read-write-src 'ref-read-write)
       (display "make edits and run benchmark\n")))]
   ["--compile" src bin "Run c compiler on generated code"
    (program-main (lambda () (cc/runtime* src bin)))]
   ["--run" "Run the benchmark on generated code"
    (program-main
     (lambda () (analyze-results (run-static-benchmarks))))]
   #:args () ((program-main))))
