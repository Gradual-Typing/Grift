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
(define-runtime-path tmp-dir "tmp")
(build-dir/check tmp-dir)
(define-runtime-path src-dir "src")
(build-dir/check src-dir)
(define-runtime-path src-paths-file "src/schml.path-objects")

;; where is clang
(define clang
  (or (find-executable-path "clang")
      (error 'clang "couldn't locate")))

;; These keys are what are iterated over to control the benchmark
(define tests               '(fn-app ref-read-write))
(define compilers           '(Type-Based Coercions))
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
    (define schml-path  (configuration->schml-src-path test n))
    (define src-code (generate n))
    (write-source schml-path src-code)
    
    (define type-based-path (configuration->src-path test n 'Type-Based #f))
    (compile schml-path #:cast 'Type-Based #:keep-c type-based-path)
    
    (define type-based-c-path (configuration->src-path test n 'Type-Based #t))
    (copy-file type-based-path type-based-c-path copy-over-it)
    
    (define coercions-path (configuration->src-path test n 'Coercions #f))
    (compile schml-path #:cast 'Coercions #:keep-c coercions-path)
    
    (define coercions-c-path (configuration->src-path test n 'Coercions #t))
    (copy-file coercions-path coercions-c-path copy-over-it)))

(define (configuration->string test reps compiler hand-coded?)
  (define hand-coded-str (if hand-coded? ".byHand" ""))
  (format "~a-x~a.~a~a" test reps compiler hand-coded-str))

(define (configuration->src-path test reps compiler hand-coded?)
  (define name (configuration->string test reps compiler hand-coded?))
  (build-path src-dir (string-append name ".c")))

(define (configuration->schml-src-path test reps)
  (build-path tmp-dir (format "~a-x~a.schml" test reps)))

(define (configuration->exe-path test reps compiler hand-coded?)
  (define name (configuration->string test reps compiler hand-coded?))
  (build-path tmp-dir name))

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


(define (check-src-files-ready-to-run-benchmarks)
  (unless (directory-exists? src-dir)
    (error 'src-dir "no source directory found ~a" src-dir))
  (for* ([t tests]
         [n (overhead-test-repetitions)]
         [c compilers]
         [hc? hand-coded?])
    (define src-path (configuration->src-path t n c hc?))
    (unless (file-exists? src-path)
      (error 'src-path "no source file found at ~a" src-path))
    (cond
      ;; If handcoded and there ops to replace
      [(and hc? (not (= 0 n))) 
       (cond
         [(not (> (file-or-directory-modify-seconds src-path)
                  (file-or-directory-modify-seconds
                   (configuration->src-path t n c #f))))
          (error 'src-path "hand coded version not updated: ~v" src-path)])])))

(define (run-static-benchmarks
         [iterations (iterations-parameter)]
         [number-of-runs (runs-parameter)]
         [epsilon    (epsilon-parameter)])
  (check-src-files-ready-to-run-benchmarks)
  (run-benchmarks
   tests
   `(,(overhead-test-repetitions) ,compilers ,hand-coded?)
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
  #;
  (define brs-no-overhead
    (for*/fold ([brs-no '()])
               ([t tests]
                [c compilers]
                [d dynamic-operations?])
      (cond
        [(dynamic-function-app-skip? t c d) brs-no]
        [(or (eq? 'CallOverhead t) (eq? 'RefOverhead t)) brs-no]
        [else
         (define overhead-key (if (eq? 'Call t) 'CallOverhead 'RefOverhead))
         (define mean-overhead
           (car (benchmark-results-stats-ref brss overhead-key (list c d))))
         (define br-wo (benchmark-results-ref brs t (list c d)))
         (define (subtract-overhead x) (- x mean-overhead))
         (define br-no
           (benchmark-result t (list c d) (map subtract-overhead br-wo)))
         (cons br-no brs-no)])))
  #;(define brs-no-stats (benchmark-results-stats brs-no-overhead))

  #;
  (define date-str
    (parameterize ([date-display-format 'iso-8601])
      (date->string (current-date))))
  
  #;(define tex-file (build-path data-dir (string-append date-str "-dynamic" ".tex")))
  #;(define log-file (build-path data-dir (string-append date-str "-dynamic" ".txt")))

  #;
  (define (new-command name output)
    (format "newcommand{~a}{~a}\n" name output))
  #;
  (define (stat->tex s)
    (match-define (list name (list c d?) mean sdev) s)
    (define stat-name (configuration->string name c d?))
    (string-append
     (new-command (format "dyn~aMeanNS" stat-name)
                  (format "~a ns" (~r mean #:precision '(= 0))))
     (new-command (format "dyn~aSdevNS" stat-name)
                  (format "~a ns" (~r sdev #:precision '(= 0))))))

  #;
  (define (stat->string s)
    (match-define (list name (list c d?) mean sdev) s)
    (format "dynamic ~a (ns) mean: ~a sdev: ~a\n"
            (configuration->string name c d?)
            (~r mean #:precision '(= 0))
            (~r sdev #:precision '(= 0))))
  
  #;
  (call-with-output-file tex-file #:exists 'replace
    (lambda (tex)
      (display (new-command "dynIterations" (number->string iterations)) tex)
      (display (new-command "dynRuns" (number->string number-of-runs)) tex)
      (display (new-command "dynEpsilon" (number->string epsilon))  tex)
      (call-with-output-file log-file #:exists 'replace
        (lambda (log)
          (display (string-append "Iterations:" (number->string iterations) "\n") log)
          (display (string-append "Runs:      " (number->string number-of-runs) "\n") log)
          (display (string-append "Epsilon:   " (number->string epsilon) "\n") log)
          (for ([stat brs-no-stats])
            (display (stat->tex stat) tex)
            (display (stat->string stat) log))))))
  #;(pretty-print brs-no-stats)
  )



(define (string->exact-integer x)
  (cond
    [(string->number x) => 
     (lambda (n) (and (exact-integer? n) n))]
    [else (error 'string->exact-integer "failed ~v" x)]))

(define (build-benchmark test reps compiler hand-coded?)
  (define out-path (configuration->exe-path test reps compiler hand-coded?))
  (define src-path (configuration->src-path test reps compiler hand-coded?))
  (cc/runtime (path->string out-path) (path->string src-path) "-O3 -w"))

(define ((run-benchmark iterations) test reps compiler hand-coded?)
  (define exe-path (configuration->exe-path test reps compiler hand-coded?))
  (with-input-from-string (~a iterations)
    (lambda ()
      (unless (system* (path->string exe-path))
        (error 'run-benchmark "failed to run ~a" exe-path)))))

(define schml-spec #px"time \\(sec\\): (\\d+.\\d+)\nInt : \\d")

(define ((parse-output iterations epsilon) str)
  (define (->number tmp)
    (string->number
     (or (and (string? tmp) tmp)
         (and (bytes->string/utf-8 tmp))
         (error '->number "failed to parse number ~v" tmp))))
  
  (define schml-result
    (or (regexp-match schml-spec str)
        (error 'parse-output "failed to parse: ~v" str)))

  (define run-result-in-milliseconds
    (* (expt 10 3) (->number (cadr schml-result))))
  
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
    (for*/list ([t tests] [c compilers] [hc hand-coded?])

      #;
      (define-values repsXruntime*(overhead-reps* runtime*)
        (for/lists (n* r*)
                   ([n (overhead-test-repetitions)]
                    #:when #t
                    [r (benchmark-results-ref results t (list n c hc))])
          (values n r)))

      (define overhead-runtime*
        (benchmark-results-ref results t (list 0 c hc)))

      (define overhead-mean (mean overhead-runtime*))
      (define overhead-sdev (stddev overhead-runtime*))
      
      (define unit-runtime*
        (benchmark-results-ref results t (list 1 c hc)))

      (define unit-mean (mean unit-runtime*))
      (define unit-sdev (stddev unit-runtime*))

      (debug (list t c hc) overhead-mean overhead-sdev unit-mean unit-sdev)
      (benchmark-result t (list c hc) (list (- unit-mean overhead-mean)))))

  (pretty-print mean-no-overhead)
  mean-no-overhead)



(define (generate-documentation results)
  (match-define (list mean-ref-rw-twosomes #;sdev-ref-rw-twosomes
                      )
    (benchmark-results-ref results 'ref-read-write '(Type-Based #f)))
  (match-define (list mean-ref-rw-twosomes-hc #;sdev-ref-rw-twosomes-hc
                      )
    (benchmark-results-ref results 'ref-read-write '(Type-Based #t)))
  (match-define (list mean-ref-rw-coercions #;sdev-ref-rw-coercions
                      )
    (benchmark-results-ref results 'ref-read-write '(Coercions #f)))
  (match-define (list mean-ref-rw-coercions-hc #;sdev-ref-rw-coercions-hc
                      )
    (benchmark-results-ref results 'ref-read-write '(Coercions #t)))
  (match-define (list mean-fn-app-twosomes #;sdev-fn-app-twosomes
                      )
    (benchmark-results-ref results 'fn-app '(Type-Based #f)))
  (match-define (list mean-fn-app-twosomes-hc #;sdev-fn-app-twosomes-hc
                      )
    (benchmark-results-ref results 'fn-app '(Type-Based #t)))
  (match-define (list mean-fn-app-coercions #;sdev-fn-app-coercions
                      )
    (benchmark-results-ref results 'fn-app '(Coercions #f)))
  (match-define (list mean-fn-app-coercions-hc #;sdev-fn-app-coercions-hc
                      )
    (benchmark-results-ref results 'fn-app '(Coercions #t)))
  (define (fmt x) (format "~a ns" (~r x #:precision '(= 2))))
  (with-output-to-file (build-path data-dir "static-results.tex")  
    #:exists 'replace
    (lambda ()
      (display
       (string-append
        (new-command "staticRefTypeBasedMeanNano" (fmt mean-ref-rw-twosomes))
        #;(new-command "staticRefTypeBasedSDevNano" (fmt sdev-ref-rw-twosomes))
        (new-command "staticRefTypeBasedHandCodedMeanNano"
                     (fmt mean-ref-rw-twosomes-hc))
        #;
        (new-command "staticRefTypeBasedHandCodedSDevNano"
                     (fmt sdev-ref-rw-twosomes-hc))
        (new-command "staticRefCoercionsMeanNano"
                     (fmt mean-ref-rw-coercions))
        #;
        (new-command "staticRefCoercionsSDevNano"
                     (fmt sdev-ref-rw-coercions))
        (new-command "staticRefCoercionsHandCodedMeanNano"
                     (fmt mean-ref-rw-coercions-hc))
        #;
        (new-command "staticRefCoercionsHandCodedSDevNano"
                     (fmt sdev-ref-rw-coercions-hc))
        (new-command "staticFnAppTypeBasedMeanNano"
                     (fmt mean-fn-app-twosomes))
        #;
        (new-command "staticFnAppTypeBasedSDevNano"
                     (fmt sdev-fn-app-twosomes))
        (new-command "staticFnAppTypeBasedHandCodedMeanNano"
                     (fmt mean-fn-app-twosomes-hc))
        #;
        (new-command "staticFnAppTypeBasedHandCodedSDevNano"
                     (fmt sdev-fn-app-twosomes-hc))
        (new-command "staticFnAppCoercionsMeanNano"
                     (fmt mean-fn-app-coercions))
        #;
        (new-command "staticFnAppCoercionsSDevNano"
                     (fmt sdev-fn-app-coercions))
        (new-command "staticFnAppCoercionsHandCodedMeanNano"
                     (fmt mean-fn-app-coercions-hc))
        #;
        (new-command "staticFnAppCoercionsHandCodedSDevNano"
                     (fmt sdev-fn-app-coercions-hc)))))))

;;  ;; (define ref-write-read-test
;;  ;;  (make-reference-wr-timing-loop '(GRef Int) '(GRef Dyn) 0 0))

;; (define ref-write-read-src-file
;;   (write-source "fully-static-ref-write-read" ref-write-read-test))

;; (define-values (ref-write-read-twosomes ref-write-read-coercions)
;;   (values
;;    (compile&run/iteration-time
;;     #:base-name     "fully-static-ref-write-read-twosomes"
;;     #:src-file      ref-write-read-src-file
;;     #:runs          runs
;;     #:iterations    iters
;;     #:cast-repr     'Twosomes
;;     #:function-repr 'Functional
;;     #:output-regexp spec)
;;    (compile&run/iteration-time
;;     #:base-name     "fully-static-ref-write-read-coercions"
;;     #:src-file      ref-write-read-src-file
;;     #:runs          runs
;;     #:iterations    iters
;;     #:cast-repr     'Coercions
;;     #:function-repr 'Hybrid
;;     #:output-regexp spec)))

;; (define fn-app-twosomes-c-src
;;   (build-path src-dir "fully-static-fn-app-twosomes.c"))
;; (define fn-app-coercions-c-src
;;   (build-path src-dir "fully-static-fn-app-coercions.c"))
;; (define ref-write-read-twosomes-c-src
;;   (build-path src-dir "fully-static-ref-write-read-twosomes.c"))
;; (define ref-write-read-coercions-c-src
;;   (build-path src-dir "fully-static-ref-write-read-coercions.c"))

;; (unless (and
;;          (file-exists? fn-app-twosomes-c-src)
;;          (file-exists? fn-app-coercions-c-src)
;;          (file-exists? ref-write-read-twosomes-c-src)
;;          (file-exists? ref-write-read-coercions-c-src)
;;          (begin
;;            (display "Use existing files? [#t/#f]:")
;;            (read)))
;;   (copy-file (build-path tmp-dir "fully-static-fn-app-twosomes.c")
;;              fn-app-twosomes-c-src
;;              #t)
;;   (copy-file (build-path tmp-dir "fully-static-fn-app-coercions.c")
;;              fn-app-coercions-c-src
;;              #t)
;;   (copy-file (build-path tmp-dir "fully-static-ref-write-read-twosomes.c")
;;              ref-write-read-twosomes-c-src
;;              #t)
;;   (copy-file (build-path tmp-dir "fully-static-ref-write-read-coercions.c")
;;              ref-write-read-coercions-c-src
;;              #t)
;;   (begin
;;     (display "Edit files and press any key to run tests")
;;     (read)))

;; (define-values (fn-app-twosomes-c fn-app-coercions-c)
;;   (values
;;    (compile-c/run #:base-name "fully-static-fn-app-twosomes-c"
;;                   #:src-file  fn-app-twosomes-c-src
;;                   #:runs      runs
;;                   #:iters     iters
;;                   #:out-rx    spec)
;;    (compile-c/run #:base-name "fully-static-fn-app-coercions-c"
;;                   #:src-file  fn-app-coercions-c-src
;;                   #:runs      runs
;;                   #:iters     iters
;;                   #:out-rx    spec)))

;; (define-values (ref-write-read-twosomes-c ref-write-read-coercions-c)
;;   (values
;;    (compile-c/run #:base-name "ref-write-read-twosomes-c"
;;                   #:src-file  ref-write-read-twosomes-c-src
;;                   #:runs      runs
;;                   #:iters     iters
;;                   #:out-rx    spec)
;;    (compile-c/run #:base-name "ref-write-read-coercions-c"
;;                   #:src-file  ref-write-read-coercions-c-src
;;                   #:runs      runs
;;                   #:iters     iters
;;                   #:out-rx    spec)))



;; (define-values (results)
;;   (list (list "Function Application" 
;;               (list "Type-Based"
;;                     fn-app-twosomes
;;                     fn-app-twosomes-c)
;;               (list "Coercions"
;;                     fn-app-coercions
;;                     fn-app-coercions-c))
;;         (list "Reference Read and Write"
;;               (list "Type-Based"
;;                     ref-write-read-twosomes
;;                     ref-write-read-twosomes-c)
;;               (list "Coercions"
;;                     ref-write-read-coercions
;;                     ref-write-read-coercions-c))))



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
    "size in kilobytes of schml starting memory"
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
       (generate-benchmarks generate-ref-read-write-src 'ref-read-write)
       (display "make edits and run benchmark\n")))]
   ["--run" "Run the benchmark on generated code"
    (program-main
     (lambda ()
       (let ([results (analyze-results (run-static-benchmarks))])
         (generate-documentation results))))]
   #:args () ((program-main))))
