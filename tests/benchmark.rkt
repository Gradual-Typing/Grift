#lang typed/racket

#|
This is a small utility that uses the unix time utility and system calls in order
to time programs compiled by the grift compiler. It should probably use something
slightly more acurate than time.

It takes a structured list of information named benchmarks,
and prints the timing log to the variable named benchmark-log-file.
|#
(require racket/port)
(require grift/src/compile
         grift/src/helpers
         grift/testing/paths
         grift/testing/values
         grift/src/errors)

;; This structure is a list representing the arguments to a call to bench
(: benchmarks (Listof (List String Test-Value)))
(define benchmarks
  (list (list "odd-20-static.grift" (boole #f))
        (list "odd-20-hybrid1.grift" (boole #f))
        (list "odd-20-hybrid2.grift" (boole #f))
        (list "odd-20-hybrid3.grift" (boole #f))
        (list "odd-20-hybrid4.grift" (boole #f))
        (list "odd-20-hybrid5.grift" (boole #f))
        (list "odd-20-dynamic.grift" (dynamic))
        (list "ack-1-2-static.grift" (integ 4))
        (list "ack-2-3-static.grift"  (integ 9))
        (list "ack-3-10-static.grift" (integ 8189))
        (list "ack-3-10-hybrid1.grift" (integ 8189))
        (list "ack-3-10-hybrid2.grift" (integ 8189))
        (list "ack-3-10-hybrid3.grift" (integ 8189))
        (list "ack-3-10-dynamic.grift" (integ 8189))
        #;(list "ack-4-1-static.grift"  (integ 65533))
        #;(list  "ack-static.grift"      (boole #t))
        ))

;; The file that is used to return information from this test
(: benchmark-log-file Path)
(define benchmark-log-file (build-path test-tmp-path "benchmark-log.txt"))

;; The temporary file that is used to store and name the compiled executables
(: executable-path Path)
(define executable-path (build-path test-tmp-path "b.out"))

;; The temporary file that is used to stor and name the c intermediate file
(: c-source-path Path)
(define c-source-path (build-path test-tmp-path "b.c"))

;; This is the invocation that is called at the system command line
(: invocation String)
(define invocation (format "time ~a" (path->string executable-path)))

;; This is the number of times each test is repeated by default
(: default-iterations Index)
(define default-iterations 10)

;; This build a regular expression in order to parse the output from
;; the linux time utility.
(define num-str  "(\\d+)m(\\d+\\.\\d+)s")
(define pat-str-fmt "~a\\s+~a\\s+")
(define parse-time-pattern
  (pregexp (string-append (format pat-str-fmt "real" num-str)
                          (format pat-str-fmt "user" num-str)
                          (format pat-str-fmt "sys" num-str))))

;; run-and-log takes the iteration the number of iteration in total
;; and the path of the source file and logs timing information
;; in benchmark-log-file
(: run-and-log (-> String Number Number Any))
(define (run-and-log name run runs)
  (let* ([out (call-with-output-string
               (lambda ([p : Output-Port])
                 (parameterize ([current-error-port p]
                                [current-output-port (open-output-nowhere)])
                   (system invocation)
                   (close-output-port (current-output-port)))))]
         [parsed-out (regexp-match parse-time-pattern out)])
    (match parsed-out
      [(list whole real-m real-s user-m user-s sys-m sys-s)
       (if (not (and real-m real-s user-m user-s sys-m sys-s))
           (fail-benchmark "partial parse")
           (let ([real : Number (time-m+s real-m real-s)]
                 [user : Number (time-m+s user-m user-s)]
                 [sys  : Number (time-m+s sys-m sys-s)])
             (logf "[Result] ~a, ~a, ~a, ~a, ~a, ~a\n" 
                   name run runs real user sys)))]
      [otherwise (fail-benchmark "parsing returned #f")])))

;; time-m+s reads in the textual integer representing minutes in time,
;; the textual float representing seconds in time, and adds them together
;; returning a Real value
(: time-m+s (-> String String Number))
(define (time-m+s mins secs)
  (let* ([mins (string->number mins)]
         [secs (string->number secs)])
    (if (not (and mins secs))
        (fail-benchmark "time-m+s")
        (+ (* mins 60.0) secs))))


;; correct? determines if the executable is giving the correct result
(: correct? (-> Config Test-Value Boolean))
(define (correct? configuration expected-out)
  (value=? (observe (envoke-compiled-program #:config configuration)) expected-out))

;; The structure raised when a benchmark fails
(struct exn:grift:benchmark exn:grift ())

;; raise an exception signaling that the benchmark has failed to run
(: fail-benchmark (-> String Nothing))
(define (fail-benchmark msg)
  (raise (exn:grift:benchmark msg (current-continuation-marks))))


;; benchmark is the main logic of a single program benchmark
;; compile the file to tmp-exec-path
(: benchmark (-> String Test-Value Config Void))
(define (benchmark name expected config)
  (with-handlers ([exn:grift:benchmark?
                   (lambda ([e : exn:grift:benchmark])
                     (logf "[Error] ~a ~a\n" name (exn-message e)))])
    (compile/conf (build-path test-suite-path name) config)
    (if (correct? config expected)
        (for [(i (in-range 0 default-iterations))] (run-and-log name i default-iterations))
        (fail-benchmark "correct?"))))

;; shortened ideomatic and applyable call to benchmark
(: bench (->* (String Test-Value) #:rest Any Void))
(define (bench name result . rest)
  (benchmark name result (Config 'Lazy-D executable-path c-source-path)))

(call-with-output-file benchmark-log-file #:exists 'replace 
  (lambda ([log : Output-Port])
    (parameterize ([current-log-port log])
      (for ([args (in-list benchmarks)])
        (apply bench args)))))


