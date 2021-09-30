#lang racket/base

(require
 racket/cmdline
 racket/file
 racket/list
 racket/match
 racket/path
 racket/place
 (only-in srfi/13 string-suffix-ci?))

(require "../src/errors.rkt"
         "../src/compile.rkt"
         "./configs.rkt"
         "../src/configuration.rkt"
         "../tests/paths.rkt")

(provide compile-file)

(define-syntax-rule (debug v ...)
  (begin (printf "~a=~v\n" 'v v) ... (newline)))

(define (guarded-compile src i cast ref specialize hybrid-runtime fun-proxy-rep)
  (define hybrid-runtime?
    (match hybrid-runtime
      ['Eager   #f]
      ['Lazy     #t]
      [_ (error 'benchmark/bench/guarded-compile
                "invalid specialization: ~a"
                specialize)]))
  (define specialize-casts?
    (match specialize
      ['Unspecialized #f]
      ['Specialized #t]
      [_ (error 'benchmark/bench/guarded-compile
                "invalid specialization: ~a"
                specialize)]))
  (define fun-proxy-rep-param
    (match fun-proxy-rep
      ['UniformClosure   'Hybrid]
      ['NoUniformClosure 'Data]
      [_ (error 'benchmark/bench/guarded-compile
                "invalid option for function proxy representation: ~a"
                fun-proxy-rep)]))
  (define ext (string-append ".o" (number->string i)))
  (define exe (path-replace-extension src ext))
  (define exe.prof (path-replace-extension src (string-append ext ".prof.o")))
  (parameterize ([specialize-cast-code-generation? specialize-casts?]
                 [hybrid-cast/coercion-runtime? hybrid-runtime?]
                 [fn-proxy-representation fun-proxy-rep-param])
    (unless (and (file-exists? exe) (if (cast-profiler?) (file-exists? exe.prof) #t))
      (printf "~a\n" exe)
      (let doit ()
        (cond
          [(*custom-feature*)
           =>
           (λ (x)
             (parameterize ([*custom-feature* #f])
               (let ([config (if (< i 0)
                                 (custom-feature-neg-config x)
                                 (custom-feature-pos-config x))])
                 (call-with-grift-parameterization config doit))))]
          [(cast-profiler?)
           (compile src #:output exe.prof #:cast cast #:ref ref)
           (parameterize ([cast-profiler? #f]) (doit))]
          [else (compile src #:output exe #:cast cast #:ref ref)]))))
  exe)

(define (compile-file f cs)
  (for ([i (in-list cs)]) 
    (apply guarded-compile (cons f (cons i (hash-ref configs (abs i)))))))

(define (place-main id my-chan cp? cflags dyn-ops?)
  (printf "grift-compile-place ~a: init\n" id)
  (define send-ready (lambda a (error 'unitialized)))
  (define compile    (lambda a (error 'unitialized)))
  (match (place-channel-get my-chan)
    [`(setup ,ready-chan ,my-chan-in ,cs)
     (printf "grift-compile-place ~a: setup configs=~a\n" id cs)
     (set! send-ready (lambda () (place-channel-put ready-chan `(ready ,my-chan-in))))
     (set! compile (lambda (fs) (parameterize ([cast-profiler? cp?]
                                               [c-flags cflags]
                                               [dynamic-operations? dyn-ops?])
                                  (for ([f fs])
                                    (compile-file f cs)))))])
  (send-ready)
  (let loop ()
    (match (place-channel-get my-chan)
      [`(compile-files ,fs)
       (printf "grift-compile-place ~a: compile files ~a\n" id fs)
       (compile fs)
       (send-ready)
       (loop)]
      [`(exit) (void)])))

(define (compile-directory compile-dir cs [threads 1] [batch-size 5])
  ;; The directory should exist if we are going to compile it
  (unless (directory-exists? compile-dir)
    (error 'compile-directory-all-configs "no such directory ~a" compile-dir))

  ;; We are currently only supporting .grift files
  (define (grift-file? path-searched)
    (equal? #"grift" (filename-extension path-searched)))

  (define (take/rest l n)
    (if (<= (length l) n)
        l
        (take l n)))
  (define (drop/rest l n)
    (if (<= (length l) n)
        '()
        (drop l n)))
  ;; Iterate over all grift files in directory and
  ;; compile them to 
  (define files (find-files grift-file? compile-dir))
  (cond
    [(<= threads 1) (for ((fl files)) (compile-file fl cs))]
    [else
     (define-values (work-q-out work-q-in) (place-channel))
     (define cp? (cast-profiler?))
     (define cf  (c-flags))
     (define dos (dynamic-operations?))
     (define pls (for/list ([i (in-range threads)])
                   (place/context chan (place-main i chan cp? cf dos))))
     (for ([pl pls]) (place-channel-put pl `(setup ,work-q-in ,pl ,cs)))
     (let loop ([files files])
       (cond
         [(null? files)
          (for ([pl pls]) (place-channel-put pl '(exit)))
          (for ([pl pls]) (place-wait pl))
          (printf "done\n")]
         [else
          (match (place-channel-get work-q-out)
            [`(ready ,id)
             (place-channel-put id `(compile-files ,(take/rest files batch-size)))
             (loop (drop/rest files batch-size))])]))]))

(module+ main
  ;; All valid configurations
  (define config-indices
    (make-parameter (map car (hash-values configs))))
  (define threads (make-parameter 1))
  (define batch-size (make-parameter 5))
  (c-flags (cons "-O3" (c-flags)))
  (command-line
   #:once-each
   ["--custom-feature"
    config
    "todo"
    (parameterize-*custom-feature*/string config)]
   ["--no-dyn-operations"
    "disable the specialization of dynamic elimination for functions, references, and tuples"
    (dynamic-operations? #f)]
   [("--keep-c") name
    "keep the c intermediate representation"
    (ir-code-path (build-path name))]
   [("--configs" "-s") cs
    "Compile a path with multiple configurations"
    (match (regexp-match* #px"(-?\\d+)" cs)
      [(list ds ...)
       (config-indices (map string->number ds))]
      [other
       (error 'benchmark/bench.rkt "invalid configs string: ~v ~v" cs other)])]
   [("--config" "-i") i
    "Compile a path with a single configuration, must be a number > 0"
    (define n? (string->number i))
    ;; compile-file checks for valid indices
    (config-indices (list n?))]
   [("--cast-profiler")
    "Compile benchmarks with the cast-profiler enabled and then with the cast profiler disabled"
    (cast-profiler? #t)]
   [("--threads" "-j") jobs
    "Compile with multiple threads"
    (define j (string->number jobs))
    (unless (exact-nonnegative-integer? j)
      (error 'benchmark/bench.rkt "expected exact-nonnegative-integer got: ~a" jobs))
    (threads j)]
   [("--batch-size" "-b") size
    "How many files to hand off to each job"
    (define s (string->number size))
    (unless (exact-nonnegative-integer? s)
      (error 'benchmark/bench.rkt "expected exact-nonnegative-integer got: ~a" size))
    (batch-size s)]
   #:args (path)
   (cond
     [(string->path path)
      (cond
        [(directory-exists? path)
         (compile-directory path (config-indices) (threads) (batch-size))]
        [(file-exists? path)
         (compile-file path (config-indices))])]
     [else
      (error 'benchmark-main "could parse ~v as a path" path)])))
