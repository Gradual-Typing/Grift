#lang racket

(require
 racket/path
 (only-in srfi/13 string-suffix-ci?))

(require "../src/errors.rkt"
         "../src/compile.rkt"
         "./config_str.rkt"
         "../src/configuration.rkt"
         "../tests/paths.rkt")

(define-syntax-rule (debug v ...)
  (begin (printf "~a=~v\n" 'v v) ... (newline)))

(define (guarded-compile src i cast ref specialize hybrid-runtime)
  (define hybrid-runtime?
    (match hybrid-runtime
      ['Strict   #f]
      ['Lazy     #t]
      [_ (error 'benchmark/guarded-compile
                "invalid specialization: ~a"
                specialize)]))
  (define specialize-casts?
    (match specialize
      ['Unspecialized #f]
      ['Specialized #t]
      [_ (error 'benchmark/guarded-compile
                "invalid specialization: ~a"
                specialize)]))
  (define exe
    (path-replace-extension
     src
     (string-append ".o" (number->string i))))
  (parameterize ([specialize-cast-code-generation? specialize-casts?]
                 [hybrid-cast/coercion-runtime? hybrid-runtime?])
    (if (not (file-exists? exe))
        (begin
          (printf "~a\n" exe)
          (compile src #:output exe #:cast cast #:ref ref))
        (void))))

#;
(define configs
  (call-with-input-file "benchmark/configs.dat"
    (lambda (in) (read in))))

(define (compile-file f i)
  (apply guarded-compile (cons f (cons i (hash-ref configs i)))))

(define (compile-file-all-configs f)
  (for ([i (in-range 1 (+ (hash-count configs) 1))])
    (compile-file f i)))

(define (compile-directory-all-configs compile-dir)
  ;; The directory should exist if we are going to compile it
  (unless (directory-exists? compile-dir)
    (error 'compile-directory-all-configs "no such directory ~a" compile-dir))

  ;; ;; if the output path exists make sure it is a directory
  ;; ;; otherwise make the output directory.
  ;; (cond
  ;;   [(file-exists? c-dir)
  ;;    (error 'compiler-directory "not a directory ~a" compile-dir)]
  ;;   [(not (directory-exists? c-dir)) (make-directory c-dir)])

  ;; We are currently only supporting .grift files
  (define (grift-file? path-searched)
    (equal? #"grift" (filename-extension path-searched)))
  
  ;; Iterate over all grift files in directory and
  ;; compile them to 
  (for ((fl (find-files grift-file? compile-dir)))
    (compile-file-all-configs fl)))

(module+ main
  (define config-index? (make-parameter 0))
  (c-flags (cons "-O3" (c-flags)))
  (command-line
   #:once-each
   ["--no-dyn-operations"
    "disable the specialization of dynamic elimination for functions, references, and tuples"
    (dynamic-operations? #f)]
   [("--config" "-i") i
    "Compile a path with a single configuration, must be a number > 0"
    (config-index? (string->number i))]
   #:args (path)
   (if (string->path path)
       (cond
         [(and (directory-exists? path) (> (config-index?) 0))
          (compile-directory path (config-index?))]
         [(directory-exists? path)
          (compile-directory-all-configs path)]
         [(and (file-exists? path) (> (config-index?) 0))
          (compile-file path (config-index?))]
         [(file-exists? path)
          (compile-file-all-configs path)])
       (error 'benchmark-main "could parse ~v as a path" path))))
