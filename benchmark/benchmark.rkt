#lang racket

(require
 racket/path
 (only-in srfi/13 string-suffix-ci?))

(require "../src/errors.rkt"
         "../src/compile.rkt"
         "./config_str.rkt"
         "../src/configuration.rkt"
         "../tests/paths.rkt")

(provide compile-file)

(define-syntax-rule (debug v ...)
  (begin (printf "~a=~v\n" 'v v) ... (newline)))

(define (guarded-compile src i cast ref specialize hybrid-runtime)
  (define hybrid-runtime?
    (match hybrid-runtime
      ['Eager   #f]
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
    (unless (file-exists? exe)
      (printf "~a\n" exe)
      (compile src #:output exe #:cast cast #:ref ref)))
  exe)

#;
(define configs
  (call-with-input-file "benchmark/configs.dat"
    (lambda (in) (read in))))

(define (compile-file f cs)
  (for ([i (in-list cs)]) 
    (apply guarded-compile (cons f (cons i (hash-ref configs i))))))

(define (compile-directory compile-dir cs)
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
    (compile-file fl cs)))

(module+ main
  ;; All valid configurations
  (define config-indices
    (make-parameter (map car (hash-values configs))))
  (c-flags (cons "-O3" (c-flags)))
  (command-line
   #:once-each
   ["--no-dyn-operations"
    "disable the specialization of dynamic elimination for functions, references, and tuples"
    (dynamic-operations? #f)]
   [("--configs" "-s") cs
    "Compile a path with multiple configurations"
    (match (regexp-match* #px"(\\d+)" cs)
      [(list ds ...)
       (config-indices (map string->number ds))]
      [other
       (error 'benchmark.rkt "invalid configs string: ~v ~v" cs other)])]
   [("--config" "-i") i
    "Compile a path with a single configuration, must be a number > 0"
    (define n? (string->number i))
    ;; compile-file checks for valid indices
    (config-indices (list n?))]
   #:args (path)
   (cond
     [(string->path path)
      (cond
        [(directory-exists? path)
         (compile-directory path (config-indices))]
        [(file-exists? path)
         (compile-file path (config-indices))])]
     [else
      (error 'benchmark-main "could parse ~v as a path" path)])))
