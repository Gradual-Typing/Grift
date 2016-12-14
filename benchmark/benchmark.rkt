#lang racket

(require
 racket/path
 (only-in srfi/13 string-suffix-ci?))

(require "../src/errors.rkt"
         "../src/compile.rkt"
         
         "../src/configuration.rkt"
         "../tests/paths.rkt")

(define-syntax-rule (debug v ...)
  (begin (printf "~a=~v\n" 'v v) ... (newline)))

(define (guarded-compile src i cast ref opened?)
  (let* ([exe (path-replace-extension
               src
               (string-append ".o" (number->string i)))])
    (parameterize ([specialize-cast-code-generation? opened?])
      (if (not (file-exists? exe))
          (begin
            (printf "~a\n" exe)
            (compile src #:output exe #:cast cast #:ref ref))
          (void)))))

(define (compile-file f)
  ;; config 01: guarded references,   coercions,  close coded
  (guarded-compile f 1 'Coercions 'Guarded #f)
  ;; config 02: guarded references,   type-based, close coded
  (guarded-compile f 2 'Type-Based 'Guarded #f)
  ;; config 03: monotonic references, coercions,  close coded
  (guarded-compile f 3 'Coercions 'Monotonic #f)
  ;; config 04: monotonic references, type-based, close coded
  (guarded-compile f 4 'Type-Based 'Monotonic #f)
  ;; ;; config 05: guarded references,   coercions,  open coded
  ;; (guarded-compile f 5 'Coercions 'Guarded #t)
  ;; ;; config 06: guarded references,   type-based, open coded
  ;; (guarded-compile f 6 'Type-Based 'Guarded #t)
  ;; ;; config 07: monotonic references, coercions,  open coded
  ;; (guarded-compile f 7 'Coercions 'Monotonic #t)
  ;; ;; config 08: monotonic references, type-based, open coded
  ;; (guarded-compile f 8 'Type-Based 'Monotonic #t)
  )

(define (compile-directory compile-dir)
  ;; The directory should exist if we are going to compile it
  (unless (directory-exists? compile-dir)
    (error 'compile-directory "no such directory ~a" compile-dir))

  ;; ;; if the output path exists make sure it is a directory
  ;; ;; otherwise make the output directory.
  ;; (cond
  ;;   [(file-exists? c-dir)
  ;;    (error 'compiler-directory "not a directory ~a" compile-dir)]
  ;;   [(not (directory-exists? c-dir)) (make-directory c-dir)])

  ;; We are currently only supporting .schml files
  (define (schml-file? path-searched)
    (equal? #"schml" (filename-extension path-searched)))
  
  ;; Iterate over all schml files in directory and
  ;; compile them to 
  (for ((fl (find-files schml-file? compile-dir)))
    (debug fl)
    (compile-file fl)))

(module+ main
  (c-flags (cons "-O3" (c-flags)))
  (command-line
   #:once-each
   ["--no-dyn-operations"
    "disable the specialization of dynamic elimination for functions, references, and tuples"
    (dynamic-operations? #f)]
   #:args (path)
   (if (string->path path)
       (cond
         [(directory-exists? path) (compile-directory path)]
         [(file-exists? path) (compile-file path)])
       (error 'benchmark-main "could parse ~v as a path" path))))
