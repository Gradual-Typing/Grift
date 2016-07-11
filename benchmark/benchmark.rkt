#lang racket

(require
 racket/path
 (only-in srfi/13 string-suffix-ci?))

(require "../src/errors.rkt"
         "../src/compile.rkt"
         "../tests/values.rkt"
         "../src/configuration.rkt"
         "../tests/paths.rkt")

(define-syntax-rule (debug v ...)
  (begin (printf "~a=~v\n" 'v v) ... (newline)))

(define (compile-directory compile-dir)
  ;; The directory should exist if we are going to compile it
  (unless (directory-exists? compile-dir)
    (error 'compile-directory "no such directory ~a" compile-dir))

  ;; Don't store the files output on top of the of the source
  ;; put them in a sub directory so that it is easier to delete
  ;; alternatively we should put them in a user specified place.
  (define c-dir (build-path compile-dir "c"))

  ;; if the output path exists make sure it is a directory
  ;; otherwise make the output directory.
  (cond
    [(file-exists? c-dir)
     (error 'compiler-directory "not a directory ~a" compile-dir)]
    [(not (directory-exists? c-dir)) (make-directory c-dir)])

  ;; We are currently only supporting .schml files
  (define (schml-file? path-searched)
    (equal? #"schml" (filename-extension path-searched)))

  (debug compile-dir c-dir)
  
  ;; Iterate over all schml files in directory and
  ;; compile them to 
  (for ((fl (find-files schml-file? compile-dir)))
    (debug fl)
    (define file-name (file-name-from-path fl))

    (define (renaming-output-files suffix)
      (define out-file (path-replace-suffix file-name suffix))
      (define c-file (path-add-suffix out-file #".c"))
      (values (build-path compile-dir out-file) (build-path c-dir c-file)))
        
    (define-values (coercion-f-o coercion-f-c) (renaming-output-files #".o1"))
    (define-values (twosome-f-o twosome-f-c)   (renaming-output-files #".o2"))
    (compile fl
             #:output coercion-f-o
             #:keep-c coercion-f-c
             #:cast-rep 'Coercions)
    (compile fl
             #:output twosome-f-o
             #:keep-c twosome-f-c
             #:cast-rep 'Twosomes)))

(module+ main
  (command-line
   #:once-each
   ["--no-dyn-operations"
    "disable the specialization of dynamic elimination for functions, references, and tuples"
    (dynamic-operations? #f)]
   #:args (directory memory-limit)

   ;; set the default memory limit for the compiler
   (mem-dflt
    (or (string->number memory-limit)
        (error 'command-line
               "couldn't parse ~a as a number" memory-limit)))
   (compile-directory
    (or (string->path directory)
        (error 'benchmark-main "could parse ~v as a path" directory)))))
