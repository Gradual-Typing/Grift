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

    (define (guarded-compile fl i cast ref opened?)
      (let* ([o (path-replace-extension
                 file-name
                 (string-append ".o" (number->string i)))]
             [f (build-path compile-dir o)]
             ;; [c (build-path c-dir (path-add-extension o ".c"))]
             )
        (parameterize ([specialize-cast-code-generation? opened?])
          (if (not (file-exists? f))
              (begin
                (display f)
                (display "\n")
                (compile fl #:output f #:cast cast #:ref ref))
              (void)))))

    ;; config 01: guarded references,   coercions,  close coded
    ;; config 02: guarded references,   type-based, close coded
    ;; config 03: guarded references,   coercions,  open coded
    ;; config 04: guarded references,   type-based, open coded
    ;; config 05: monotonic references, coercions,  close coded
    ;; config 06: monotonic references, type-based, close coded
    ;; config 07: monotonic references, coercions,  open coded
    ;; config 08: monotonic references, type-based, open coded
    (guarded-compile fl 1 'Coercions 'Guarded #f)
    (guarded-compile fl 2 'Type-Based 'Guarded #f)
    (guarded-compile fl 3 'Coercions 'Guarded #t)
    (guarded-compile fl 4 'Type-Based 'Guarded #t)
    (guarded-compile fl 5 'Coercions 'Monotonic #f)
    (guarded-compile fl 6 'Type-Based 'Monotonic #f)
    (guarded-compile fl 7 'Coercions 'Monotonic #t)
    (guarded-compile fl 8 'Type-Based 'Monotonic #t)
    ))

(module+ main
  (c-flags (cons "-O3" (c-flags)))
  (command-line
   #:once-each
   ["--no-dyn-operations"
    "disable the specialization of dynamic elimination for functions, references, and tuples"
    (dynamic-operations? #f)]
   #:args (directory)
   (compile-directory
    (or (string->path directory)
        (error 'benchmark-main "could parse ~v as a path" directory)))))
