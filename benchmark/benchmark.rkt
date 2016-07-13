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
=======
;; (define compiler-config : (Parameterof Config) 
;;   (make-parameter
;;    (Config (build-path "test")
;;            'Lazy-D
;;            (build-path test-tmp-path "t.out")
;;            (build-path test-tmp-path "t.c")
;;            #f
;;           '()
;;           #f
;;           'Twosomes
;;           1000000)))


#;
(define (compile-directory dir m)
  (begin
    (make-directory (string->path (string-append dir "c/")))
    (for ((fl (find-files (lambda (f) (string-suffix-ci? ".schml" (path->string f))) dir)))
      (let-values ([(dirname filename _) (split-path (path-replace-suffix fl ""))])
        (let* ([f-o (string-append (path->string dirname) (path->string filename) ".o")]
               [f-c (string-append (path->string dirname) "c/" (path->string filename))]
               [coercion-f-o (string-append f-o "1")]
               [coercion-f-c (string-append f-c "_1.c")]
               [twosome-f-o (string-append f-o "2")]
               [twosome-f-c (string-append f-c "_2.c")])
          (compile fl
                   #:output (build-path coercion-f-o)
                   #:keep-c (build-path coercion-f-c)
                   #:cast-rep 'Coercions
                   ;; #:log (build-path "tests" "tmp" "p1.log")
                   #:mem m)
          (compile fl
                   #:output (build-path twosome-f-o)
                   #:keep-c (build-path twosome-f-c)
                   #:cast-rep 'Twosomes
                   ;; #:log (build-path "tests" "tmp" "p2.log")
                   #:mem m))))
    ))

;; 9999999999

#;
(module+ main
  (let ([args (current-command-line-arguments)])
    (cond
      [(> 2 (vector-length args)) (display "please specify what directory to compile and how much memory to be allocated!\n")]
      [(< 2 (vector-length args)) (display "too many arguments!\n")]
      [(compile-directory (vector-ref args 0) (string->number (vector-ref args 1))) (display "\n")])))

