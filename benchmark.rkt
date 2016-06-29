#lang racket

(require
 racket/path
 (only-in srfi/13 string-suffix-ci?))

(require "src/errors.rkt"
         "src/compile.rkt"
         "tests/values.rkt"
         "src/configuration.rkt"
         "tests/paths.rkt")

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

(module+ main
  (let ([args (current-command-line-arguments)])
    (cond
      [(> 2 (vector-length args)) (display "please specify what directory to compile and how much memory to be allocated!\n")]
      [(< 2 (vector-length args)) (display "too many arguments!\n")]
      [(compile-directory (vector-ref args 0) (string->number (vector-ref args 1))) (display "\n")])))
