#lang racket

(require
 racket/path
 (only-in srfi/13 string-suffix-ci?))

(require ;"src/language.rkt"
         "src/errors.rkt"
         "src/compile.rkt"
         "tests/values.rkt"
         "src/configuration.rkt"
         "tests/paths.rkt")

;; (define compiler-config
;;   (make-parameter
;;    (Config (build-path "test")
;;            'Lazy-D
;;            (build-path test-tmp-path "t.out")
;;            (build-path test-tmp-path "t.c")
;;            #f
;;            '()
;;            #f
;;            'Coercions
;;            1000000)))

(define (compile-directory dir)
  (for ((fl (find-files (lambda (f) (string-suffix-ci? ".schml" (path->string f))) dir)))
    (let* ([f-o (path->string (path-replace-suffix fl ".o"))]
           [f-c (path->string (path-replace-suffix fl ".c"))]
           [coercion-f-o (string-append f-o "1")]
           [coercion-f-c f-c]
           [twosome-f-o (string-append f-o "2")]
           [twosome-f-c f-c])
      (compile fl
               #:output (build-path coercion-f-o)
               #:keep-c (build-path coercion-f-c)
               #:cast-rep 'Coercions
               #:mem 9999999999)
      (compile fl
               #:output (build-path twosome-f-o)
               #:keep-c (build-path twosome-f-c)
               #:cast-rep 'Twosomes
               #:mem 9999999999))))

(module+ main
  (let ([args (current-command-line-arguments)])
    (cond
     [(= 0 (vector-length args)) (display "please specify what directory to compile!\n")]
     [(< 1 (vector-length args)) (display "please only specify one directory to compile!\n")]
     [(compile-directory (vector-ref args 0)) (display "\n")])))
