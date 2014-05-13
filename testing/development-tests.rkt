#lang racket
(require rackunit rackunit/text-ui
         Schml/)

(define-syntax test-valid-file
  (syntax-rules ()
    (_ name) (test-not-exn name (lambda () (compile name)))))

(define-syntax test-invalid-file
  (syntax-rules ()
    (_ name) (test-exn name (lambda () (compile name)))))
#|
(define tests
  (test-suite "compiler"
    (test-suite "valid"
      (test-valid-file )
      )))
|#
(module+ main
  (current-directory))

     
