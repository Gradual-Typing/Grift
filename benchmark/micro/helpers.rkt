#lang racket

(provide (all-defined-out))

(define epsilon-parameter (make-parameter 10))
(define iterations-parameter (make-parameter (* (expt 10 7) 1)))
(define runs-parameter (make-parameter 100))

;; Check to see if a file exists before making it
(define (build-dir/check x)
  (cond
    [(file-exists? x) (error 'build-dir/check "file not dir: ~a" x)]
    [(not (directory-exists? x)) (make-directory* x)]))

