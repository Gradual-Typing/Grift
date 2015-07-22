#lang racket

(require "../helpers.rkt")

#| test parameters that may need to be messed with |#
;; Number of samples taken at each size
(define num-of-samples 100)
;; Number of Sizes
(define num-of-sizes 20)
;; List of the sizes

(display 'run)
(flush-output)

(define loop-sizes ;`(0 1 2 .. 2^n)
  (for/list ([i (in-range num-of-sizes)])
    (expt 2 i)))


#|
Due to not having command line arguments we currently
rely on generating all copies of the test that we want
to run.
|#
(define (name n)
  (format "loop~a.schml" n))

(define (gen-schml-loop n)
  (with-output-to-file (string-append (src 'schml) (name n))
    #:mode 'text #:exists 'replace
    (lambda ()
      (display
       `(begin
          (timer-start)
          (repeat (i 0 ,n) ())
          (timer-stop)
          (timer-report))))))

(for ([n (in-list loop-sizes)])
  (gen-schml-loop n)
  (schml-compile (name n)))

(c-compile "loop.c")
;;(gambit-compile "loop.scm")
