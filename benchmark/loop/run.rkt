#lang racket

(require "../helpers.rkt")
(require "../../src/macros.rkt")

#| test parameters that may need to be messed with |#
;; Number of samples taken at each size
(define num-of-samples 100)
;; Number of Sizes
(define num-of-sizes 30)
;; List of the sizes

(define loop-sizes
  (for/list ([i (in-range num-of-sizes)]) (expt 2 i)))


#|
Compile All the Files for the Test

Note: Due to not having command line arguments in schml we currently
rely on generating all copies of the test that we want to run.
|#

(define (name n)
  (format "loop~a.schml" n))



(define (gen-schml-loop n)
  (with-output-to-file (src 'schml (name n))
    #:mode 'text #:exists 'replace
    (lambda ()
      (display
       `(begin
          (timer-start)
          (repeat (i 0 ,n) ())
          (timer-stop)
          (timer-report))))))

(display "Compiling Files ... ")
(for ([n (in-list loop-sizes)])
  (gen-schml-loop n)
  (schml-compile (name n)))
(c-compile "loop.c")
(gambit-compile "loop.scm")
(display "done\n")

(display "Running Benchmarks ... ")
(for ([n (in-list loop-sizes)])
  (parse-basic-time 'schml n (out->string (run 'schml (name n))))
  (parse-basic-time 'c n (out->string (run 'c "loop.c" n)))
  (parse-gambit-time n (out->string (run 'gambit "loop.scm" n))) 
  )
(display "done\n")
