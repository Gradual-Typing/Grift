#lang racket

(define (gen-loop-file n)
  (with-output-to-file (format "loop~a.schml" n) #:mode 'text
    (lambda ()
      (display
       `(begin
          (timer-start)
          (repeat (i 0 ,n) ())
          (timer-stop)
          (timer-report))))))

(for ([i (in-naturals)]
      #:final (> i 20)
      [n (list (expt 2 i))])
  (display n)
  (newline)
  (display
   `(begin
      (timer-start)
      (repeat (i 0 ,n) ())
      (timer-stop)
      (timer-report)))
  (newline))
