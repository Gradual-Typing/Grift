#lang racket

(define (do-compare p1 p2 [Δ 0.000000001])
  (let recur ([d1 (read p1)] [d2 (read p2)])
    (cond
      [(and (eof-object? d1) (eof-object? d2)) #t]
      [(equal? d1 d2) (recur (read p1) (read p2))]
      [(and (real? d1) (real? d2))
       (and (<= (- d1 Δ) d2 (+ d1 Δ)) (recur (read p1) (read p2)))]
      [else #f])))

(module+ test
  (require rackunit)
  (define dyn
    (open-input-string
         #<<eof
4.759422997128201160
0.808599977156765348
3.714601868896551196
8.591659418825138061
eof
         ))
  (define gambit
    (open-input-string
     #<<eof
4.759422997128201
.8085999771567653
3.714601868896551
8.591659418825138
eof
     ))

  (define ((cmp p1) p2)
    (do-compare p1 p2))
  
  (test-pred "minimal" (cmp dyn) gambit))

(module+ main
  (require racket/cmdline)
  (define delta (make-parameter 0.000000001))
  (define who 'compare.rkt)
  (command-line
   #:once-each
   [("--delta") num
    "give a delta for numeric comparisons"
    (define n (string->number num))
    (unless (real? n)
      (error who "expected number for --delta flag: ~a" num))
    (delta n)]
   #:args (f1 f2)
   (call-with-input-file f1
     (lambda (p1)
       (call-with-input-file f2
         (lambda (p2)
           (unless (do-compare p1 p2 (delta))
             (error who "failed comparison"))))))))
