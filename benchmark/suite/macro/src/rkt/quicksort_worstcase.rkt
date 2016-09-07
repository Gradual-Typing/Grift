#lang racket

(module+ main
  (command-line
   #:args (array-len)
   (let ([size (string->number array-len)])
     (let ([a (make-vector size 1)])
       (begin
         (for ([i (range size)])
           (vector-set! a i (- size i)))
         ;; (timer-start)
         (letrec ([sort (lambda (a p r)
                          (if (< p r)
                              (let ([q (partition a p r)])
                                (begin
                                  (sort a p (- q 1))
                                  (sort a (+ q 1) r)))
                              0))]
                  [partition (lambda (a p r)
                               (let ([i (box (- p 1))]
                                     [x (vector-ref a r)])
                                 (begin
                                   (for ([j (range p r)])
                                     (if (<= (vector-ref a j) x)
                                         (begin
                                           (set-box! i (+ (unbox i) 1))
                                           (swap a (unbox i) j))
                                         0))
                                   (swap a (+ (unbox i) 1) r)
                                   (+ (unbox i) 1))))]
                  [swap (lambda (a i j)
                          (if (= i j)
                              0
                              (let ([t (vector-ref a i)])
                                (begin
                                  (vector-set! a i (vector-ref a j))
                                  (vector-set! a j t)
                                  0))))])
           (time (sort a 0 (- size 1))))
         ;; (timer-stop)
         (vector-ref a (- size 1))
         ;; (timer-report)
         )))))
