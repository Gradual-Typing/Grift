#lang racket

(require 2htdp/batch-io)

(module+ main
  (let* ([a (read-words 'stdin)]
         [aa (map string->number a)]
         [size (car aa)]
         [arr (list->vector (cdr aa))])
    (begin
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
        (sort arr 0 (- size 1)))
      (printf "~a\n" (vector-ref arr (- size 1))))))
