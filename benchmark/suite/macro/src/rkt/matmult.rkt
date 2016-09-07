#lang racket

(module+ main
  (command-line
   #:args (mat-side-len)
   (let ([size (string->number mat-side-len)])
     (let ([ar size]
           [ac size]
           [br size]
           [bc size])
       (if (= ac br)
           (letrec ([create (lambda (l1 l2)
                              (let ([x  (make-vector l1 (make-vector l2 0))])
                                (begin
                                  (for [(i (range 0 l1))]
                                    (let ([xi (make-vector l2 0)])
                                      (begin
                                        (for ([j (range 0 l2)])
                                          (vector-set! xi j (+ j i)))
                                        (vector-set! x i xi))))
                                  x)))]
                    [mult (lambda (x x1 x2 y y1 y2)
                            (let ([r (make-vector ar (make-vector bc 0))])
                              (begin
                                (for ([i (range 0 x1)])
                                  (let ([ri (make-vector y2 0)])
                                    (begin
                                      (for ([j (range 0 y2)])
                                        (for ([k (range 0 y1)])
                                          (vector-set! ri j
                                                       (+ (vector-ref ri j)
                                                          (* (vector-ref (vector-ref x i) k)
                                                             (vector-ref (vector-ref y k) j))))))
                                      (vector-set! r i ri))))
                                r)))])
             (let ([a (create ar ac)]
                   [b (create br bc)]
                   [bx (box 0)])
               (begin
                 (time (set-box! bx (vector-ref (vector-ref (mult a ar ac b br bc) (- ar 1)) (- ac 1))))
                 (unbox bx))))
           '())))))
