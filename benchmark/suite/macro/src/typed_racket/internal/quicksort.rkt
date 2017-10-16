;;; 10/9/2017 changed to use internal timing (Andre)
#lang typed/racket/base

(require racket/fixnum)

(: sort : (Vectorof Fixnum) Fixnum Fixnum -> Fixnum)
(define (sort a p r)
  (if (fx< p r)
      (let ([q (partition a p r)])
        (begin
          (sort a p (fx- q 1))
          (sort a (fx+ q 1) r)))
      0))

(: partition : (Vectorof Fixnum) Fixnum Fixnum -> Fixnum)
(define (partition a p r)
  ;; Why does this box exist?
  (let ([i : (Boxof Fixnum) (box (fx- p 1))]
        [x (vector-ref a r)])
    (begin
      (let loop : Fixnum ([j : Fixnum p])
       (if (fx< j r)
           (begin
             (if (fx<= (vector-ref a j) x)
                 (begin
                   (set-box! i (fx+ (unbox i) 1))
                   (swap a (unbox i) j))
                 0)
             (loop (fx+ j 1)))
           0))
      (swap a (fx+ (unbox i) 1) r)
      (fx+ (unbox i) 1))))

(: swap : (Vectorof Fixnum) Fixnum Fixnum -> Fixnum)
(define (swap a i j)
  (if (fx= i j)
      0
      (let ([t (vector-ref a i)])
        (begin
          (vector-set! a i (vector-ref a j))
          (vector-set! a j t)
          0))))

(define (main)
  (let ([size (read)])
    (unless (fixnum? size)
      (error 'quicksort "invalid input: expected fixnum"))
    (let ([a : (Vectorof Fixnum) (make-vector size 1)])
      (begin
        (let loop ([i : Fixnum 0])
          (when (fx< i size)
            (let ([e (read)])
              (unless (fixnum? e)
                (error 'quicksort.rkt "invalid input: expected integer"))
              (vector-set! a i e)
              (loop (fx+ i 1)))))
        (time (sort a 0 (fx- size 1)))
        (print (vector-ref a (fx- size 1)))))))

(time (main))
