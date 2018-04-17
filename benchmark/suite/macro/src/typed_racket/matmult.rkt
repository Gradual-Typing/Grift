#lang typed/racket/base
;;; 10/9/2017 changed to use internal timing (Andre)
;;;
;;; We actively choose to not use racket racket/fixnum. Use of generic
;;; numeric ops is disadvantage for racket but there is no safe
;;; version of fixnum operations that avoids the overhead of
;;; contracts, and we are only interested in comparing safe code.  The
;;; racket/fixnum safe operations are generally no faster than using
;;; generic primitives like +. (According to the documentation)

(: create : Integer Integer -> (Vectorof Integer))
(define (create l1 l2)
  (let ([x : (Vectorof Integer) (make-vector (* l1 l2) 0)])
    (let loop1 ([i 0])
      (if (< i l1)
          (let loop2 ([j 0])
            (begin
              (if (< j l2)
                  (begin
                    (vector-set! x (+ (* l2 i) j) (+ j i))
                    (loop2 (+ 1 j)))
                  (loop1 (+ 1 i)))))
          x))))

(: mult : (Vectorof Integer) Integer Integer (Vectorof Integer) Integer Integer
   -> (Vectorof Integer))
(define (mult x x1 x2 y y1 y2)
  (let ([r : (Vectorof Integer) (make-vector (* y2 x1) 0)])
    (let loop1 ([i 0])
      (if (< i x1)
          (let loop2 ([j 0])
            (if (< j y2)
                (let loop3 ([k 0])
                  (if (< k y1)
                      (begin
                        (vector-set! r (+ (* i y2) j)
                                     (+ (vector-ref r (+ (* i y2) j))
                                          (*
                                           (vector-ref x (+ (* i x2) k))
                                           (vector-ref y (+ (* k y2) j)))))
                        (loop3 (+ k 1)))
                      (loop2 (+ j 1))))
                (loop1 (+ i 1))))
          r))))

(define (main)
  (let ([size (read)])
  (unless (fixnum? size)
    (error 'matmult.rkt "invalid input: expected an integer size"))
  (let ([ar size]
        [ac size]
        [br size]
        [bc size])
    (if (= ac br)
        (let ([a (create ar ac)]
              [b (create br bc)])
          (display (vector-ref (mult a ar ac b br bc) (- (* ar bc) 1)))
          (newline))
        0))))

(time (main))
