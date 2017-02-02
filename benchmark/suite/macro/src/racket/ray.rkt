#lang racket/base

;;; RAY -- Ray-trace a simple scene with spheres.
;;; Translated to Scheme from Paul Graham's book ANSI Common Lisp, Example 9.8
;;; And then translated to Racket by Deyaaeldeen Almahallawi

(define (make-point x y z)
  (vector x y z))

(define (point-x p) (vector-ref p 0))
(define (point-y p) (vector-ref p 1))
(define (point-z p) (vector-ref p 2))

(define (sq x) (* x x))

(define (mag x y z) 
  (sqrt (+ (sq x) (+ (sq y) (sq z)))))

(define (unit-vector x y z) 
  (let ([d  (mag x y z)])
    (make-point (/ x d) (/ y d) (/ z d))))

(define (distance p1 p2) 
  (mag (- (point-x p1) (point-x p2))
       (- (point-y p1) (point-y p2))
       (- (point-z p1) (point-z p2))))

(define *world* (make-vector 33 (vector 0.0 0.0 (vector 0.0 0.0 0.0))))

(define eye (make-point 0.0 0.0 200.0))

(define (tracer res)
  (let ([extent (* res 100)])
    (display "P2 ")
    (write extent)
    (display " ")
    (write extent)
    (display " 255")
    (newline)
    (do ((y 0 (+ y 1)))
        ((= y extent))
      (do ((x 0 (+ x 1)))
          ((= x extent))
        (write (color-at
                (+ -50.0
                   (/ (exact->inexact x) (exact->inexact res)))
                (+ -50.0
                   (/ (exact->inexact y) (exact->inexact res)))))
        (newline)))))

(define (color-at x y)
  (let ([ray (unit-vector (- x (point-x eye))
                          (- y (point-y eye))
                          (- (point-z eye)))])
    (inexact->exact (round (* (sendray eye ray) 255.0)))))

(define (sendray pt ray)
  (let ([x (loop pt ray 0
                 (vector-length *world*)
                 *world*
                 #f
                 #f
                 1e308)])
    (let ([s (vector-ref x 0)]
          [int (vector-ref x 1)])
      (if s
          (* (lambert s int ray)
             (sphere-color s))
          0.0))))

(define (loop pt ray index lst-len lst surface hit dist)
  (if (= index lst-len)
      (vector surface hit)
      (let ([s (vector-ref lst index)])
        (let ([xr  (point-x ray)]
              [yr  (point-y ray)]
              [zr  (point-z ray)]
              [sc  (sphere-center s)])
          (let ([a  (+ (sq xr) (+ (sq yr) (sq zr)))]
                [b  (* 2.0
                       (+ (* (- (point-x pt) (point-x sc)) xr)
                          (+ (* (- (point-y pt) (point-y sc)) yr)
                             (* (- (point-z pt) (point-z sc)) zr))))]
                [c  (+ (+ (sq (- (point-x pt) (point-x sc)))
                          (sq (- (point-y pt) (point-y sc))))
                       (+ (sq (- (point-z pt) (point-z sc)))
                          (- (sq (sphere-radius s)))))])
            (if (zero? a)
                (let ([n  (/ (- c) b)])
                  (let ([h (make-point (+ (point-x pt) (* n xr))
                                       (+ (point-y pt) (* n yr))
                                       (+ (point-z pt) (* n zr)))])
                    (let ([d (distance h pt)])
                      (if (< d dist)
                          (loop pt ray (+ index 1) lst-len lst s h d)
                          (loop pt ray (+ index 1) lst-len lst surface hit dist)))))
                (let ([disc  (- (sq b) (* 4.0 (* a c)))])
                  (if (negative? disc)
                      (loop pt ray (+ index 1) lst-len lst surface hit dist)
                      (let ([discrt  (sqrt disc)]
                            (minus-b  (- b))
                            (two-a  (* 2.0 a)))
                        (let ([n (min (/ (+ minus-b discrt) two-a)
                                      (/ (- minus-b discrt) two-a))])
                          (let ([h (make-point (+ (point-x pt) (* n xr))
                                               (+ (point-y pt) (* n yr))
                                               (+ (point-z pt) (* n zr)))])
                            (let ([d  (distance h pt)])
                              (if (< d dist)
                                  (loop pt ray (+ index 1) lst-len lst s h d)
                                  (loop pt ray (+ index 1) lst-len lst surface hit dist))))))))))))))


(define (lambert s int ray)
  (let ([n (sphere-normal s int)])
    (max 0.0
         (+ (* (point-x ray) (point-x n))
            (+ (* (point-y ray) (point-y n))
               (* (point-z ray) (point-z n)))))))

(define (make-sphere color radius center) 
  (vector color radius center))

(define (sphere-color s) 
  (vector-ref s 0))
(define (sphere-radius s) 
  (vector-ref s 1))
(define (sphere-center s) 
  (vector-ref s 2))

(define (defsphere i x y z r c) 
  (let ([s (make-sphere c r (make-point x y z))])
    (begin
      (vector-set! *world* i s)
      s)))

(define (sphere-normal s pt) 
  (let ([c (sphere-center s)])
    (unit-vector (- (point-x c) (point-x pt))
                 (- (point-y c) (point-y pt))
                 (- (point-z c) (point-z pt)))))

(begin
  (let ([counter (box 29)])
    (begin
      (defsphere 32 0.0 -300.0 -1200.0 200.0 0.8)
      (defsphere 31 -80.0 -150.0 -1200.0 200.0 0.7)
      (defsphere 30 70.0 -100.0 -1200.0 200.0 0.9)
      (do ((x -2 (+ x 1)))
          ((> x 2))
        (do ((z 2 (+ z 1)))
            ((> z 7))
          (defsphere
            (unbox counter)
            (* (exact->inexact x) 200.0)
            300.0
            (* (exact->inexact z) -400.0)
            40.0
            0.75)
          (set-box! counter (- (unbox counter) 1))))))
  (tracer 1))
