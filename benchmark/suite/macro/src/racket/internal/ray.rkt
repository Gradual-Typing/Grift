;;; RAY -- Ray-trace a simple scene with spheres.
;;; Translated to Scheme from Paul Graham's book ANSI Common Lisp, Example 9.8
;;; And then translated to Racket by Deyaaeldeen Almahallawi
#lang racket/base

(require racket/flonum racket/format racket/fixnum)

(define (make-point x y z)
  (vector x y z))

(define (point-x p) (vector-ref p 0))
(define (point-y p) (vector-ref p 1))
(define (point-z p) (vector-ref p 2))

(define (sq x) (fl* x x))

(define (mag x y z) 
  (flsqrt (fl+ (sq x) (fl+ (sq y) (sq z)))))

(define (unit-vector x y z) 
  (let ([d  (mag x y z)])
    (make-point (fl/ x d) (fl/ y d) (fl/ z d))))

(define (distance p1 p2) 
  (mag (fl- (point-x p1) (point-x p2))
       (fl- (point-y p1) (point-y p2))
       (fl- (point-z p1) (point-z p2))))

(define (main)
  (define *world* (make-vector 33 (vector 0.0 0.0 (vector 0.0 0.0 0.0))))

  (define eye (make-point 0.0 0.0 200.0))

  (define (tracer res)
    (let ([extent (fx* res 100)])
      (display "P2 ")
      (write extent)
      (display " ")
      (write extent)
      (display " 255")
      (newline)
      (do ((y 0 (fx+ y 1)))
          ((fx= y extent))
        (do ((x 0 (fx+ x 1)))
            ((fx= x extent))
          (write (color-at
                  (fl+ -50.0
                       (fl/ (fx->fl x) (fx->fl res)))
                  (fl+ -50.0
                       (fl/ (fx->fl y) (fx->fl res)))))
          (newline)))))

  (define (color-at x y)
    (let ([ray (unit-vector (fl- x (point-x eye))
                            (fl- y (point-y eye))
                            (fl* -1.0 (point-z eye)))])
      (fl->fx (flround (fl* (sendray eye ray) 255.0)))))

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
            (fl* (lambert s int ray)
                 (sphere-color s))
            0.0))))

  (define (loop pt ray index lst-len lst surface hit dist)
    (if (fx= index lst-len)
        (vector surface hit)
        (let ([s (vector-ref lst index)])
          (let ([xr  (point-x ray)]
                [yr  (point-y ray)]
                [zr  (point-z ray)]
                [sc  (sphere-center s)])
            (let ([a  (fl+ (sq xr) (fl+ (sq yr) (sq zr)))]
                  [b  (fl* 2.0
                           (fl+ (fl* (fl- (point-x pt) (point-x sc)) xr)
                                (fl+ (fl* (fl- (point-y pt) (point-y sc)) yr)
                                     (fl* (fl- (point-z pt) (point-z sc)) zr))))]
                  [c  (fl+ (fl+ (sq (fl- (point-x pt) (point-x sc)))
                                (sq (fl- (point-y pt) (point-y sc))))
                           (fl+ (sq (fl- (point-z pt) (point-z sc)))
                                (fl* -1.0 (sq (sphere-radius s)))))])
              (if (zero? a)
                  (let ([n  (fl/ (fl* -1.0 c) b)])
                    (let ([h (make-point (fl+ (point-x pt) (fl* n xr))
                                         (fl+ (point-y pt) (fl* n yr))
                                         (fl+ (point-z pt) (fl* n zr)))])
                      (let ([d (distance h pt)])
                        (if (fl< d dist)
                            (loop pt ray (fx+ index 1) lst-len lst s h d)
                            (loop pt ray (fx+ index 1) lst-len lst surface hit dist)))))
                  (let ([disc (fl- (sq b) (fl* 4.0 (fl* a c)))])
                    (if (negative? disc)
                        (loop pt ray (fx+ index 1) lst-len lst surface hit dist)
                        (let ([discrt  (flsqrt disc)]
                              (minus-b  (fl* -1.0 b))
                              (two-a  (fl* 2.0 a)))
                          (let ([n (flmin (fl/ (fl+ minus-b discrt) two-a)
                                          (fl/ (fl- minus-b discrt) two-a))])
                            (let ([h (make-point (fl+ (point-x pt) (fl* n xr))
                                                 (fl+ (point-y pt) (fl* n yr))
                                                 (fl+ (point-z pt) (fl* n zr)))])
                              (let ([d (distance h pt)])
                                (if (fl< d dist)
                                    (loop pt ray (fx+ index 1) lst-len lst s h d)
                                    (loop pt ray (fx+ index 1) lst-len lst surface hit dist))))))))))))))


  (define (lambert s int ray)
    (let ([n (sphere-normal s int)])
      (flmax 0.0
             (fl+ (fl* (point-x ray) (point-x n))
                  (fl+ (fl* (point-y ray) (point-y n))
                       (fl* (point-z ray) (point-z n)))))))

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
      (unit-vector (fl- (point-x c) (point-x pt))
                   (fl- (point-y c) (point-y pt))
                   (fl- (point-z c) (point-z pt)))))

  (begin
    (let ([counter (box 29)])
      (begin
        (defsphere 32 0.0 -300.0 -1200.0 200.0 0.8)
        (defsphere 31 -80.0 -150.0 -1200.0 200.0 0.7)
        (defsphere 30 70.0 -100.0 -1200.0 200.0 0.9)
        (do ((x -2 (fx+ x 1)))
            ((fx> x 2))
          (do ((z 2 (fx+ z 1)))
              ((fx> z 7))
            (defsphere
              (unbox counter)
              (fl* (fx->fl x) 200.0)
              300.0
              (fl* (fx->fl z) -400.0)
              40.0
              0.75)
            (set-box! counter (fx- (unbox counter) 1))))))
    (tracer 1)))

(time (main))
