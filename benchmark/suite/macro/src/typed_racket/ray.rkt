#lang typed/racket/base

(require racket/flonum racket/format)

;;; RAY -- Ray-trace a simple scene with spheres.
;;; Translated to Scheme from Paul Graham's book ANSI Common Lisp, Example 9.8
;;;
;;; And then translated to Racket by Deyaaeldeen Almahallawi
;;; We actively choose to not use racket racket/fixnum. Use of generic
;;; numeric ops is disadvantage for racket but there is no safe
;;; version of fixnum operations that avoids the overhead of
;;; contracts, and we are only interested in comparing safe code.  The
;;; racket/fixnum safe operations are generally no faster than using
;;; generic primitives like +. (According to the documentation)
;;;
;;; 9/27/2017 Added types for typed-racket Andre Kuhlenschmidt
;;; 10/9/2017 changed to use internal timing (Andre)

(define-type Point (Vector Flonum Flonum Flonum))
(: make-point : Flonum Flonum Flonum -> Point)
(define (make-point x y z)
  (vector x y z))

(: point-x : Point -> Flonum)
(define (point-x p) (vector-ref p 0))
(: point-y : Point -> Flonum)
(define (point-y p) (vector-ref p 1))
(: point-z : Point -> Flonum)
(define (point-z p) (vector-ref p 2))

(: sq : Flonum -> Flonum)
(define (sq x) (fl* x x))

(: mag : Flonum Flonum Flonum -> Flonum)
(define (mag x y z) 
  (flsqrt (fl+ (sq x) (fl+ (sq y) (sq z)))))

(: unit-vector : Flonum Flonum Flonum -> Point)
(define (unit-vector x y z) 
  (let ([d  (mag x y z)])
    (make-point (fl/ x d) (fl/ y d) (fl/ z d))))

(: distance : Point Point -> Float)
(define (distance p1 p2) 
  (mag (fl- (point-x p1) (point-x p2))
       (fl- (point-y p1) (point-y p2))
       (fl- (point-z p1) (point-z p2))))

(define-type Surface (Vector Flonum Flonum Point))
(define-type World (Vectorof Surface))
(define-type Loop-Result (Vector (Option Surface) (Option Point)))

(define (main)
  (define *world* (make-vector 33 (vector 0.0 0.0 (vector 0.0 0.0 0.0))))

  (: eye : Point)
  (define eye (make-point 0.0 0.0 200.0))

  (: tracer : Integer -> Void)
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
                  (fl+ -50.0
                       (fl/ (->fl x) (->fl res)))
                  (fl+ -50.0
                       (fl/ (->fl y) (->fl res)))))
          (newline)))))

  (: color-at : Flonum Flonum -> Integer)
  (define (color-at x y)
    (let ([ray (unit-vector (fl- x (point-x eye))
                            (fl- y (point-y eye))
                            (fl* -1.0 (point-z eye)))])
      (fl->exact-integer (flround (fl* (sendray eye ray) 255.0)))))


  (: sendray : Point Point -> Flonum)
  (define (sendray pt ray)
    (let ([x : Loop-Result
             (loop pt ray 0
                   (vector-length *world*)
                   *world*
                   #f
                   #f
                   1e308)])
      (let ([s (vector-ref x 0)]
            [int (vector-ref x 1)])
        ;; Had to add check of int to make type correct
        (if (and s int)
            (fl* (lambert s int ray)
                 (sphere-color s))
            0.0))))

  (: loop : Point Point Integer Integer World (Option Surface) (Option Point) Flonum
     -> Loop-Result)
  (define (loop pt ray index lst-len lst surface hit dist)
    (if (= index lst-len)
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
                            (loop pt ray (+ index 1) lst-len lst s h d)
                            (loop pt ray (+ index 1) lst-len lst surface hit dist)))))
                  (let ([disc (fl- (sq b) (fl* 4.0 (fl* a c)))])
                    (if (negative? disc)
                        (loop pt ray (+ index 1) lst-len lst surface hit dist)
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
                                    (loop pt ray (+ index 1) lst-len lst s h d)
                                    (loop pt ray (+ index 1) lst-len lst surface hit dist))))))))))))))

  (: lambert : Sphere Point Point -> Flonum)
  (define (lambert s int ray)
    (let ([n (sphere-normal s int)])
      (flmax 0.0
             (fl+ (fl* (point-x ray) (point-x n))
                  (fl+ (fl* (point-y ray) (point-y n))
                       (fl* (point-z ray) (point-z n)))))))

  (define-type Color Flonum)
  (define-type Radius Flonum)
  (define-type Sphere (Vector Color Radius Point))
  (: make-sphere : Color Radius Point -> Sphere)
  (define (make-sphere color radius center) 
    (vector color radius center))

  (: sphere-color : Sphere -> Color)
  (define (sphere-color s) 
    (vector-ref s 0))
  (: sphere-radius : Sphere -> Radius)
  (define (sphere-radius s) 
    (vector-ref s 1))
  (: sphere-center : Sphere -> Point)
  (define (sphere-center s) 
    (vector-ref s 2))

  (: defsphere : Integer Flonum Flonum Flonum Radius Color -> Sphere)
  (define (defsphere i x y z r c) 
    (let ([s (make-sphere c r (make-point x y z))])
      (begin
        (vector-set! *world* i s)
        s)))

  (: sphere-normal : Sphere Point -> Point)
  (define (sphere-normal s pt) 
    (let ([c (sphere-center s)])
      (unit-vector (fl- (point-x c) (point-x pt))
                   (fl- (point-y c) (point-y pt))
                   (fl- (point-z c) (point-z pt)))))

  (begin
    (let ([counter : (Boxof Integer) (box 29)])
      (begin
        (defsphere 32 0.0 -300.0 -1200.0 200.0 0.8)
        (defsphere 31 -80.0 -150.0 -1200.0 200.0 0.7)
        (defsphere 30 70.0 -100.0 -1200.0 200.0 0.9)
        (do : Void
          ((x : Integer -2 (+ x 1)))
          ((> x 2))
          (do : Void ((z : Integer 2 (+ z 1)))
              ((> z 7))
              (defsphere
                (unbox counter)
                (fl* (->fl x) 200.0)
                300.0
                (fl* (->fl z) -400.0)
                40.0
                0.75)
              (set-box! counter (- (unbox counter) 1))))))
    (tracer 1)))

(time (main))
