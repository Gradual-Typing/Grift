#lang typed/racket/base

;; The Computer Language Benchmarks Game
;; http://benchmarksgame.alioth.debian.org/
;;
;; Imperative-style implementation based on the SBCL implementation by
;; Patrick Frankenberger and Juho Snellman, but using only native Scheme
;; idioms like 'named let' and 'do' special form.
;;
;; Contributed by Anthony Borla, then converted for Racket
;; by Matthew Flatt and Brent Fulgham
;; Made unsafe and optimized by Sam TH
;; Made safe by Andre Kuhlenschmidt
;; Added types for use with typed-racket by Andre Kuhlenschmidt
;; 10/9/2017 changed to use internal timing (Andre)
#|
Correct output N = 1000 is

-0.169075164
-0.169087605
|#

;; Removed racket/cmline and racket/require because we
;; altered this code to read the input from standard in
;; in order to be uniform with other benchmarks in this
;; suite.
;; Removed unsafe operation because we are only interested
;; in safe program performance.
(require 
	 (for-syntax racket/base)
	 racket/flonum
	 racket/fixnum)

;; ------------------------------
;; define planetary masses, initial positions & velocity

(define +pi+ : Flonum 3.141592653589793) ;; define locally to enable inlining
(define +days-per-year+ : Flonum 365.24)

(define +solar-mass+ : Flonum (fl* 4.0 (fl* +pi+ +pi+)))

(define +dt+ : Flonum 0.01)

(define-type Body (Vector Flonum Flonum Flonum Flonum Flonum Flonum Flonum))
(: make-body :  Flonum Flonum Flonum Flonum Flonum Flonum Flonum -> Body)
;; Changed to define to allow type-checking
(define (make-body x y z vx vy vz m)
  (vector x y z vx vy vz m))
(define-syntax-rule (deffield n getter setter)
  (begin (define-syntax-rule (getter b) (vector-ref b n))
         (define-syntax-rule (setter b x) (vector-set! b n x))))
(deffield 0 body-x set-body-x!)
(deffield 1 body-y set-body-y!)
(deffield 2 body-z set-body-z!)
(deffield 3 body-vx set-body-vx!)
(deffield 4 body-vy set-body-vy!)
(deffield 5 body-vz set-body-vz!)
(deffield 6 body-mass set-body-mass!)

(define (main)
  (define *sun* : Body
  (make-body 0.0 0.0 0.0 0.0 0.0 0.0 +solar-mass+))

(define *jupiter* : Body
  (make-body 4.84143144246472090
             -1.16032004402742839
             -1.03622044471123109e-1
             (fl* 1.66007664274403694e-3 +days-per-year+)
             (fl* 7.69901118419740425e-3 +days-per-year+)
             (fl* -6.90460016972063023e-5 +days-per-year+)
             (fl* 9.54791938424326609e-4 +solar-mass+)))

(define *saturn* : Body
  (make-body 8.34336671824457987
             4.12479856412430479
             -4.03523417114321381e-1
             (fl* -2.76742510726862411e-3 +days-per-year+)
             (fl* 4.99852801234917238e-3 +days-per-year+)
             (fl* 2.30417297573763929e-5 +days-per-year+)
             (fl* 2.85885980666130812e-4 +solar-mass+)))

(define *uranus* : Body
  (make-body 1.28943695621391310e1
             -1.51111514016986312e1
             -2.23307578892655734e-1
             (fl* 2.96460137564761618e-03 +days-per-year+)
             (fl* 2.37847173959480950e-03 +days-per-year+)
             (fl* -2.96589568540237556e-05 +days-per-year+)
             (fl*  4.36624404335156298e-05 +solar-mass+)))

(define *neptune* : Body
  (make-body 1.53796971148509165e+01
             -2.59193146099879641e+01
             1.79258772950371181e-01
             (fl* 2.68067772490389322e-03 +days-per-year+)
             (fl* 1.62824170038242295e-03 +days-per-year+)
             (fl* -9.51592254519715870e-05 +days-per-year+)
             (fl* 5.15138902046611451e-05 +solar-mass+)))

(define-type System (Vector Body Body Body Body Body))
(define *system* : System (vector *sun* *jupiter* *saturn* *uranus* *neptune*))
(define *system-size* : Fixnum 5)
;; -------------------------------
(: offset-momentum : -> Void)
(define (offset-momentum)
  (let loop-i ([i : Fixnum 0]
               [px : Flonum 0.0]
               [py : Flonum 0.0]
               [pz : Flonum 0.0])
    (if (fx= i *system-size*)
      (begin
        (set-body-vx! (vector-ref *system* 0) (fl/ (fl- 0.0 px) +solar-mass+))
        (set-body-vy! (vector-ref *system* 0) (fl/ (fl- 0.0 py) +solar-mass+))
        (set-body-vz! (vector-ref *system* 0) (fl/ (fl- 0.0 pz) +solar-mass+)))
      (let ([i1 (vector-ref *system* i)])
        (loop-i (fx+ i 1)
                (fl+ px (fl* (body-vx i1) (body-mass i1)))
                (fl+ py (fl* (body-vy i1) (body-mass i1)))
                (fl+ pz (fl* (body-vz i1) (body-mass i1))))))))

;; -------------------------------
(define (energy)
  (let loop-o : Flonum ([o : Fixnum 0] [e : Flonum 0.0]) 
   (if (fx= o *system-size*)
       e
       (let* ([o1 (vector-ref *system* o)]
              [e (fl+ e (fl* (fl* 0.5 (body-mass o1))
                             (fl+ (fl+ (fl* (body-vx o1) (body-vx o1))
                                       (fl* (body-vy o1) (body-vy o1)))
                                  (fl* (body-vz o1) (body-vz o1)))))])
         (let loop-i : Flonum ([i : Fixnum (fx+ o 1)] [e : Flonum e])
           (if (fx= i *system-size*)
               (loop-o (fx+ o 1) e)
               (let* ([i1   (vector-ref *system* i)]
                      [dx   (fl- (body-x o1) (body-x i1))]
                      [dy   (fl- (body-y o1) (body-y i1))]
                      [dz   (fl- (body-z o1) (body-z i1))]
                      [dist (flsqrt (fl+ (fl+ (fl* dx dx) (fl* dy dy)) (fl* dz dz)))]
                      [e    (fl- e (fl/ (fl* (body-mass o1) (body-mass i1)) dist))])
                 (loop-i (fx+ i 1) e))))))))

;; -------------------------------
(define (advance)
  (let loop-o : Void ([o : Fixnum 0])
    (unless (fx= o *system-size*)
      (let* ([o1 (vector-ref *system* o)])
        (let loop-i ([i  (fx+ o 1)]
                     [vx (body-vx o1)]
                     [vy (body-vy o1)]
                     [vz (body-vz o1)])
          (if (fx< i *system-size*)
            (let* ([i1    (vector-ref *system* i)]
                   [dx    (fl- (body-x o1) (body-x i1))]
                   [dy    (fl- (body-y o1) (body-y i1))]
                   [dz    (fl- (body-z o1) (body-z i1))]
                   [dist2 (fl+ (fl+ (fl* dx dx) (fl* dy dy)) (fl* dz dz))]
                   [mag   (fl/ +dt+ (fl* dist2 (flsqrt dist2)))]
                   [dxmag (fl* dx mag)]
                   [dymag (fl* dy mag)]
                   [dzmag (fl* dz mag)]
                   [om (body-mass o1)]
                   [im (body-mass i1)])
              (set-body-vx! i1 (fl+ (body-vx i1) (fl* dxmag om)))
              (set-body-vy! i1 (fl+ (body-vy i1) (fl* dymag om)))
              (set-body-vz! i1 (fl+ (body-vz i1) (fl* dzmag om)))
              (loop-i (fx+ i 1)
                      (fl- vx (fl* dxmag im))
                      (fl- vy (fl* dymag im))
                      (fl- vz (fl* dzmag im))))
            (begin (set-body-vx! o1 vx)
                   (set-body-vy! o1 vy)
                   (set-body-vz! o1 vz)
                   (set-body-x! o1 (fl+ (body-x o1) (fl* +dt+ vx)))
                   (set-body-y! o1 (fl+ (body-y o1) (fl* +dt+ vy)))
                   (set-body-z! o1 (fl+ (body-z o1) (fl* +dt+ vz)))))))
      (loop-o (fx+ o 1)))))

;; -------------------------------

(let ([n (read)])
  (unless (fixnum? n)
    (error 'n-body.rkt "invalid input: expected integer"))
  (time
   (offset-momentum)
   (printf "~a\n" (real->decimal-string (energy) 9))
   ;; Changed from for loop to do loop to ensure fixnum operations
   (do ([i : Fixnum 0 (fx+ i 1)]) ((fx= i n)) (advance))
   ;; (for ([i (in-range n)]) (advance))
   (printf "~a\n" (real->decimal-string (energy) 9)))))

(time (main))
