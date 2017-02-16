#!gsi-script

;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/
;;
;; Derived by Bradley Lucier from the Ikarus variant
;; Derived by Michael D. Adams from the MzScheme variant
;; Taken by Andre Kuhlenschmidt from http://gambitscheme.org/wiki/index.php/Programming_language_shootout

#|
Correct output N = 1000 is

-0.169075164
-0.169087605
|#

(define-macro (unless test . body)
  `(if (not ,test)
       (begin
	 ,@body)))

;;; Stupid boiler-plate for formatting floating point values
(define (roundto digits n)
  (let* ([e (expt 10 digits)]
         [num (round (abs (* e (inexact->exact n))))]
         [str (number->string (modulo num e))])
    (string-append
     (if (negative? n) "-" "")
     (number->string (quotient num e))
     "."
     (make-string (- digits (string-length str)) #\0)
     str)))

;; ------------------------------
;; define planetary masses, initial positions & velocity

(define *pi* 3.141592653589793)
(define *days-per-year* 365.24)

(define *solar-mass* (fl* 4.0 *pi* *pi*))

(define (make-body x y z vx vy vz mass)
  (vector x y z vx vy vz mass))

(define (body-x body)           (vector-ref  body 0))
(define (body-x-set! body val)  (vector-set! body 0 val))
(define (body-y body)           (vector-ref  body 1))
(define (body-y-set! body val)  (vector-set! body 1 val))
(define (body-z body)           (vector-ref  body 2))
(define (body-z-set! body val)  (vector-set! body 2 val))
(define (body-vx body)          (vector-ref  body 3))
(define (body-vx-set! body val) (vector-set! body 3 val))
(define (body-vy body)          (vector-ref  body 4))
(define (body-vy-set! body val) (vector-set! body 4 val))
(define (body-vz body)          (vector-ref  body 5))
(define (body-vz-set! body val) (vector-set! body 5 val))
;; mass is immutable
(define (body-mass body) (vector-ref body 6))


(define *sun*
  (make-body 0.0 0.0 0.0 0.0 0.0 0.0 *solar-mass*))

(define *jupiter*
  (make-body 4.84143144246472090
             -1.16032004402742839
             -1.03622044471123109e-1
             (fl* 1.66007664274403694e-3 *days-per-year*)
             (fl* 7.69901118419740425e-3 *days-per-year*)
             (fl* -6.90460016972063023e-5 *days-per-year*)
             (fl* 9.54791938424326609e-4 *solar-mass*)))

(define *saturn*
  (make-body 8.34336671824457987
             4.12479856412430479
             -4.03523417114321381e-1
             (fl* -2.76742510726862411e-3 *days-per-year*)
             (fl* 4.99852801234917238e-3 *days-per-year*)
             (fl* 2.30417297573763929e-5 *days-per-year*)
             (fl* 2.85885980666130812e-4 *solar-mass*)))

(define *uranus*
  (make-body 1.28943695621391310e1
             -1.51111514016986312e1
             -2.23307578892655734e-1
             (fl* 2.96460137564761618e-03 *days-per-year*)
             (fl* 2.37847173959480950e-03 *days-per-year*)
             (fl* -2.96589568540237556e-05 *days-per-year*)
             (fl*  4.36624404335156298e-05 *solar-mass*)))

(define *neptune*
  (make-body 1.53796971148509165e+01
             -2.59193146099879641e+01
             1.79258772950371181e-01
             (fl* 2.68067772490389322e-03 *days-per-year*)
             (fl* 1.62824170038242295e-03 *days-per-year*)
             (fl* -9.51592254519715870e-05 *days-per-year*)
             (fl* 5.15138902046611451e-05 *solar-mass*)))

;; -------------------------------
(define (offset-momentum system)
  (let loop-i ((i system) (px 0.0) (py 0.0) (pz 0.0))
    (if (null? i)
        (begin
          (body-vx-set! (car system) (fl/ (fl- px) *solar-mass*))
          (body-vy-set! (car system) (fl/ (fl- py) *solar-mass*))
          (body-vz-set! (car system) (fl/ (fl- pz) *solar-mass*)))
        (loop-i (cdr i)
                (fl+ px (fl* (body-vx (car i)) (body-mass (car i))))
                (fl+ py (fl* (body-vy (car i)) (body-mass (car i))))
                (fl+ pz (fl* (body-vz (car i)) (body-mass (car i))))))))

;; -------------------------------
(define (energy system)
  (let loop-o ((o system) (e 0.0))
    (if (null? o)
        e
        (let ([e (fl+ e (fl* 0.5 (body-mass (car o))
                             (fl+ (fl* (body-vx (car o)) (body-vx (car o)))
                                  (fl* (body-vy (car o)) (body-vy (car o)))
                                  (fl* (body-vz (car o)) (body-vz (car o))))))])
          (let loop-i ((i (cdr o)) (e e))
            (if (null? i)
                (loop-o (cdr o) e)
                (let* ((dx (fl- (body-x (car o)) (body-x (car i))))
                       (dy (fl- (body-y (car o)) (body-y (car i))))
                       (dz (fl- (body-z (car o)) (body-z (car i))))
                       (distance (flsqrt (fl+ (fl* dx dx) (fl* dy dy) (fl* dz dz)))))
                  (let ([e  (fl- e (fl/ (fl* (body-mass (car o)) (body-mass (car i))) distance))])
                    (loop-i (cdr i) e)))))))))

;; -------------------------------
(define (advance system dt)
  (let loop-o ((o system))
    (unless (null? o)
            (let loop-i ((i (cdr o)))
              (unless (null? i)
                      (let* ((o1 (car o))
                             (i1 (car i))
                             (dx (fl- (body-x o1) (body-x i1)))
                             (dy (fl- (body-y o1) (body-y i1)))
                             (dz (fl- (body-z o1) (body-z i1)))
                             (distance (flsqrt (fl+ (fl* dx dx)
                                                    (fl* dy dy)
                                                    (fl* dz dz))))
                             (mag (fl/ dt (fl* distance distance distance)))
                             (dxmag (fl* dx mag))
                             (dymag (fl* dy mag))
                             (dzmag (fl* dz mag))
                             (om (body-mass o1))
                             (im (body-mass i1)))
                        (body-vx-set! o1 (fl- (body-vx o1) (fl* dxmag im)))
                        (body-vy-set! o1 (fl- (body-vy o1) (fl* dymag im)))
                        (body-vz-set! o1 (fl- (body-vz o1) (fl* dzmag im)))
                        (body-vx-set! i1 (fl+ (body-vx i1) (fl* dxmag om)))
                        (body-vy-set! i1 (fl+ (body-vy i1) (fl* dymag om)))
                        (body-vz-set! i1 (fl+ (body-vz i1) (fl* dzmag om)))
                        (loop-i (cdr i)))))
            (loop-o (cdr o))))
  (let loop-o ((o system))
    (unless (null? o)
            (let ([o1 (car o)])
              (body-x-set! o1 (fl+ (body-x o1) (fl* dt (body-vx o1))))
              (body-y-set! o1 (fl+ (body-y o1) (fl* dt (body-vy o1))))
              (body-z-set! o1 (fl+ (body-z o1) (fl* dt (body-vz o1))))
              (loop-o (cdr o))))))

;; -------------------------------
(define (main . args)
  (let ((n (if (fx< (length args) 1)
               (read)
               (string->number (car args))))
        (system (list *sun* *jupiter* *saturn* *uranus* *neptune*)))
    (offset-momentum system)
    (display (roundto 9 (energy system))) (newline)
    (do ((i 1 (fx+ i 1)))
        ((fx< n i))
      (advance system 0.01))
    (display (roundto 9 (energy system))) (newline)))
