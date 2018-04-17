#lang typed/racket/base
;;; 10/9/2017 changed to use internal timing (Andre)
;;;
;;; We actively choose to not use racket racket/fixnum. Use of generic
;;; numeric ops is disadvantage for racket but there is no safe
;;; version of fixnum operations that avoids the overhead of
;;; contracts, and we are only interested in comparing safe code.  The
;;; racket/fixnum safe operations are generally no faster than using
;;; generic primitives like +. (According to the documentation)

(require racket/flonum)

(define (main)
  (define n
    (let ([n (read)])
      (unless (fixnum? n)
        (error 'fft.rkt "invalid input: expected fixnum"))
      n))

  (define data : (Vectorof Flonum) (make-vector n #i0.0))

  (define pi*2 : Flonum #i6.28318530717959) ; to compute the inverse, negate this value

  (: loop1 : Integer Integer -> Void)
  (define (loop1 i j)
    (if (< i n)
        (begin
          (if (< i j)
              (begin
                (let ([temp (vector-ref data i)])
                  (begin
                    (vector-set! data i (vector-ref data j))
                    (vector-set! data j temp)))
                (let ([temp (vector-ref data (+ i 1))])
                  (begin
                    (vector-set! data (+ i 1) (vector-ref data (+ j 1)))
                    (vector-set! data (+ j 1) temp))))
              (void))
          (loop2 (quotient n 2) j i))
        (void)))

  (: loop2 : Integer Integer Integer -> Void)
  (define (loop2 m j i)
    (if (and (>= m 2) (>= j m))
        (loop2 (quotient m 2) (- j m) i)
        (loop1 (+ i 2) (+ j m))))

  (: loop3 : Integer -> Void)
  (define (loop3 mmax)
    (if (< mmax n)
        (let ([theta (fl/ pi*2 (->fl mmax))])
          (let ([wpr (let ([x (flsin (fl* #i0.5 theta))])
                       (fl* #i-2.0 (fl* x x)))]
                [wpi (flsin theta)])
            (begin
              (loop4 #i1.0 #i0.0 0 mmax wpr wpi)
              (loop3 (* mmax 2)))))
        (void)))

  (: loop4 : Flonum Flonum Integer Integer Flonum Flonum -> Void)
  (define (loop4 wr wi m mmax wpr wpi)
    (if (< m mmax)
        (loop5 m mmax wr wi m wpr wpi)
        (void)))

  (: loop5 : Integer Integer Flonum Flonum Integer Flonum Flonum -> Void)
  (define (loop5 i mmax wr wi m wpr wpi)
    (if (< i n)
        (let ([j (+ i mmax)])
          (let ([tempr
                 (fl-
                  (fl* wr (vector-ref data j))
                  (fl* wi (vector-ref data (+ j 1))))]
                [tempi
                 (fl+
                  (fl* wr (vector-ref data (+ j 1)))
                  (fl* wi (vector-ref data j)))])
            (begin
              (vector-set! data j
                           (fl- (vector-ref data i) tempr))
              (vector-set! data (+ j 1)
                           (fl- (vector-ref data (+ i 1)) tempi))
              (vector-set! data i
                           (fl+ (vector-ref data i) tempr))
              (vector-set! data (+ i 1)
                           (fl+ (vector-ref data (+ i 1)) tempi))
              (loop5 (+ j mmax) mmax wr wi m wpr wpi))))
        (loop4 (fl+ (fl- (fl* wr wpr) (fl* wi wpi)) wr)
               (fl+ (fl+ (fl* wi wpr) (fl* wr wpi)) wi)
               (+ m 2)
               mmax wpr wpi)))

  (begin
    (loop1 0 0) ;; bit-reversal section
    (loop3 2)   ;; Danielson-Lanczos section
    (display (vector-ref data 0))
    (newline)))

(time (main))
