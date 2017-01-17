#!gsi-script -:d0

(define n 4096)
(define data (make-vector n #i0.0))
(define pi*2 #i6.28318530717959) ; to compute the inverse, negate this value

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

(define (loop2 m j i)
  (if (and (>= m 2) (>= j m))
      (loop2 (quotient m 2) (- j m) i)
      (loop1 (+ i 2) (+ j m))))

(define (loop3 mmax)
  (if (< mmax n)
      (let ([theta (/ pi*2 (exact->inexact mmax))])
        (let ([wpr (let ([x (sin (* #i0.5 theta))])
		     (* #i-2.0 (* x x)))]
              [wpi (sin theta)])
          (begin
            (loop4 #i1.0 #i0.0 0 mmax wpr wpi)
            (loop3 (* mmax 2)))))
      (void)))

(define (loop4 wr wi m mmax wpr wpi)
  (if (< m mmax)
      (loop5 m mmax wr wi m wpr wpi)
      (void)))

(define (loop5 i mmax wr wi m wpr wpi)
  (if (< i n)
      (let ([j (+ i mmax)])
        (let ([tempr
	       (-
		(* wr (vector-ref data j))
		(* wi (vector-ref data (+ j 1))))]
              [tempi
	       (+
		(* wr (vector-ref data (+ j 1)))
		(* wi (vector-ref data j)))])
          (begin
            (vector-set! data j
                         (- (vector-ref data i) tempr))
            (vector-set! data (+ j 1)
                         (- (vector-ref data (+ i 1)) tempi))
            (vector-set! data i
                         (+ (vector-ref data i) tempr))
            (vector-set! data (+ i 1)
                         (+ (vector-ref data (+ i 1)) tempi))
            (loop5 (+ j mmax) mmax wr wi m wpr wpi))))
      (loop4 (+ (- (* wr wpr) (* wi wpi)) wr)
             (+ (+ (* wi wpr) (* wr wpi)) wi)
             (+ m 2)
             mmax wpr wpi)))

(define (main . args)
  (begin
    (loop1 0 0) ;; bit-reversal section
    (loop3 2)   ;; Danielson-Lanczos section
    (display (vector-ref data 0))))
