#lang racket/base

(require racket/flonum racket/format racket/fixnum)

(define (main)
  (define n (read))
  (define data (make-vector n #i0.0))
  (define pi*2 #i6.28318530717959) ; to compute the inverse, negate this value

  (define (loop1 i j)
    (if (fx< i n)
        (begin
          (if (fx< i j)
              (begin
                (let ([temp (vector-ref data i)])
                  (begin
                    (vector-set! data i (vector-ref data j))
                    (vector-set! data j temp)))
                (let ([temp (vector-ref data (fx+ i 1))])
                  (begin
                    (vector-set! data (fx+ i 1) (vector-ref data (fx+ j 1)))
                    (vector-set! data (fx+ j 1) temp))))
              (void))
          (loop2 (quotient n 2) j i))
        (void)))

  (define (loop2 m j i)
    (if (and (fx>= m 2) (fx>= j m))
        (loop2 (quotient m 2) (fx- j m) i)
        (loop1 (fx+ i 2) (fx+ j m))))

  (define (loop3 mmax)
    (if (fx< mmax n)
        (let ([theta (fl/ pi*2 (exact->inexact mmax))])
          (let ([wpr (let ([x (flsin (fl* #i0.5 theta))])
                       (fl* #i-2.0 (fl* x x)))]
                [wpi (flsin theta)])
            (begin
              (loop4 #i1.0 #i0.0 0 mmax wpr wpi)
              (loop3 (fx* mmax 2)))))
        (void)))

  (define (loop4 wr wi m mmax wpr wpi)
    (if (fx< m mmax)
        (loop5 m mmax wr wi m wpr wpi)
        (void)))

  (define (loop5 i mmax wr wi m wpr wpi)
    (if (fx< i n)
        (let ([j (fx+ i mmax)])
          (let ([tempr
                 (fl-
                  (fl* wr (vector-ref data j))
                  (fl* wi (vector-ref data (fx+ j 1))))]
                [tempi
                 (fl+
                  (fl* wr (vector-ref data (fx+ j 1)))
                  (fl* wi (vector-ref data j)))])
            (begin
              (vector-set! data j
                           (fl- (vector-ref data i) tempr))
              (vector-set! data (fx+ j 1)
                           (fl- (vector-ref data (fx+ i 1)) tempi))
              (vector-set! data i
                           (fl+ (vector-ref data i) tempr))
              (vector-set! data (fx+ i 1)
                           (fl+ (vector-ref data (fx+ i 1)) tempi))
              (loop5 (fx+ j mmax) mmax wr wi m wpr wpi))))
        (loop4 (fl+ (fl- (fl* wr wpr) (fl* wi wpi)) wr)
               (fl+ (fl+ (fl* wi wpr) (fl* wr wpi)) wi)
               (fx+ m 2)
               mmax wpr wpi)))

  (begin
    (loop1 0 0) ;; bit-reversal section
    (loop3 2)   ;; Danielson-Lanczos section
    (vector-ref data 0)))

(time (main))
