#lang racket/base

(require racket/flonum racket/format racket/fixnum)

(define inv-sqrt-2x-pi 0.39894228040143270286)

(define (cummulative-normal-distribution InputX)
  (let ([sign (fl< InputX #i0.0)]
        [x-input (if (fl< InputX #i0.0) (fl* InputX #i-1.0) InputX)])
    (let ([exp-values (flexp (fl* #i-0.5 (fl* x-input x-input)))])
      (let ([n-prime-of-x (fl* exp-values inv-sqrt-2x-pi)]
            [x-k2 (fl/ #i1.0 (fl+ #i1.0 (fl* #i0.2316419 x-input)))])
        (let ([x-k2^2 (fl* x-k2 x-k2)])
          (let ([x-k2^3 (fl* x-k2^2 x-k2)])
            (let ([x-k2^4 (fl* x-k2^3 x-k2)])
              (let ([x-k2^5 (fl* x-k2^4 x-k2)])
                (let ([x1 (fl* 0.319381530  x-k2)]
                      [x2 (fl* -0.356563782 x-k2^2)]
                      [x3 (fl* 1.781477937  x-k2^3)]
                      [x4 (fl* -1.821255978 x-k2^4)]
                      [x5 (fl* 1.330274429  x-k2^5)])
                  (let ([x (fl+ x1 (fl+ x5 (fl+ x4 (fl+ x2 x3))))])
                    (let ([x (fl- #i1.0 (fl* x n-prime-of-x))])
                      (let ([OutputX (if sign (fl- #i1.0 x) x)])
                        OutputX))))))))))))

(define (black-scholes spot strike rate volatility time option-type timet)
  (let ([log (fllog (fl/ spot strike))]
        [pow (fl* #i0.5 (fl* volatility volatility))]
        [den (fl* volatility (flsqrt time))])
    (let ([d1 (fl/ (fl+ log (fl* time (fl+ rate pow))) den)])
      (let ([d2 (fl- d1 den)])
        (let ([n-of-d1 (cummulative-normal-distribution d1)]
              [n-of-d2 (cummulative-normal-distribution d2)]
              [fut-value (fl* strike (flexp (fl* #i-1 (fl* rate time))))])
          (let ([price (if (fx= option-type 0)
                           (fl- (fl* spot n-of-d1) (fl* fut-value n-of-d2))
                           (fl- (fl* fut-value (fl- #i1.0 n-of-d2))
                                (fl* spot (fl- #i1.0 n-of-d1))))])
            price))))))

(define (make-option spot-price strike-price rfi-rate
                     divr volatility time option-type
                     divs DerivGem-value)
  (vector spot-price strike-price rfi-rate
          divr volatility time option-type
          divs DerivGem-value))

(define (read-option-type)
  (let ([c (read-char)])
    (if (fx= (char->integer c) (char->integer #\P))
        c
        (if (fx= (char->integer c) (char->integer #\C))
            c
            (if (fx= (char->integer c) (char->integer #\space))
                (read-char)
                (read-char))))));; raise error?

(define (read-option)
  (let ([spot-price (read)])
    (let ([strike-price  (read)])
      (let ([rfi-rate (read)])
        (let ([dividend-rate (read)])
          (let ([volatility (read)])
            (let ([maturity-len (read)])
              (let ([option-type (read-option-type)])
                (let ([divs (read)])
                  (let ([DerivGem-value (read)])
                    (make-option spot-price strike-price rfi-rate
                                 dividend-rate volatility maturity-len
                                 option-type divs DerivGem-value)))))))))))

(define number-of-runs 100)

(define (main)
  (define number-of-options (read))

  (define fake-data '#(#i0 #i0 #i0 #i0 #i0 #i0 #\P #i0 #i0))

  (define data
    (let ([v (make-vector number-of-options fake-data)])
      (for ([i (in-range 0 number-of-options)])
        (vector-set! v i (read-option)))
      v))
  

  ;; This seems really dumb but I am doing it because
  ;; this is the way the original benchmark did it.
  (define spots (make-vector number-of-options #i0))
  (define strikes (make-vector number-of-options #i0))
  (define rates  (make-vector number-of-options #i0))
  (define volatilities  (make-vector number-of-options #i0))
  (define otypes (make-vector number-of-options 0))
  (define otimes 
    ;; This is done this way to prevent the unit value
    ;; from printing out at the top level.
    (let ([otimes (make-vector number-of-options #i0)])
      (for ([i (in-range 0 number-of-options)])
        (let ([od (vector-ref data i)])
          (vector-set! otypes i
                       (if (fx= (char->integer (vector-ref od 6))
                                (char->integer #\P))
                           1
                           0))
          (vector-set! spots i (vector-ref od 0))
          (vector-set! strikes i (vector-ref od 1))
          (vector-set! rates i (vector-ref od 2))
          (vector-set! volatilities i (vector-ref od 4))
          (vector-set! otimes i (vector-ref od 5))))
      otimes))

  (define prices 
    (let ([prices (make-vector number-of-options #i0)])
      (for* ([j (in-range 0 number-of-runs)]
             [i (in-range 0 number-of-options)])
        (vector-set! prices i
                     (black-scholes (vector-ref spots i)
                                    (vector-ref strikes i)
                                    (vector-ref rates i)
                                    (vector-ref volatilities i)
                                    (vector-ref otimes i)
                                    (vector-ref otypes i)
                                    #i0)))
      prices))

  (for ([i (in-range 0 number-of-options)])
    (display (~r (vector-ref prices i) #:precision '(= 18)))
    (newline)))

(time (main))


