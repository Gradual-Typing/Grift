#lang typed/racket

(provide (all-defined-out))

(define-type Value (U blame boole integ dynamic function))

(struct not-lbl ([value : String])
	#:transparent)
(struct blame ([static? : Boolean]
	       [lbl : (U not-lbl String False)])
	#:transparent)
(struct boole ([value : Boolean])
	#:transparent)
(struct integ ([value : Integer])
	#:transparent)
(struct dynamic ()
	#:transparent)
(struct function ()
	#:transparent)

(: value=? (Value Value . -> . Boolean))
(define (value=? x y)
  (or (and (blame? x) (blame? y) (blame=? x y))
      (and (boole? x) (boole? y) (boole=? x y))
      (and (integ? x) (integ? y) (integ=? x y))
      (and (dynamic? x) (dynamic? y))
      (and (function? x) (function? y))))

(: blame=? (blame blame . -> . Boolean))
(define (blame=? x y)
  (and 
   (eq? (blame-static? x) (blame-static? y))
   (let ([x (blame-lbl x)]
	 [y (blame-lbl y)])
     (cond
      [(not-lbl? x) (not (equal? (not-lbl-value x) y))]
      [(not-lbl? y) (not (equal? (not-lbl-value y) x))]
      [(or (not x) (not y)) #t]
      [else (equal? x y)]))))
      

(: boole=? (boole boole . -> . Boolean))
(define (boole=? x y)
  (eq? (boole-value x) (boole-value y)))

(: integ=? (integ integ . -> . Boolean))
(define (integ=? x y)
  (equal? (integ-value x) (integ-value y)))

