#lang typed/racket

(require ;;racket/port
         schml/src/helpers)

(provide (all-defined-out))

(define-type Test-Value (U blame boole integ dynamic function))

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


(: value=? (Any Any . -> . Boolean))
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
      [else
       (and (string? x) (string? y)
            (cond
             [(equal? x y)]
             [(regexp-match y x) #t]
             [else #f]))]))))
      

(: boole=? (boole boole . -> . Boolean))
(define (boole=? x y)
  (eq? (boole-value x) (boole-value y)))

(: integ=? (integ integ . -> . Boolean))
(define (integ=? x y)
  (equal? (integ-value x) (integ-value y)))

#| capture the output of exp on current-output-port and match
   as if it were returning a value from one of our compiled
   programs.
|#
(define-syntax-rule (observe exp)
  (let ([s (with-output-to-string (lambda () exp))])
    (when (trace? 'Out 'All 'Vomit) (logf "program output:\n ~a\n" s))
    (cond
     [(regexp-match #rx".*Int : ([0-9]+)" s) => 
      (lambda (r)
        (integ (cast (string->number (cadr (cast r (Listof String)))) Integer)))]
     [(regexp-match #rx".*Bool : #(t|f)" s) =>
      (lambda (r)
        (boole (not (equal? "f" (cadr (cast r (Listof String)))))))]
     [(regexp-match #rx".*Function : \\?" s) (function)]
     [(regexp-match #rx".*Dynamic : \\?" s) (dynamic)]
     [else (blame #f s)])))
