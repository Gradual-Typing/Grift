#lang typed/racket

(require schml/src/language
	 schml/src/closures/label-lambdas
         schml/src/closures/uncover-free
         schml/src/closures/convert-closures
         schml/src/closures/specify-representation
         schml/src/closures/lift-functions)

(provide make-closures-explicit)

(: make-closures-explicit (Lambda0-Lang Config . -> . Data0-Lang))
(define (make-closures-explicit l0 config)                                     
  (let* ([l1 (label-lambdas l0 config)]
	 [l2 (uncover-free l1 config)]
	 [l3 (convert-closures l2 config)]
	 [l4 (specify-representation l3 config)])
    (lift-functions l4 config)))
