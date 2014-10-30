#lang typed/racket

(require schml/framework/build-compiler
         schml/compiler/language
	 schml/compiler/closures/label-lambdas
         schml/compiler/closures/uncover-free
         schml/compiler/closures/convert-closures
         schml/compiler/closures/specify-representation
         schml/compiler/closures/lift-functions)

(provide make-closures-explicit)

(: make-closures-explicit (Lambda0-Lang Config . -> . Data0-Lang))
(define (make-closures-explicit l0 config)                                     
  (let* ([l1 (label-lambdas l0 config)]
	 [l2 (uncover-free l1 config)]
	 [l3 (convert-closures l2 config)]
	 [l4 (specify-representation l3 config)])
    (lift-functions l4 config)))
