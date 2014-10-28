#lang typed/racket

(require Schml/framework/build-compiler
         Schml/compiler/language
	 Schml/compiler/closures/label-lambdas
         Schml/compiler/closures/uncover-free
         Schml/compiler/closures/convert-closures
         Schml/compiler/closures/specify-representation
         Schml/compiler/closures/lift-functions)

(provide make-closures-explicit)

(: make-closures-explicit (Lambda0-Lang Config . -> . Data0-Lang))
(define (make-closures-explicit l0 config)                                     
  (let* ([l1 (label-lambdas l0 config)]
	 [l2 (uncover-free l1 config)]
	 [l3 (convert-closures l2 config)]
	 [l4 (specify-representation l3 config)])
    (lift-functions l4 config)))
