#lang typed/racket

(require Schml/framework/build-compiler
         Schml/compiler/language
	 Schml/compiler/closures/label-lambdas
         Schml/compiler/closures/uncover-free
         Schml/compiler/closures/convert-closures
         ;Schml/compiler/closures/introduce-closure-primitives
         ;Schml/compiler/closures/lift-functions
         )

(provide make-closures-explicit)

(: make-closures-explicit (Lambda-Prog Config . -> . Lambda3))
(define (make-closures-explicit prgm config)
  ;introduce-closure-primitives lift-functions
  (let* ([l1 (label-lambdas prgm config)]
	 [l2 (uncover-free l1 config)]
	 [l3 (convert-closures l2 config)])
    l3))
