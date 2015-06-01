#lang typed/racket

(require schml/src/language
	 schml/src/data/normalize-context
         schml/src/data/remove-let
         schml/src/data/remove-complex-opera
         schml/src/data/flatten-assignments
         schml/src/data/lift-effects)

(provide convert-representation)

(: convert-representation (Data0-Lang Config . -> . Data5-Lang))
(define (convert-representation d0 config)
  (let* ([d1 (normalize-context d0 config)]
         [d2 (remove-let d1 config)]
         [d3 (remove-complex-opera d2 config)]
         [d4 (flatten-assignments d3 config)]
         [d5 (lift-effects d4 config)])
    d5))
