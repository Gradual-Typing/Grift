#lang typed/racket

(require schml/src/language
	 schml/src/data/normalize-context
         schml/src/data/remove-let)

(provide convert-representation)

(: convert-representation (Data0-Lang Config . -> . Data2-Lang))
(define (convert-representation d0 config)                                     
  (let* ([d1 (normalize-context d0 config)]
	 [d2 (remove-let d1 config)])
    d2))
