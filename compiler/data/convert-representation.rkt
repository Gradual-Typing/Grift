#lang typed/racket

(require schml/framework/build-compiler
         schml/compiler/language
	 schml/compiler/data/normalize-context
         schml/compiler/data/remove-let)

(provide convert-representation)

(: convert-representation (Data0-Lang Config . -> . Data2-Lang))
(define (convert-representation d0 config)                                     
  (let* ([d1 (normalize-context d0 config)]
	 [d2 (remove-let d1 config)])
    d2))
