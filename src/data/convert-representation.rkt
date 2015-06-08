#lang typed/racket

(require "../language.rkt"
         "./normalize-context.rkt"
         "./remove-let.rkt")

(provide convert-representation)

(: convert-representation (Data0-Lang Config . -> . Data2-Lang))
(define (convert-representation d0 config)
  (let* ([d1 (normalize-context d0 config)]
	 [d2 (remove-let d1 config)])
    d2))
