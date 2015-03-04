#lang typed/racket/base
#|
This is a micro compiler that removes the cast language form.
|#

(provide (all-defined-out))
(require schml/src/language
         schml/src/casts/introduce-castable-references
         schml/src/casts/introduce-castable-functions
         schml/src/casts/interpret-casts
         schml/src/casts/normalize-context
         schml/src/casts/specify-representation)


(: impose-cast-semantics (Cast0-Lang Config . -> . Lambda0-Lang))
(define (impose-cast-semantics prgm config)
  (let* ([c1 (introduce-castable-functions prgm config)]
         [c2 (introduce-castable-references c1 config)]
         [c3 (interpret-casts c3 config)]
         [l1 (label-lambdas c3 config)]
	 [l2 (uncover-free l1 config)]
	 [l3 (convert-closures l2 config)]
         [l4 (normalize-context l3 config)])
    (specify-representation l4 config)))
