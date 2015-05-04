#lang typed/racket/base
#|
This is a micro compiler that removes the cast language form.
|#

(provide (all-defined-out))
(require schml/src/helpers)
(require schml/src/language
         schml/src/casts/introduce-castable-references
         schml/src/casts/introduce-castable-functions
         schml/src/casts/interpret-casts
         schml/src/casts/label-lambdas
         schml/src/casts/uncover-free
         schml/src/casts/convert-closures
         schml/src/casts/specify-representation)


(: impose-cast-semantics (Cast0-Lang Config . -> . Data0-Lang))
(trace-define (impose-cast-semantics prgm config)
  (let* ([c1 (introduce-castable-functions prgm config)]
           [c2 (introduce-castable-references c1 config)]
           [c3 (interpret-casts c2 config)]
           [l1 (label-lambdas c3 config)]
           [l2 (uncover-free l1 config)]
           [l3 (convert-closures l2 config)])
      (specify-representation l3 config)))
