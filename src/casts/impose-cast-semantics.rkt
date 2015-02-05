#lang typed/racket/base
#| 
This is a micro compiler that removes the cast language form.
|#

(provide (all-defined-out))
(require schml/src/language
         schml/src/casts/introduce-castable-references
         schml/src/casts/introduce-castable-functions
         schml/src/casts/interpret-casts
         schml/src/casts/specify-representation)


(: impose-cast-semantics (Cast0-Lang Config . -> . Lambda0-Lang))
(define (impose-cast-semantics prgm config)
  (let* ((c1 (introduce-castable-references prgm config))
         (c2 (introduce-castable-functions c1 config))
         (c3 (interpret-casts c2 config))) 
    (specify-representation c3 config)))
