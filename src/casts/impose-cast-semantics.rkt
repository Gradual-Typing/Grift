#lang typed/racket/base
#| 
This is a micro compiler that removes the cast language form.
|#

(provide (all-defined-out))
(require schml/src/language)

(: impose-cast-semantics (Cast0-Lang Config . -> . Lambda0-Lang))
(define (impose-cast-semantics prgm config)
  (local-require schml/src/casts/introduce-castable-functions
		 schml/src/casts/interpret-casts
                 schml/src/casts/specify-representation)
  (let* ((c1 (introduce-castable-functions prgm config))
         (c2 (interpret-casts c1 config))) 
    (specify-representation c2 config)))
