#lang typed/racket/base
#| 
This is a micro compiler that removes the cast language form.
|#
(require schml/framework/build-compiler)
(provide (all-defined-out))
(require schml/compiler/language)

(: impose-cast-semantics (Cast0-Lang Config . -> . Lambda0-Lang))
(define (impose-cast-semantics prgm config)
  (local-require schml/compiler/casts/introduce-castable-functions
		 schml/compiler/casts/interpret-casts
                 schml/compiler/casts/specify-representation)
  (let* ((c1 (introduce-castable-functions prgm config))
         (c2 (interpret-casts c1 config))) 
    (specify-representation c2 config)))
