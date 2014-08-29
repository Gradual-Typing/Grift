#lang typed/racket
#| 
This is a micro compiler that removes the cast language form.
|#
(require Schml/framework/build-compiler)
(provide (all-defined-out))
(require Schml/compiler/language)
(: impose-cast-semantics (Cast0-Lang Config . -> . Lambda0-Lang))
(define (impose-cast-semantics prgm config)
  (local-require Schml/compiler/casts/introduce-castable-functions
		 Schml/compiler/casts/interpret-casts)
  (let ((c1 (introduce-castable-functions prgm config)))
    (interpret-casts c1 config)))
