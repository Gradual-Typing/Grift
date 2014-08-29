#lang typed/racket
#| 
This is a micro compiler that takes a path, reads the contents
of that file, parses the syntax into an ast, type-checks the
ast, and finally converts that ast into an equivalent ast
of the cast calculus.
|#
(require Schml/framework/build-compiler)
(provide (all-defined-out))
(require Schml/compiler/language)

(: reduce-to-cast-calculus (Path Config . -> . Cast0-Lang))
(define (reduce-to-cast-calculus path config)
  (local-require Schml/compiler/schml/read
		 Schml/compiler/schml/parse
		 Schml/compiler/schml/type-check
		 Schml/compiler/schml/insert-implicit-casts)
  (let* ((stx-lang (read path config))
	 (s0 (parse stx-lang config))
	 (s1 (type-check s0 config)))
    (insert-implicit-casts s1 config)))
