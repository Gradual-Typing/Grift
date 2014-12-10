#lang typed/racket
#| 
This is a micro compiler that takes a path, reads the contents
of that file, parses the syntax into an ast, type-checks the
ast, and finally converts that ast into an equivalent ast
of the cast calculus.
|#
(provide (all-defined-out))
(require schml/src/language)

(: reduce-to-cast-calculus (Path Config . -> . Cast0-Lang))
(define (reduce-to-cast-calculus path config)
  (local-require schml/src/schml/read
		 schml/src/schml/parse
		 schml/src/schml/type-check
		 schml/src/schml/insert-implicit-casts)
  (let* ((stx-lang (read path config))
	 (s0 (parse stx-lang config))
	 (s1 (type-check s0 config)))
    (insert-implicit-casts s1 config)))
