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
(: path->cast-calculus (Path Config . -> . Cast-Prog))
(define (path->cast-calculus path config)
  (local-require Schml/compiler/schml/read
		 Schml/compiler/schml/parse
		 Schml/compiler/schml/type-check
		 Schml/compiler/schml/insert-implicit-casts)
  (let* ((stx-tree (read path config))
	 (core-forms (parse stx-tree config))
	 (well-typed (type-check core-forms config)))
    (insert-implicit-casts well-typed config)))
