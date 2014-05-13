#lang racket
(require Schml/framework/build-compiler)
(require Schml/compiler/read
         Schml/compiler/parse)
(provide compile compiler)

#|
The Compiler is composed of passes which themselves may be composed
of multiple passes.

Passes - Description :
reader :: file-name -> syntax-objects
       - uses a restricted subset of s-expr syntax to tokenize the file
       - and record source information
parser :: syntax-objects -> implicitly-typed core forms
       - checks that syntax is correct
impose-type-semantics :: implicitly-typed core -> ?
impose-cast-semantics (typcheck, insert-implicit-casts, eliminate-casts) 
convert-closures 
		    ;; purify-letrec
		    ;; optimize-direct
		    ;; remove-annon-lambda
		    ;; sanitize-binding-forms
		    ;; uncover-free
		    ;; convert-closures
		    ;; analyze-closure-size
		    ;; optimize-known-call
		    ;; optimize-self-reference
		    ;; introduce-closure-primitives
optimizations - passes that make the compiler slower and the program faster
		    ;; inlining 
		    ;; constant-folding
		    ;; dead-code
		    ;; copy-propagation
convert-data - stackframes, heap allocations, data it is all the same
code-gen - covert a c-level language to the assembly il
assemble - convert a code tree to the targeted assembly language
   ;; Assembly))

|#

;; The compiler takes an s-expr and performs the various compiler
;; passes listed bellow on the s-expr after parsing the expression
;; will be a collection of records with a pretty printing function.
(define (compiler file-name settings) 
  (compose-compiler (file-name settings)
                    read parse
                    ))

;; Since #f is a valid expression a different value is
;; use to represent false for the expression variable.
(define hidden-false (gensym))

;; Invokes the compiler on an s-expr obtained by various means
(define compile
  (lambda (#:file-name [f #f])
    (compiler f (compiler-config 'ld 'all 'all))))

