#lang racket
(require Schml/framework/build-compiler)
(provide compile compiler)

;; The compiler takes an s-expr and performs the various compiler
;; passes listed below on the s-expr after parsing the expression
;; will be a collection of records with a pretty printing function.
(define (compiler file-name settings) 
  (local-require Schml/compiler/read
                 Schml/compiler/parse
                 Schml/compiler/type-check
                 Schml/compiler/casts/make-casts-explicit
                 Schml/compiler/closures/make-closures-explicit)
  (compose-compiler (file-name settings)
                    read
                    parse
                    type-check
                    make-casts-explicit
                    make-closures-explicit))

;; Since #f is a valid expression a different value is
;; use to represent false for the expression variable.
(define hidden-false (gensym))

;; Invokes the compiler on an s-expr obtained by various means
(define compile
  (lambda (#:file-name [f #f])
    (compiler f (compiler-config 'ld 'all 'all))))

