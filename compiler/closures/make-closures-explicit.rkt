#lang racket

(require Schml/framework/build-compiler
         Schml/compiler/closures/label-lambdas
         Schml/compiler/closures/uncover-free
         Schml/compiler/closures/convert-closures
         Schml/compiler/closures/introduce-closure-primitives
         
         )

(provide make-closures-explicit)

(define-pass (make-closures-explicit ast config)
  (compose-compiler (ast config)
                    label-lambdas
                    uncover-free
                    convert-closures
                    introduce-closure-primitives
                    ))
