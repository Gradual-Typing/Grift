#lang racket

(require Schml/framework/build-compiler
         Schml/compiler/closures/remove-anonymous-lambdas
         ;;Schml/compiler/closures/sanitize-binding-forms
         )

(provide make-closures-explicit)

(define-pass (make-closures-explicit ast config)
  (compose-compiler (ast config)
                    remove-anonymous-lambdas
                    ;;sanitize-binding-forms
                    ;;uncover-free
                    ;;convert-closure
                    ;;introduce-procedure-primitives
                    ))
