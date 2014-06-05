#lang racket

(require Schml/framework/build-compiler
         Schml/compiler/closures/label-lambdas
         ;;Schml/compiler/closures/sanitize-binding-forms
         )

(provide make-closures-explicit)

(define-pass (make-closures-explicit ast config)
  (compose-compiler (ast config)
                    label-lambdas
                    ;;sanitize-binding-forms
                    ;;uncover-free
                    ;;convert-closure
                    ;;introduce-procedure-primitives
                    ))
