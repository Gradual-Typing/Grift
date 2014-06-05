#lang racket

(define-pass (make-closures-explicit ast config)
  (compose-compiler (ast config)
                    remove-anonymous-lambdas
                    ;;sanitize-binding-forms
                    ;;uncover-free
                    ;;convert-closure
                    ;;introduce-procedure-primitives
                    ))
