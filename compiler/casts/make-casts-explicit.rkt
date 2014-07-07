#lang racket

(require Schml/framework/build-compiler
         Schml/compiler/casts/insert-implicit-casts
         ;;Schml/compiler/casts/expand-function-casts
         )

(provide make-casts-explicit)

(define-pass (make-casts-explicit ast config)
  (compose-compiler (ast config)
                    insert-implicit-casts
                    ;;expand-function-casts
                    ))
