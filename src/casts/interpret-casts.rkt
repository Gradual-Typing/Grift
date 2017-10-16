#lang typed/racket/base

(require
 racket/match
 "../unique-identifiers.rkt"
 "../configuration.rkt"
 "../language/cast0.rkt"
 "../language/cast-or-coerce3.rkt"
 "interpret-casts-common.rkt"
 "interpret-casts-with-type-based-casts.rkt"
 "interpret-casts-with-coercions.rkt"
 "interpret-casts-with-hyper-coercions.rkt"
 "interpret-casts-with-error.rkt")

(provide
 interpret-casts
 (all-from-out
  "../language/cast0.rkt"
  "../language/cast-or-coerce3.rkt"))

(: interpret-casts : Cast0.5-Lang -> Cast-or-Coerce3-Lang)
(define (interpret-casts prgm)
  (match-define (Prog (list name next type) e) prgm)
  ;; All the implicit state of this program
  (define next-unique (make-unique-counter next))
  
  (parameterize ([cast-runtime-code-bindings '()]
                 [current-unique-counter next-unique]
                 [types-greatest-lower-bound-code-label? #f])

    (define ic-expr! : (C0-Expr -> CoC3-Expr)
      (case (cast-representation) 
        [(|Type-Based Casts|) (interpret-casts/type-based-casts)]
        [(Coercions) (interpret-casts/coercions)]
        [(Hyper-Coercions) (interpret-casts/hyper-coercions)]
        [(Static) (interpret-casts/error)]))
    
    (define new-e (ic-expr! e))

    (define rt-bindings (cast-runtime-code-bindings))

    ;; This is needed to convince the typechecker that parameter returned non-false
    (unless rt-bindings
      ;; This should never happen because in the dynamic extent of the above parameterize
      ;; the cast-runtime-code-bindings parameter should be of type CoC3-Code-Bnd*
      (error 'interpret-cast "very unexpected error"))
    
    ;; Reconstruct the entire program
    (Prog (list name (unique-counter-next! next-unique) type)
      (Labels rt-bindings new-e))))
