#lang typed/racket/base/no-check

(require
 racket/match
 "../configuration.rkt"
 "../language/forms.rkt"
 "../unique-identifiers.rkt"
 "./interpret-casts-common.rkt"
 "./interpret-casts-with-type-based-casts.rkt"
 "./interpret-casts-with-coercions.rkt"
 "./interpret-casts-with-hyper-coercions.rkt"
 "./interpret-casts-with-error.rkt"
 "./coercion-passing.rkt")

(provide
 interpret-casts)

(: interpret-casts : Cast0.5-Lang -> Cast-or-Coerce3-Lang)
(define (interpret-casts prgm)
  (match-define (Prog (list name next type) e) prgm)
  ;; All the implicit state of this program
  (define next-unique (make-unique-counter next))
  
  (parameterize ([cast-runtime-code-bindings '()]
                 [cast-runtime-constant-bindings '()]
                 [current-unique-counter next-unique]
                 [types-greatest-lower-bound-code-label? #f]
                 [mref-state-reduction-uid? #f]
                 [mvect-state-reduction-uid? #f])

    (define ic-expr! : (C0-Expr -> CoC3-Expr)
      (case (cast-representation)
        [(|Type-Based Casts|) (interpret-casts/type-based-casts)]
        [(Coercions)
         (cond
          [(enable-crcps?)
           (define-values (icc apply-coercion-uid compose-coercions-uid)
             (interpret-casts/coercions))
           (compose1 (coercion-passing-trans #:apply-coercion-uid apply-coercion-uid
                                             #:compose-coercions-uid compose-coercions-uid)
                     icc)]
          [else
           (define-values (icc apply-coercion-uid compose-coercions-uid)
             (interpret-casts/coercions))
           icc])]
        [(Hyper-Coercions) (interpret-casts/hyper-coercions)]
        [(Static) (interpret-casts/error)]
        [else (error 'grift/interpret-casts
                     "unexpected cast representation: ~a"
                     (cast-representation))]))

    (define new-e
      (cond [(enable-crcps?)
             ;; avoid removing id coercions around injection/projection:
             (parameterize ([optimize-first-order-coercions? #f])
               (ic-expr! e))]
             [else (ic-expr! e)]))
    
    (define rt-bindings  (cast-runtime-code-bindings))
    (define rt-constants : (Option CoC3-Bnd*)
      (cast-runtime-constant-bindings))
    
    ;; This is needed to convince the typechecker that parameter
    ;; returned non-false
    (unless (and rt-bindings rt-constants)
      ;; This should never happen because in the dynamic extent of the
      ;; above parameterize the cast-runtime-code-bindings parameter
      ;; should be of type CoC3-Code-Bnd*
      (error 'interpret-cast "very unexpected error"))
    
    ;; Reconstruct the entire program
    (Prog (list name (unique-counter-next! next-unique) type)
      (Static*
       (list rt-constants)
       (Labels rt-bindings new-e)))))
