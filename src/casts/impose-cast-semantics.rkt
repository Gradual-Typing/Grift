#lang typed/racket/base
#|
This is a micro compiler that removes the cast language form.
|#

(require "../helpers.rkt"
         "../configuration.rkt"
         "casts-to-coercions.rkt"
         "lower-function-casts.rkt"
         "lower-reference-casts.rkt"
         "interpret-casts.rkt"
         "label-lambdas.rkt"
         "uncover-free.rkt"
         "convert-closures.rkt"
         "specify-representation.rkt"
         "../language/cast0.rkt"
         "../language/data0.rkt")

(provide (all-defined-out)
         (all-from-out
          "../language/cast0.rkt"
          "../language/data0.rkt"))


(require (for-syntax racket/base racket/syntax))

(define-syntax-rule (nop . a) (void))

(define-syntax compose-passes
  (syntax-rules (when get-ast begin)
    [(_ (a c t)) a]
    [(_ (a c t) (get-ast v) p* ...)
     (let ([v a])
       (compose-passes (a c t) p* ...))]
    [(_ (a c t) (begin e* ...) p* ...)
     (begin e* ... (compose-passes (a c t) p* ...))]
    [(_ (a c t) (when t? p) p* ...)
     (let ([ast (if (t? a c) (p a c) a)])
       (t ast (format "post ~a" 'p))
       (compose-passes (ast c t) p* ...))]
    [(_ (a c t) p p* ...)
     (let ([a (p a c)])
       (t a (format "post ~a" 'p))
       (compose-passes (a c t) p* ...))]))

(define-syntax (define-compiler stx)
  (syntax-case stx (:)
    [(_ name : t p* ...)
     (with-syntax ([test-name (format-id #'name "test-~a" #'name)])
       #'(begin
           (: name t)
           (define (name ast config)
             (compose-passes (ast config nop) p* ...))
           (define-syntax-rule (test-name ast config test-macro)
             (let ([ast-value    ast]
                   [config-value config])
               (compose-passes (ast-value config-value test-macro) p* ...)))))]))

#;(: using-coercions? (Config -> Bool))
#;
(define (using-coercions? c)
  (or (eq? (Config-function-representation c)
           'Space-Efficient-Hybrid-Functions)))

#;(: impose-cast-semantics (Cast0-Lang Config -> Data0-Lang))
#;
(define (impose-cast-semantics prgm config)
  (let* 
      (
 ;;   [prgm                             ;
       ;;   (if (using-coercions? config) (casts->coercions prgm config) prgm)]
       [c1 (lower-function-casts prgm config)]
       [c2 (lower-reference-casts c1 config)]
       [c3 (interpret-casts c2 config)]
       [l1 (label-lambdas c3 config)]
       [l2 (uncover-free l1 config)]
       [l3 (convert-closures l2 config)]
       )
    (specify-representation l3 config)))

(: coercion-semantics? (Any Config -> Boolean))
(define (coercion-semantics? _ c)
  (equal? 'Coercions (Config-cast-rep c)))

(define-compiler impose-cast-semantics : (Cast0-Lang Config -> Any #;Cast-or-Coerce1-Lang
                                                     )
  (when coercion-semantics?
    casts->coercions)
    lower-function-casts
  )


