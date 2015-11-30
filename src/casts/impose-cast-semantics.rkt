#lang typed/racket/base
#|
This is a micro compiler that removes the cast language form.
|#

(require "../helpers.rkt"
         "../configuration.rkt"
         "./purify-letrec.rkt"
         "./hoist-types.rkt"
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
     (begin
       (t a (format "pre ~a" 'p))
       (let ([a (if (t? a c) (p a c) a)])
         (compose-passes (a c t) p* ...)))]
    [(_ (a c t) p p* ...)
     (begin
       (t a (format "pre ~a" 'p))
       (let ([a (p a c)])
         (compose-passes (a c t) p* ...)))]))

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

(: coercion-representation? (Any Config -> Boolean))
(define (coercion-representation? _ c)
  (equal? 'Coercions (Config-cast-rep c)))

(define-compiler impose-cast-semantics : (Cast0-Lang Config -> Data0-Lang)
  purify-letrec
  (when coercion-representation?
    casts->coercions)
  lower-function-casts
  lower-reference-casts
  interpret-casts
  hoist-types
  label-lambdas
  uncover-free
  convert-closures
  specify-representation)

(: t (Crcn-Expr -> Cast-or-Coerce0-Lang))
(define (t p)
  (if (Lambda? p)
      (error 'todo)
      (Prog (list "" 0 (Int)) p)))

