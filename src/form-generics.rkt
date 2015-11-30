#lang racket

(require racket/generic)

(provide form-map form->sexp language-form?)
(define-generics language-form
  #:fast-defaults
  ([null? (define (form-map f p) '())
          (define (form->sexp f) '())]
   [pair? (define/generic fmap  form-map)
          (define/generic fsexp form->sexp)
          (define (form-map f p)
            (cons (fmap (car f) p)
                  (fmap (cdr f) p)))
          (define (form->sexp f)
            (cons (fsexp (car f))
                  (fsexp (cdr f))))])
  ;; The generic interface form mapping over language forms
  [form-map language-form procs]
  [form->sexp language-form])
