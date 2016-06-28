#lang racket

(require racket/generic)

(provide form-map form-map-forward form-map-backward
         form->sexp language-form?)

(define-generics language-form
  #:fast-defaults
  ([null? (define (form-map f p) '())
          (define (form-map-forward f p) '())
          (define (form-map-backward f p) '())
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
  [form-map-forward language-form procs]
  [form-map-backward language-form procs]
  [form->sexp language-form])

(define (sexp->form x)
  (match x
    [(list (? symbol? ctr) xs ...)
     ((hash-ref ctr (thunk (error 'sexp->form "unregistered ~a" ctr))) xs)]
    [(list) '()]))
