#lang racket/base

(provide (all-defined-out))

(require racket/port)
;; Return a string for anything printed to current-output-port
;; in expression p
(define-syntax-rule (out->string p)
  (with-output-to-string (lambda () p)))

;; Allows breaking up multi-line strings
(require (for-syntax racket/port))
(require (for-syntax racket/base))
(define-syntax (concat-string-literal stx)
  (syntax-case stx ()
    [(_ s* ...)
     (datum->syntax stx
      (with-output-to-string
        (lambda () (for-each display (syntax->datum #'(s* ...))))))]))


(define (id-append id #:ctx [ctx #f] . rest)
  (define (help ctx id rest)
    (if (null? rest)
        (datum->syntax ctx (string->symbol id) ctx ctx)
        (help ctx (string-append id (id->str (car rest))) (cdr rest))))
  (define (check-ctx id ctx)
    (cond
      [ctx
       (unless (syntax? ctx)
         (error 'id-append "the ctx expression must be syntax"))
         ctx]
        [(identifier? id) id]
        [else (error 'id-append "no available context")]))
    (define (id->str id)
      (cond
        [(identifier? id) (symbol->string (syntax->datum id))]
        [(symbol? id) (symbol->string)]
        [(string? id) id]
        [else (error 'id->str "with irritant ~a" id)]))
    (help (check-ctx id ctx) (id->str id) rest))
