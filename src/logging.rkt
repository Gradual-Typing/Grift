#lang typed/racket/base

(require racket/logging
         racket/port 
         syntax/srcloc
         syntax/location
         (for-syntax racket/base)
         "helpers.rkt")

(provide (all-defined-out))

(define-logger schml)

(define-syntax logging
  (syntax-rules ()
    [(logging n () f a ...) (logging n (All) f a ...)]
    [(logging n (o0 o* ...) f a ...)
     (let ([t? (trace? 'n 'o0 'o* ...)])
       (when t?
         (log-schml-debug "~a: ~a\n\n" 'n f)
         (log-schml-debug (with-output-to-string (lambda () (display a) ...)))))]))

(define-syntax (debug stx)
  (syntax-case stx ()
    [(_ e ...)
     #`(log-schml-debug
        (with-output-to-string
          (lambda ()
            (printf "~a:\n" (srcloc->string (quote-srcloc #,stx)))
            (printf "\t~a=~a\n" 'e e)
            ...
            (newline))))]
    [other (error 'debug "invalid syntax")]))


