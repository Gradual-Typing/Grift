#lang racket/base

(require (for-syntax racket/base)
         racket/logging
         racket/port 
         syntax/srcloc
         syntax/location
         "helpers.rkt")
(provide (all-defined-out))

(define-logger grift)

(define-syntax (current-srcloc-as-string stx)
  (syntax-case stx ()
    [(_) #`(srcloc->string (quote-srcloc #,stx))]))

(define-syntax logging
  (syntax-rules ()
    [(logging n () f a ...) (logging n (All) f a ...)]
    [(logging n (o0 o* ...) f a ...)
     (let ([t? (trace? 'n 'o0 'o* ...)])
       (when t?
         (log-grift-debug "~a: ~a\n\n" 'n f)
         (log-grift-debug (with-output-to-string (lambda () (display a) ...)))))]))

(define-syntax (debug stx)
  (syntax-case stx (off std)
    [(debug off e* ... e)
     #'(begin e* ... e)]
    [(debug std e* ... e)
     (with-syntax ([(t* ... t) (generate-temporaries #'(e* ... e))])
       #`(let ([t* e*] ... [t e])
           (printf "~a:\n" (srcloc->string (quote-srcloc #,stx)))
           (printf "  ~a=~v\n" 'e* t*)
           ...
           (printf "  ~a=~v\n" 'e t)
           (newline)
           t))]
    [(_ e* ... e)
     (with-syntax ([(t* ... t) (generate-temporaries #'(e* ... e))])
       #`(let ([t* e*] ... [t e])
           (begin
             (log-grift-debug
              (with-output-to-string
                (lambda ()
                  (printf "~a:\n" (srcloc->string (quote-srcloc #,stx)))
                  (printf "  ~a=~v\n" 'e* t*)
                  ...
                  (printf "  ~a=~v\n" 'e t)
                  (newline))))
             t)))]
    [other (error 'debug "invalid syntax")]))

;; Code copying is dumb but I couldn't figure out how to
;; use the macros in both typed and untyped.
;; Should I look at rewriters?
(module typed typed/racket/base
  (require (for-syntax racket/base)
           racket/logging
           racket/port
           syntax/location)
  (require/typed racket/base
    [srcloc->string (srcloc -> (Option String))])
  (provide (all-defined-out))
  (define-logger grift)
  (define-syntax logging
    (syntax-rules ()
      [(logging n () f a ...) (logging n (All) f a ...)]
      [(logging n (o0 o* ...) f a ...)
       (let ([t? (trace? 'n 'o0 'o* ...)])
         (when t?
           (log-grift-debug "~a: ~a\n\n" 'n f)
           (log-grift-debug
            (with-output-to-string (lambda () (display a) ...)))))]))

  (define-syntax (debug stx)
    (syntax-case stx (off std)
      [(debug off e* ... e)
       #'(begin e* ... e)]
      [(debug std e* ... e)
       (with-syntax ([(t* ... t) (generate-temporaries #'(e* ... e))])
         #`(let ([t* e*] ... [t e])
             (printf "~a:\n" (srcloc->string (quote-srcloc #,stx)))
             (printf "  ~a=~v\n" 'e* t*)
             ...
             (printf "  ~a=~v\n" 'e t)
             (newline)
             t))]
      [(_ e* ... e)
       (with-syntax ([(t* ... t) (generate-temporaries #'(e* ... e))])
         #`(let ([t* e*] ... [t e])
             (begin
               (log-grift-debug
                (with-output-to-string
                  (lambda ()
                    (printf "~a:\n" (srcloc->string (quote-srcloc #,stx)))
                    (printf "  ~a=~v\n" 'e* t*)
                    ...
                    (printf "  ~a=~v\n" 'e t)
                    (newline))))
               t)))]
      [other (error 'debug "invalid syntax")])))





