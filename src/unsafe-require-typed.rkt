#lang typed/racket/base

(require (prefix-in tr: typed/racket/unsafe)
         racket/stxparam
         (for-syntax racket/base))

(provide unsafe-require/typed)


(define-for-syntax grift-debug? (or (getenv "GRIFT_DEBUG")))

(define-syntax (unsafe-require/typed stx)
  (syntax-case stx ()
    [(_ . rest)
     (with-syntax ([reqt (if (not grift-debug?)
                             #'tr:unsafe-require/typed
                             #'require/typed)])
       (syntax/loc stx (reqt . rest)))]))
