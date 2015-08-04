#lang racket
#|
The true draw back of using typed racket is that macros typed
macros will not work in untyped code. This module duplicates
any untyped macros so that they will work in untyped code.
I have not yet checked to see if no-check is sufficient to
get around this problem.
|#

(provide (all-defined-out))

;; Return a string for anything printed to current-output-port
;; in expression p
(define-syntax-rule (out->string p)
  (with-output-to-string (lambda () p)))

;; Allows breaking up multi-line strings
(require (for-syntax racket/port))
(define-syntax (concat-string-literal stx)
  (syntax-case stx ()
    [(_ s* ...)
     (datum->syntax stx
      (with-output-to-string
        (lambda ()
          (for ([s (in-list (syntax->datum #'(s* ...)))])
            (display s)))))]))
