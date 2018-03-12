#lang typed/racket/no-check

(require
 racket/string
 "../configuration.rkt"
 "../backend-c/runtime-location.rkt")

(provide (all-defined-out))

(: define-line (Output-Port -> ((Syntaxof Symbol) Syntax -> Void)))
(define ((define-line in) s v)
  (define n? (syntax->datum v))
  (define str (string-replace (symbol->string (syntax-e s)) #px"[^\\w]" "_"))
  (displayln
   (string-append
    "#define "
    str
    " "
    (if (number? n?)
        (number->string n?)
        (string-append "printf(\"I do not know what is " str "\\n\");exit(-1)")))
   in))

(: replace/append-to-constants.h ((U 'replace 'append) (Output-Port -> Void) -> Void))
(define (replace/append-to-constants.h exists f)
  (call-with-output-file
    constants.h-path
    #:mode 'text
    #:exists exists
    (lambda (in) (f in))))
