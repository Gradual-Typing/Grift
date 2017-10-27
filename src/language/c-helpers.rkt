#lang racket
(require racket/string "../configuration.rkt")
(provide (all-defined-out))

(define ((define-line in) s v)
  (define n? (syntax->datum v))
  (define str (string-replace (symbol->string (syntax->datum s)) #rx"-|/" "_"))
  (displayln
   (string-append
    "#define "
    str
    " "
    (if (number? n?)
        (number->string n?)
        (string-append "printf(\"I do not know what is " str "\\n\");exit(-1)")))
   in))

(define (append-to-constants.h exists f)
  (call-with-output-file
    (constants-path)
    #:mode 'text
    #:exists exists
    (lambda (in) (f in))))
