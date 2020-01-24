#lang typed/racket/no-check

(require
 racket/string
 "../configuration.rkt"
 "../backend/runtime-location.rkt")

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

(: replace/append-to-cheader (Path-String (U 'replace 'append) (Output-Port -> Void) -> Void))
(define (replace/append-to-cheader path exists f)
  (call-with-output-file
    path
    #:mode 'text
    #:exists exists
    (lambda (in) (f in))))

(define fresh-constants.h? (box #t))

(: replace/append-to-constants.h ((Output-Port -> Void) -> Void))
(define (replace/append-to-constants.h f)
  (if (unbox fresh-constants.h?)
      (begin
        (replace/append-to-cheader constants.h-path 'replace f)
        (set-box! fresh-constants.h? #f))
      (replace/append-to-cheader constants.h-path 'append f)))

(: create-cast-profiler.h ((Output-Port -> Void) -> Void))
(define (create-cast-profiler.h f)
  (replace/append-to-cheader cast-profiler.h-path 'replace f))

(: create-cast-profiler.c ((Output-Port -> Void) -> Void))
(define (create-cast-profiler.c f)
  (replace/append-to-cheader cast-profiler.c-path 'replace f))
