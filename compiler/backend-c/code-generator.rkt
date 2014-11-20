#lang typed/racket

(require schml/compiler/language
         schml/compiler/helpers
         schml/compiler/backend-c/generate-c)

(provide (all-defined-out))

;; Basic driver for the entire backend
(: c-backend-generate-code (Data2-Lang Config . -> . Boolean))
(define (c-backend-generate-code uil config)
  ;; Write the C code to a file
  (when (trace? 'Vomit)
    (logf "Wrote C Output to ~a" (path->string (Config-c-path config))))
  (with-output-to-file
      (Config-c-path config) #:mode 'text #:exists 'replace
      (lambda ()
        (generate-c uil config)))
  ;; Invoke the system cc compiler on the file
  (invoke-c-compiler config))

;; call the host system's cc function
(: invoke-c-compiler (-> Config Boolean))
(define (invoke-c-compiler config)
  (let* ([out (path->string (Config-exec-path config))]
         [in  (path->string (Config-c-path config))]
         [cmd (format "cc -o ~a ~a ~a" out in (warning-flags))])
    (when (trace? 'Vomit)
      (logf "System call: ~a" cmd))
    (parameterize
        ([current-error-port (if (trace? 'CC-Errors 'All 'Vomit)
                                 (current-log-port)
                                 (current-error-port))])
      (system cmd))))

;; C compiler warnings written with regard to clang
;; I need to check to make sure that these option will work with gcc
(define-type Opt-Warning (Parameter (Pair Boolean String)))

(define warn-format : Opt-Warning
  (make-parameter '(#f . "format")))

(define warn-int-conversion : Opt-Warning
  (make-parameter '(#f . "int-conversion")))

(: emit-opt-warning (-> Opt-Warning Void))
(define (emit-opt-warning warning)
  (let* ([option (warning)]
         [switch (car option)]
         [flag   (cdr option)])
    (display "-W")
    (unless switch
      (display "no-"))
    (display flag)
    (display " ")))

(define (warning-flags)
  (with-output-to-string
    (lambda ()
      (emit-opt-warning warn-format)
      (emit-opt-warning warn-int-conversion))))
    
  
