#lang typed/racket

(require "../language.rkt"
         "../helpers.rkt"
         "./generate-c.rkt")

(provide (all-defined-out))

;; Basic driver for the entire backend
(: c-backend-generate-code (Data5-Lang Config . -> . Path))
(define (c-backend-generate-code uil config)
  (define c-path (Config-c-path config))
  ;; Write the C code to a file
  (logging c-backend-generate-code (Vomit) "~v" c-path)
  (with-output-to-file c-path #:mode 'text #:exists 'replace
    (lambda ()
      (generate-c uil config)))
  ;; if clang-format is present and we keep c then clean up
  ;; the file
  (parameterize ([current-error-port (open-output-string)]
                 [current-output-port (open-output-string)])
         ;; TODO I think that I need to close this port
    (cond
      [(system "which clang-format")
       ;; There is an error message that doesn't mean anything
       (system (format "clang-format -i ~a" (path->string c-path)))]))

  ;; Invoke the system cc compiler on the file
  (invoke-c-compiler config))

;; call the host system's cc function
(: invoke-c-compiler (-> Config Path))
(define (invoke-c-compiler config)
  (let* ([out-path (Config-exec-path config)]
         [out (path->string out-path)]
         [in  (path->string (Config-c-path config))]
         [flags (append-flags (Config-c-flags config))]
         [cmd (format "cc -o ~a ~a ~a" out in flags)])
    (when (trace? 'Vomit)
      (logf "System call: ~a" cmd))
    (flush-output (current-log-port))
    (flush-output (current-error-port))
    (flush-output)
    (parameterize
        ([current-error-port (if (trace? 'CC-Errors 'All 'Vomit)
                                 (current-log-port)
                                 (current-error-port))])
      (let ([asm? (Config-asm-path config)])
        (when asm?
          (system (format "cc ~a -S -o ~a ~a" in (path->string asm?) flags))))
      (unless (system cmd)
        (TODO raise appropriate error here))
      out-path)))

(: append-flags : (Listof String) -> String)
(define (append-flags s)
  (if (null? s)
      (default-flags)
      (with-output-to-string
        (lambda ()
          (let loop ([s s])
            (unless (null? s)
              (display (car s))
              (display " ")
              (loop (cdr s))))))))

;; C compiler warnings written with regard to clang
;; I need to check to make sure that these option will work with gcc
(define-type Opt-Warning (Parameter (Pair Boolean String)))

(define warn-format : Opt-Warning
  (make-parameter '(#f . "format")))

(define warn-int-conversion : Opt-Warning
  (make-parameter '(#f . "int-conversion")))

(define warn-unused-value : Opt-Warning
  (make-parameter '(#f . "unused-value")))




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

(define (emit-optimize-c)
  (display " -O3 "))

(define (default-flags)
  (with-output-to-string
    (lambda ()
      (emit-optimize-c)
      (emit-opt-warning warn-format)
      (emit-opt-warning warn-int-conversion)
      (emit-opt-warning warn-unused-value))))
