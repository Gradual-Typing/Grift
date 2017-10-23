#lang racket/base
;; This is a redo of the basic compiler with interpreters between every micro compiler.
;; Once the file is finished compiling the program is run and the value checked against
;; an expected value. The real compiler is then invoked. And the result once again checked.

(require rackunit
         racket/exn
         racket/port
         racket/file
         racket/system
         racket/control
         "../src/errors.rkt")

(provide (all-defined-out))

(define-check (check-io run-thunk input output error)
  (define outp (open-output-string 'current-output-string))
  (define errp (open-output-string 'current-error-string))
  (define old-cop (current-output-port))
  (define old-cep (current-error-port))
  (define (print-exn e)
    (display (exn->string e) (current-error-port)))
  (define returns
    (parameterize ([current-input-port (open-input-string input)]
                   [current-output-port outp]
                   [current-error-port errp])
      (with-handlers ([exn:break? abort]
                      [exn:fail? print-exn]
                      [exn:grift? print-exn]) 
        (parameterize-break #t (run-thunk)))))
  (define out (get-output-string outp))
  (define err (get-output-string errp))
  (unless (and (regexp-match? output out)
               (regexp-match? error  err))
    (fail-check
     (string-append
      (format "umatched io:\n")
      (format "stdio:\n")
      (format "\texpected=\n~a\n" output)
      (format "\trecieved=\n~a\n" out)
      (format "stderr:\n")
      (format "\texpected=\n~a\n" error)
      (format "\trecieved=\n~a\n" err))))
  returns)

(define (maybe-file->regexp-match? path default)
  (if (file-exists? path)
      (let ([regex (pregexp (file->string path))])
        (lambda (err) (regexp-match? regex err)))
      default))

(define (test-compile-file test-src th)
  (define (file?->string p d)
    (cond
      [(file-exists? p) (file->string p)]
      [else d]))
  (define input  (file?->string (path-replace-suffix test-src #".in") ""))
  (define out-rx (file?->string (path-replace-suffix test-src #".out.rx" "")))
  (define err-rx (file?->string (path-replace-suffix test-src #".err.rx" "^$")))

  (check-io th input out-rx err-rx))

