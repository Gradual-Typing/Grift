#lang typed/racket/no-check
;; This is a redo of the basic compiler with interpreters between every micro compiler.
;; Once the file is finished compiling the program is run and the value checked against
;; an expected value. The real compiler is then invoked. And the result once again checked.

(require rackunit #;"./rackunit.rkt"
         racket/control
         racket/exn
         "../src/configuration.rkt"
         "../src/errors.rkt"
         "../src/helpers.rkt"
         (only-in "../src/compile.rkt" compile)
         "./values.rkt"
         "./paths.rkt")

;; (require/typed racket/control
;;   (abort (Any -> Nothing)))

;; (require/typed racket/exn
;;   (exn->string (exn -> String)))

(provide (all-defined-out)
         (all-from-out "./values.rkt"))

(define-syntax-rule (test-compile name path expected)
  (test-case name 
    (with-handlers ([exn:break? abort] 
                    [exn:test?
                     (lambda ([e : exn:test])
                       (raise e))]
                    [exn?
                     (lambda ([e : exn])
                       (unless (value=? (blame #t (exn-message e)) expected)
                         (display (exn->string e)))
                       (check value=?
                              (blame #t (exn-message e))
                              expected
                              "static type error")
                       (void))])
      (define f
        (compile path))
      (parameterize ([current-input-port (open-test-input path)])
         (check value=?
                (observe (system (path->string f)))
                expected
                "observed output")))))

(: open-test-input (Path -> Input-Port))
(define (open-test-input path)
  (let* ([root (string-trim (path->string path) ".grift")]
         [in   (string-append root ".in")])
    (if (file-exists? in)
        (open-input-file in)
        (current-input-port))))

(define-syntax-rule (test-file p ... n e)
  (test-compile n (simplify-path (build-path test-suite-path p ... n)) e))

(define-syntax-rule (test-file/no-checks p ... n e)
  (test-file p ... n e))

(define-syntax-rule (make-test-file p ...)
  (syntax-rules ()
    [(n e) (test-compile n (simplify-path (build-path test-suite-path p ... n)) e)]))


