#lang typed/racket
;; This is a redo of the basic compiler with interpreters between every micro compiler.
;; Once the file is finished compiling the program is run and the value checked against
;; an expected value. The real compiler is then invoked. And the result once again checked.

(require "./rackunit.rkt"
         "../src/configuration.rkt"
         ;;"../src/language.rkt"
         "../src/errors.rkt"
         "../src/helpers.rkt"
         #;
         (only-in "../src/compile.rkt"
         compile/conf envoke-compiled-program)
         "./values.rkt"
         "./paths.rkt")

(require ;; The passes
 "../src/schml/reduce-to-cast-calculus.rkt"
 "../src/casts/impose-cast-semantics.rkt"
 ;;"../src/casts/casts-to-coercions.rkt"
 ;;"../src/data/convert-representation.rkt"
 ;;"../src/backend-c/code-generator.rkt"
 ;; The interpreters
 "./ast-interps/cast-lang-interp.rkt"
 )

(require/typed "./ast-interps/new-form-interp.rkt"
  [interp (Any Config -> Test-Value)])

(provide (all-defined-out)
         (all-from-out "./values.rkt")
         Config)

(define (compiler-config) : Config
  (Config (build-path "test")
          'Lazy-D
          (build-path test-tmp-path "t.out")
          (build-path test-tmp-path "t.c")
          #f
          '()
          #f
          (compiler-config-cast-representation)))

(define compiler-config-cast-representation
  : (Parameterof Cast-Representation)
  (make-parameter 'Twosomes))

(define-syntax-rule (test-compile name path expected)
  (test-case name
    (call/cc
     (lambda ([exit : (Any -> Any)])
       (define (exit/success) (exit 'success))
       (define config (compiler-config))
       (define-syntax-rule (ck t m) (check value=? (interp t config) expected m))
       (define-syntax-rule (nop . a) (void))
       (with-handlers ([exn:schml:type:static?
                          (lambda ([e : exn:schml:type:static])
                            (begin
                              (check value=?
                                     expected
                                     (blame #t (exn-message e))
                                     "static type error")))])
           (define ast0 (reduce-to-cast-calculus path config))
           (check value=? (cast-lang-interp ast0 config) expected "cast-lang interp")
           (ck ast0 "cast 0")
           (define ast1 (test-impose-cast-semantics ast0 config ck))
           ast1

           
           
           #;
           (when (eq? 'Coercions (Config-cast-rep config))
           (define crcn (casts->coercions c0 config))
           (ck (interp crcn config) "coercion lang semantics")
           (exit/success))
         #|(define d0 (impose-cast-semantics c0 config))
         (define u0 (convert-representation d0 config))
         (define p  (c-backend-generate-code u0 config))
         (check value=?
                expected
                (observe (envoke-compiled-program #:config config))
                "test compiler semantics")
         (compile/conf path config)
         (check value=?
                expected
                (observe (envoke-compiled-program #:config config))
                "compiler semantics")|#)))))

(define-syntax-rule (test-file p ... n e)
  (test-compile n (simplify-path (build-path test-suite-path p ... n)) e))

(define-syntax-rule (make-test-file p ...)
  (syntax-rules ()
    [(n e) (test-compile n (simplify-path (build-path test-suite-path p ... n)) e)]))
