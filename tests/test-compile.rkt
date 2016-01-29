#lang typed/racket
;; This is a redo of the basic compiler with interpreters between every micro compiler.
;; Once the file is finished compiling the program is run and the value checked against
;; an expected value. The real compiler is then invoked. And the result once again checked.

(require "./rackunit.rkt"
         "../src/configuration.rkt"
         ;;"../src/language.rkt"
         "../src/errors.rkt"
         "../src/helpers.rkt"
         
         (only-in "../src/compile.rkt"
         compile/conf envoke-compiled-program)
         "./values.rkt"
         "./paths.rkt")

(require ;; The passes
 "../src/schml/reduce-to-cast-calculus.rkt"
 "../src/casts/impose-cast-semantics.rkt"
 ;;"../src/casts/casts-to-coercions.rkt"
 "../src/data/convert-representation.rkt"
 "../src/backend-c/code-generator.rkt"
 ;; The interpreters
 "./ast-interps/cast-lang-interp.rkt"
 )

;(require/typed "./ast-interps/new-form-interp.rkt"
;  [interp (Any Config -> Test-Value)])

(provide (all-defined-out)
         (all-from-out "./values.rkt")
         Config)

(: compiler-config-cast-representation (-> Cast-Representation Void))
(define (compiler-config-cast-representation r)
  (define c (compiler-config))
  (compiler-config
   (Config (Config-source-path c)
           (Config-semantics c)
           (Config-exec-path c)
           (Config-c-path c)
           (Config-keep-c c)
           (Config-c-flags c)
           (Config-asm-path c)
           r
           (Config-mem-limit c))))

(define intermediate-checks?
  : (Parameterof Boolean)
  (make-parameter #f))


(define compiler-config : (Parameterof Config) 
  (make-parameter
   (Config (build-path "test")
           'Lazy-D
           (build-path test-tmp-path "t.out")
           (build-path test-tmp-path "t.c")
           #f
          '()
          #f
          'Twosomes
          1000000)))


(define-syntax-rule (test-compile name path expected)
  (test-case name
    (call/cc
     (lambda ([exit : (Any -> Any)])
       (define (exit/success) (exit 'success))
       (define config (compiler-config))
       ;(define-syntax-rule (ck t m) (check value=? (interp t config) expected m))
       (define-syntax-rule (nop . a) (void))
       (with-handlers ([exn:schml:type:static?
                          (lambda ([e : exn:schml:type:static])
                            (begin
                              (check value=?
                                     expected
                                     (blame #t (exn-message e))
                                     "static type error")))])
         (when (intermediate-checks?)
           (define ast0 (reduce-to-cast-calculus path config))
           (check value=? (cast-lang-interp ast0 config) expected "cast-lang interp")
           (nop ast0 "cast 0")            
           (define ast1 (test-impose-cast-semantics ast0 config nop))
           (define ast2 (convert-representation ast1 config))
           (c-backend-generate-code ast2 config)
           (check value=?
                  (observe (envoke-compiled-program #:config config))
                  expected
                  "test compiler semantics"))
         (compile/conf path config)
         (check value=?
                (observe (envoke-compiled-program #:config config))
                expected
                "real compiler semantics"))))))

(define-syntax-rule (test-file p ... n e)
  (test-compile n (simplify-path (build-path test-suite-path p ... n)) e))

(define-syntax-rule (test-file/no-checks p ... n e)
  (parameterize ([intermediate-checks? #f])
    (test-compile n (simplify-path (build-path test-suite-path p ... n)) e)))

(define-syntax-rule (make-test-file p ...)
  (syntax-rules ()
    [(n e) (test-compile n (simplify-path (build-path test-suite-path p ... n)) e)]))
