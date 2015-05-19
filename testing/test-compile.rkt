#lang typed/racket
;; This is a redo of the basic compiler with interpreters between every micro compiler.
;; Once the file is finished compiling the program is run and the value checked against
;; an expected value. The real compiler is then invoked. And the result once again checked.

(require typed/rackunit
         schml/src/language
         schml/src/errors
         schml/src/compile
         schml/testing/values
         schml/testing/paths
         schml/src/helpers)

(require ;; The passes
         schml/src/schml/reduce-to-cast-calculus
         schml/src/casts/impose-cast-semantics
         schml/src/data/convert-representation
         schml/src/backend-c/code-generator
         ;; The interpreters
         schml/testing/ast-interps/cast-lang-interp)

(provide (all-defined-out)
         (all-from-out schml/testing/values)
         Config)

(define compiler-config : (Parameter Config)
  (make-parameter
   (Config 'Lazy-D
           (build-path test-tmp-path "t.out")
           (build-path test-tmp-path "t.c")
           '())))

(define-syntax-rule (test-compile name path expected)
  (test-case name
   (let ([config (compiler-config)])
     (with-handlers ([exn:schml:type:static?
                      (lambda ([e : exn:schml:type:static])
                        (begin
                          (check value=?
                                 expected
                                 (blame #t (exn-message e))
                                 "static type error")))])
       (let* ([c0  : Cast0-Lang (reduce-to-cast-calculus path config)]
              [_   (check value=?
                          expected
                          (cast-lang-interp c0 config)
                          "cast lang semantics")]
              [d0  : Data0-Lang (impose-cast-semantics c0 config)]
              [u0  : Data2-Lang (convert-representation d0 config)]
              [_   : Boolean    (c-backend-generate-code u0 config)])
         (check value=?
                expected
                (observe (envoke-compiled-program #:config config))
                "test compiler semantics")
         (compile/conf path config)
         (check value=?
                expected
                (observe (envoke-compiled-program #:config config))
                "compiler semantics"))))))

(define-syntax-rule (test-file p ... n e)
  (test-compile n (simplify-path (build-path test-suite-path p ... n)) e))
