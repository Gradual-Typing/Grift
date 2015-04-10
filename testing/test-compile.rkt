#lang typed/racket
;; This is a redo of the basic compiler with interpreters between every micro compiler.
;; Once the file is finished compiling the program is run and the value checked against
;; an expected value. The real compiler is then invoked. And the result once again checked.

(require typed/rackunit 
         schml/src/language
         schml/src/errors
         schml/src/compile
         schml/testing/values)

(provide (all-defined-out)
         Config)

(define compiler-config : (Parameter Config)
  (make-parameter
   (Config 'Lazy-D (build-path "t.out") (build-path "t.c"))))

(define-syntax-rule (ann-ck msg ? v e)
  (check ? v e msg))
 
(: test-compile (-> Path Test-Value Boolean))
(define (test-compile path expected)
  ;; The micro compilers
  (local-require schml/src/schml/reduce-to-cast-calculus
		 schml/src/casts/impose-cast-semantics
		 schml/src/data/convert-representation
                 schml/src/backend-c/code-generator)
  ;; The intermediary interpreters
  (local-require schml/testing/ast-interps/cast-lang-interp
		 ;;schml/testing/ast-interps/lambda-lang-interp
		 ;;schml/testing/ast-interps/data-lang-interp
                 )
  (let ((config (compiler-config)))
    (with-handlers ([exn:schml:type:static? 
		     (lambda ([e : exn:schml:type:static]) 
		       (begin
                         (ann-ck "static type error"
                                 value=? (blame #t (exn-message e)) expected)
                         #t))])
      (let* ([c0  : Cast0-Lang (reduce-to-cast-calculus path config)]
             [_   (ann-ck "cast lang semantics"
                          value=? (cast-lang-interp c0 config) expected)]
	     [d0  : Data0-Lang (impose-cast-semantics c0 config)]
          ;; [_ (ann-ck "lambda lang semantics" value=? (lambda-lang-interp l0 config) expected)]
	  ;; [d0  : Data0-Lang (make-closures-explicit l0 config)]
          ;; [_   (ann-ck "data lang semantics"
          ;;                value=? (data-lang-interp d0 config) expected)]
             [u0  : Data2-Lang (convert-representation d0 config)]
             [_   : Boolean    (c-backend-generate-code u0 config)])
        (ann-ck "test compiler semantics"
                value=?  (observe (envoke-compiled-program #:config config)) expected)
        (compile/conf path config)
        (ann-ck "compiler semantics"
                value=?  (observe (envoke-compiled-program #:config config)) expected)
        #t))))
 
