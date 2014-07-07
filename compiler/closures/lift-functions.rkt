#lang racket
#|------------------------------------------------------------------------------+
|Pass: compiler/closures/lift-functions                           |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
| Description: Lifts lambda forms to the top level of the program now that they are
| flattened.
+-------------------------------------------------------------------------------+
| Grammer:
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require Schml/framework/build-compiler
         Schml/framework/helpers
         Schml/framework/errors)

(require Schml/language/shared
         (prefix-in s: Schml/language/closure-il4)
         (prefix-in t: Schml/language/closure-il5))
;; Only the pass is provided by this module
(provide lift-functions)

(define-pass  (lift-functions prgm comp-config)
  ;; purposely backwards
  (define (lf-lambda lbl lam ty fn-bnd*)
    (match lam
      [(s:Lambda ty-lam fmls exp)
       (let-values ([(exp fn-bnd*) (lf-expr exp fn-bnd*)])
         (cons (Bnd:Ty lbl (t:Lambda ty-lam fmls exp) ty) fn-bnd*))]))
  
  (define (lf-expr* e* f*) 
    (if (null? e*)
        (values '() f*)
        (let*-values ([(e f*) (lf-expr (car e*) f*)]
                      [(e* f*) (lf-expr* (cdr e*) f*)])
          (values (cons e e*) f*))))
  (define (lf-expr exp fn-bnd*)
    (match exp
      [(s:Let-Proc ty (list (Bnd:Ty lbl* lam* ty*) ...) exp)
       (let-values ([(exp fn-bnd*) (lf-expr exp fn-bnd*)])
         (values exp (foldr lf-lambda fn-bnd* lbl* lam* ty*)))]
      [(s:Let ty (list (Bnd:Ty i* e* t*) ...) exp)
       (let*-values ([(exp fn-bnd*) (lf-expr exp fn-bnd*)]
                     [(e* fn-bnd*) (lf-expr* e* fn-bnd*)])
         (values (t:Let ty (map Bnd:Ty i* e* t*) exp) fn-bnd*))]
      [(s:Cast ty-cast exp ty-exp label)
       (let-values ([(exp fn-bnd*) (lf-expr exp fn-bnd*)])
         (values (t:Cast ty-cast exp ty-exp label) fn-bnd*))]
      [(s:If ty tst csq alt)
       (let*-values ([(tst fn-bnd*) (lf-expr tst fn-bnd*)]
                     [(csq fn-bnd*) (lf-expr csq fn-bnd*)]
                     [(alt fn-bnd*) (lf-expr alt fn-bnd*)])
         (values (t:If ty tst csq alt) fn-bnd*))]
      [(s:App ty exp exp*)
       (let*-values ([(exp fn-bnd*) (lf-expr exp fn-bnd*)]
                     [(exp* fn-bnd*) (lf-expr* exp* fn-bnd*)])
         (values (t:App ty exp exp*) fn-bnd*))]
      [(s:Begin ty exp* exp)
       (let*-values ([(exp fn-bnd*) (lf-expr exp fn-bnd*)]
                     [(exp* fn-bnd*) (lf-expr* exp* fn-bnd*)])
         (values (t:Begin ty exp* exp) fn-bnd*))]
      [(s:Prim ty pexp) (lf-prim ty pexp fn-bnd*)]
      [(s:Var t i) (values (t:Var t i) fn-bnd*)]
      [(s:Label t l) (values (t:Label t l) fn-bnd*)]
      [(s:Const t k) (values (t:Const t k) fn-bnd*)]
      [e (match-pass-error pass 'lf-expr e)]))
  (define (lf-prim ty pexp fn-bnd*)
    (match pexp
      [(Closure-field:Ref clos field) 
       (let-values ([(exp fn-bnd*) (lf-expr clos fn-bnd*)])
         (values (t:Prim ty (Closure-field:Ref exp field)) fn-bnd*))]
      [(Closure-field:Set! clos-exp field exp)
       (let*-values ([(clos-exp fn-bnd*) (lf-expr clos-exp fn-bnd*)]
                     [(exp fn-bnd*) (lf-expr exp fn-bnd*)])
         (values (t:Prim ty (Closure-field:Set! clos-exp field exp)) fn-bnd*))]
      [(Closure-field:Build fields)
       (values (t:Prim ty pexp) fn-bnd*)]
      [(Op:IntxInt fst snd)
       (let*-values ([(fst fn-bnd*) (lf-expr fst fn-bnd*)]
                     [(snd fn-bnd*) (lf-expr snd fn-bnd*)])
         (values (t:Prim ty (mk-op:int-int pexp fst snd)) fn-bnd*))]
      [(Rel:IntxInt fst snd)
       (let*-values ([(fst fn-bnd*) (lf-expr fst fn-bnd*)]
                     [(snd fn-bnd*) (lf-expr snd fn-bnd*)])
         (values (t:Prim ty (mk-rel:int-int pexp fst snd)) fn-bnd*))] 
      [otherwise (match-pass-error pass 'iic-prim otherwise)]))
  (match prgm
    [(s:Prog name unique type exp)
     (let-values ([(exp fns) (lf-expr exp '())])
       (t:Prog name unique type fns exp))]
    [otherwise (match-pass-error pass 'body prgm)]))
