#lang racket
#|------------------------------------------------------------------------------+
|Pass: compiler/closures/remove-anonymous-lambdas                                    |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
| Description: 
+-------------------------------------------------------------------------------+
| Grammer:
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require Schml/framework/build-compiler
         Schml/framework/helpers
         Schml/framework/errors)
;; The constuctors of the core language
(require Schml/language/shared
         Schml/language/lambdaless-core)
;; Only the pass is provided by this module
(provide remove-anonymous-lambdas)

(define-pass (sanatize-binding-forms prgm comp-config)
  (define (sbf-expr exp)
    ;; separate procedure and other bindings
    (define (sbf-let t b* e)
      (match exp
        [(list (or (and (Bnd:Ty _ (? Lambda?) _) lp*) l*) ...)
         (cond
           [(null? l*) (Let-Proc t lp* e)]
           [(null? lp*) (Let t l* e)]
           [else (Let-Proc t lp* (Let t l* e))])]
        [o (sbf-expr exp)]))
    (match exp
      [(Let ty (list b* ...) (app sbf-expr e)) (sbf-let ty b* e)]
      [(Lambda ty fmls (app sbf-expr exp)) (Lambda ty fmls exp)]
      [(Cast ty-cast (app sbf-expr exp) ty-exp label)
       (Cast ty-cast exp ty-exp label)]
      [(If ty (app sbf-expr tst) (app sbf-expr csq) (app sbf-expr alt))
       (If ty tst csq alt)]
      [(App ty (app sbf-expr exp) (list (app sbf-expr exp*) ...))
       (App ty exp exp*)]
      [(Prim ty pexp) (sbf-prim ty pexp)]
      [(Var _ _) exp] [(Const _ _) exp]
      [e (match-pass-error pass 'tyck-expr e)]))

  (define (sbf-prim ty pexp)
    (match pexp
      [(Op:IntxInt (app sbf-expr fst) (app sbf-expr snd))
       (Prim ty (mk-op:int-int pexp fst snd))]
      [(Rel:IntxInt (app sbf-expr fst) (app sbf-expr snd))
       (Prim ty (mk-rel:int-int pexp fst snd))] 
      [otherwise (match-pass-error pass 'iic-prim otherwise)]))
  
  (match prgm
    [(Prog n e) (Prog n (sbf-expr e))]
    [otherwise (match-pass-error pass 'body prgm)]))



