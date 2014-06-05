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
         (prefix-in sc Schml/language/sourceless-core)
         (prefix-in il Schml/language/closures-il1))
;; Only the pass is provided by this module
(provide remove-anonymous-lambdas)

(define-pass (remove-anonymous-lambdas prgm comp-config)
  (define (ral-expr exp)
    ;; Short circut on lambdas that are being bound
    (define (ral-rhs exp)
      (match exp
        [(Lambda ty fmls (app ral-expr exp)) (Lambda ty fmls exp)]
        [o (ral-expr exp)]))
    (match exp
      ;; This is only a non interesting transformation but it gives a
      ;; name to all lambdas
      [(Lambda ty fmls (app ral-expr exp))
       (let ((tmp-name (uvar 'annon)))
         (Let ty `(,(Bnd:Ty tmp-name (Lambda ty fmls exp) ty)) (Var ty tmp-name)))]
      ;; the use of ral-rhs will short circut the naming of a lambda that
      ;; is already named.
      [(Let ty (list (Bnd:Ty i* (app ral-rhs e*) t*) ...) (app ral-expr body))
       (Let ty (map Bnd:Ty i* e* t*) body)]
      [(Cast ty-cast (app ral-expr exp) ty-exp label)
       (Cast ty-cast exp ty-exp label)]
      [(If ty (app ral-expr tst) (app ral-expr csq) (app ral-expr alt))
       (If ty tst csq alt)]
      [(App ty (app ral-expr exp) (list (app ral-expr exp*) ...))
       (App ty exp exp*)]
      [(Prim ty pexp) (ral-prim ty pexp)]
      [(Var _ _) exp] [(Const _ _) exp]
      [e (match-pass-error pass 'tyck-expr e)]))

  (define (ral-prim ty pexp)
    (match pexp
      [(Op:IntxInt (app ral-expr fst) (app ral-expr snd))
       (Prim ty (mk-op:int-int pexp fst snd))]
      [(Rel:IntxInt (app ral-expr fst) (app ral-expr snd))
       (Prim ty (mk-rel:int-int pexp fst snd))] 
      [otherwise (match-pass-error pass 'iic-prim otherwise)]))
  
  (match prgm
    [(Prog n e) (Prog n (ral-expr e))]
    [otherwise (match-pass-error pass 'body prgm)]))


