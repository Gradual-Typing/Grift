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

(require Schml/language/shared
         Schml/language/sourceless-core)
;; Only the pass is provided by this module
(provide expand-function-casts)

(define-pass (expand-function-casts prgm comp-config)
  (define (efc-expr exp)
    (match exp
      ;; The only important rule everything else is just a catamorphism
      [(Cast ty-cast (app efc-expr exp) ty-exp label)
       (efc-cast ty-cast ty-exp exp label)]
      [(Lambda ty fmls (app efc-expr exp)) (Lambda ty fmls exp)]
      [(Let ty (list (Bnd:Ty u* (app efc-expr e*) t*) ...) (app efc-expr body))
       (Let ty (map Bnd:Ty u* e* t*) body)]
      [(If ty (app efc-expr tst) (app efc-expr csq) (app efc-expr alt))
       (If ty tst csq alt)]
      [(App ty (app efc-expr exp) (list (app efc-expr exp*) ...))
       (App ty exp exp*)]
      [(Prim ty pexp) (efc-prim ty pexp)]
      [(Var t k) (Var t k)]
      [(Const t k) (Const t k)]
      [e (match-pass-error pass 'tyck-expr e)]))
  (define (efc-prim ty pexp)
    (match pexp
      [(Op:IntxInt (app efc-expr fst) (app efc-expr snd))
       (Prim ty (mk-op:int-int pexp fst snd))]
      [(Rel:IntxInt (app efc-expr fst) (app efc-expr snd))
       (Prim ty (mk-rel:int-int pexp fst snd))] 
      [otherwise (match-pass-error pass 'iic-prim otherwise)]))
  (define (efc-cast ty-casted ty-exp exp blame-label)
    (cond
      [(Dyn? ty-casted)
       (if (Function? ty-exp)
           (efc-injected-function ty-casted ty-exp exp blame-label)
           (Cast ty-casted ty-exp exp blame-label))]
      [(Dyn? ty-exp)
       (if (Function? ty-casted)
           (efc-projected-function ty-casted ty-exp exp blame-label)
           (Cast ty-casted ty-exp exp blame-label))]
      [(Function? ty-exp)
       (if (Function? ty-casted)
           (efc-function-cast ty-casted ty-exp exp blame-label)
           (Cast ty-casted ty-exp exp blame-label))]
      [else (Cast ty-casted ty-exp exp blame-label)]))
  (define (efc-injected-function ty-casted ty-exp exp blame-label)
    (let ((ty-to (Function-to ty-exp))
          (ty-from* (Function-from ty-exp)))
      (if (Var? exp)
          (Dyn:Closure:Make exp label ty-from ty-to)
          (let ((tmp (mk-uvar "fun_inj_tmp")))
            (Let ty-casted '((Bnd:Ty tmp exp ty-exp))
                 (Dyn:Closure:Make (Var ty-exp tmp) label ty-to ty-from))))))
  (define (efc-projected-function ty-casted ty-exp exp blame-label)
    (define (help ty-casted ty-exp var blame-label)
      (let)
      (If ty-casted (Dyn:Is var Tag:Closure)
          ()))
    (if (Var? exp)
        (help ty-casted ty-exp exp blame-label)
        (let ((tmp (mk-uvar "fun_proj_tmp")))
          (Let ty-casted '((Bnd:Ty tmp exp ty-exp))
               (help ty-casted ty-exp (Var ty-exp tmp) blame-label)))))
  (define (efc-function-cast ty-casted ty-exp exp blame-label)
    )
  
  (match prgm
    [(Prog n c t e) (Prog n c t (efc-expr e))]
    [otherwise (match-pass-error pass 'body prgm)]))


