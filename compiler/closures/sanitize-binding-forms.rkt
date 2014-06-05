#lang racket
#|------------------------------------------------------------------------------+
|Pass: compiler/closures/sanitize-binding-forms                                 |
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
         Schml/language/closure-conversion)
;; Only the pass is provided by this module
(provide sanitize-binding-forms)

(define-pass (sanitize-binding-forms prgm config)
  (define (sbf-expr exp)
    ;; separate procedure and other bindings
    (define (sbf-let t b* e)
      (let-values ([(bp* br*)
                    (for/fold ([bp* '()] [br* '()])
                        ([b (in-list b*)])
                      (if (Lambda? (Bnd-expr b))
                          (values (cons b bp*) br*)
                          (values bp* (cons b br*))))])
        (cond
          ;; if both are null then we may remove an empty let
          [(null? br*) (if (null? bp*) e (Let-Proc t bp* e))]
          [(null? bp*) (Let t br* e)]
          ;; This transformation is only valid because bindings being
          ;; unique otherwise we might be changing the scope on the rhs
          [else (Let-Proc t bp* (Let t br* e))])))
    (match exp
      [(Let ty b* (app sbf-expr e)) (sbf-let ty b* e)]
      [(Let-Proc ty (list (Bnd:Ty i* (app sbf-expr e*) t*) ...) (app sbf-expr e))
       (Let-Proc ty (map Bnd:Ty i* e* t*) e)]
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



