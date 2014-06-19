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
         (prefix-in s: Schml/language/sourceless-core)
         Schml/language/closure-il1)
;; Only the pass is provided by this module
(provide label-lambdas)

(define-pass (label-lambdas prgm comp-config)
  (define (ll-expr exp)
    ;; Any lambdas that are label get moved to let-proc
    ;; any anonymous lambdas are given a name and returned
    ;; from a let-proc
    (match exp
      [(s:Lambda ty fmls (app ll-expr exp))
       (let ((tmp-name (mk-uvar 'annon)))
         (Let-Proc ty
                   `(,(Bnd:Ty tmp-name (Lambda ty fmls exp) ty))
                   (Var ty tmp-name)))]
      [(s:Let ty bnd* (app ll-expr body))
       (ll-let ty bnd* body)]
      [(s:Cast ty-cast (app ll-expr exp) ty-exp label)
       (Cast ty-cast exp ty-exp label)]
      [(s:If ty
             (app ll-expr tst)
             (app ll-expr csq)
             (app ll-expr alt))
       (If ty tst csq alt)]
      [(s:App ty (app ll-expr exp) (list (app ll-expr exp*) ...))
       (App ty exp exp*)]
      [(s:Prim ty pexp) (ll-prim ty pexp)]
      [(s:Var t k) (Var t k)]
      [(s:Const t k) (Const t k)]
      [e (match-pass-error pass 'tyck-expr e)]))
  
  ;; ll-let takes the fields of from core and pulls all
  ;; bound procedures out into the let-proc form. Placing
  ;; the rest of the let as the body of the let-proc
  (define (ll-let t b* e)
    ;; split-bound-procedures actually performs the filtering
    (define (split-bound-procedures b*)
      (for/fold ([bp* '()] [br* '()])
          ([b (in-list b*)])
        (match b
          [(Bnd:Ty i (app ll-expr (and (Lambda t f* e) l)) t)
           (values (cons (Bnd:Ty i l t) bp*) br*)]
          [(Bnd:Ty i (app ll-expr e) t)
            (values bp* (cons (Bnd:Ty i e t) br*))])))
    (let-values ([(bp* br*) (split-bound-procedures b*)])
      ;; if both are null then we may remove an empty let
      ;; This transformation is only valid because bindings
      ;; being unique otherwise we might be changing
      ;; the scope on the rhs
      (cond 
        [(null? br*) (if (null? bp*) e (Let-Proc t bp* e))]
        [(null? bp*) (Let t br* e)] 
        [else (Let-Proc t bp* (Let t br* e))])))
  
  (define (ll-prim ty pexp)
    (match pexp
      [(Op:IntxInt (app ll-expr fst) (app ll-expr snd))
       (Prim ty (mk-op:int-int pexp fst snd))]
      [(Rel:IntxInt (app ll-expr fst) (app ll-expr snd))
       (Prim ty (mk-rel:int-int pexp fst snd))] 
      [otherwise (match-pass-error pass 'iic-prim otherwise)]))

  (define mk-uvar 'undefined)
  
  (match prgm
    [(s:Prog n c t e)
     (let* ((_ (set! mk-uvar (get-uvar-maker c)))
            (e (ll-expr e))
            (c (mk-uvar)))
       (Prog n c t e))]
    [otherwise (match-pass-error pass 'body prgm)]))


