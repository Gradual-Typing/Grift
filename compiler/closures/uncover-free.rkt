#lang racket
#|------------------------------------------------------------------------------+
|Pass: compiler/closures/uncover-free                                           |
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
         Schml/framework/errors
         racket/set)

(require Schml/language/shared
         (prefix-in s: Schml/language/closure-il1)
         (prefix-in t: Schml/language/closure-il2))
;; Only the pass is provided by this module
(provide uncover-free)

(define-pass (uncover-free prgm comp-config)
  (define (set-union* s* s**)
    (for/fold ([s* s*]) ([s*^ (in-list s**)]) (set-union s* s*^)))
  (define (lookup env)
    (lambda (id)
      (env-lookup env id (th (pass-error pass 'lookup id)))))
  (define (uf-expr env)
    (define (recur exp)
      (match exp
        [(s:Let-Proc ty
                     (list (Bnd:Ty i* (app (uf-proc env) p* f**) t*) ...)
                     exp)
         (let*-values
             ([(new-exp f*)
               ((uf-expr (env-extend* env i* (map Fml:Ty i* t*))) exp)]
              [(f*) (set-subtract f* (list->seteq i*))])
           (values (t:Let-Proc ty (map Bnd:Ty i* p* t*) new-exp)
                   (set-union* f* f**)))]
        [(s:Let ty (list (Bnd:Ty i* (app recur e* f**) t*) ...) exp)
         (let*-values
             ([(new-exp f*)
               ((uf-expr (env-extend* env i* (map Fml:Ty i* t*))) exp)]
              [(f*) (set-subtract f* (list->seteq i*))])
           (values (t:Let ty (map Bnd:Ty i* e* t*) new-exp)
                   (set-union* f* f**)))]
        [(s:Cast ty-cast (app recur exp f*) ty-exp label)
         (values (t:Cast ty-cast exp ty-exp label) f*)]
        [(s:If ty (app recur tst tstf*)
               (app recur csq csqf*) (app recur alt altf*))
         (values (t:If ty tst csq alt) (set-union tstf* csqf* altf*))]
        [(s:App ty (app recur exp f*) (list (app recur exp* f**) ...))
         (values (t:App ty exp exp*)
                 (set-union* f* f**))]
        [(s:Prim ty pexp) (uf-prim recur ty pexp)]
        [(s:Var t i) (values (t:Var t i) (seteq i))]
        [(s:Const t k) (values (t:Const t k) (seteq))]
        [e (match-pass-error pass 'uf-expr e)]))
    recur)
  (define (uf-prim uf-expr ty pexp)
    (match pexp
      [(Op:IntxInt (app uf-expr fst fstf*) (app uf-expr snd sndf*))
       (values (t:Prim ty (mk-op:int-int pexp fst snd))
               (set-union fstf* sndf*))]
      [(Rel:IntxInt (app uf-expr fst fstf*) (app uf-expr snd sndf*))
       (values (t:Prim ty (mk-rel:int-int pexp fst snd))
               (set-union fstf* sndf*))] 
      [otherwise (match-pass-error pass 'iic-prim otherwise)]))
  (define (uf-proc env)
    (lambda (proc)
      (match proc
        [(s:Lambda ty (and fmls (list (Fml:Ty i* _) ...)) exp)
         (let*-values ([(new-exp f*) ((uf-expr (env-extend* env i* fmls)) exp)]
                       [(f*) (set-subtract f* (list->seteq i*))])
           (values (t:Lambda ty fmls (set-map f* (lookup env)) new-exp) f*))]
        [e (match-pass-error pass 'uf-proc e)])))
  (match prgm
    [(s:Prog n u t (app (uf-expr (empty-env)) e f*))
     (if (set-empty? f*)
         (t:Prog n u t e)
         (pass-error pass 'free-variables-in-program f*))]
    [otherwise (match-pass-error pass 'body prgm)]))



