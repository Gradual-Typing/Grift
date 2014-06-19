#lang racket
#|------------------------------------------------------------------------------+
|Pass: compiler/closures/convert-closures                                           |
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
         (prefix-in s: Schml/language/closure-il2)
         (prefix-in t: Schml/language/closure-il3))
;; Only the pass is provided by this module
(provide convert-closures)

(define-pass (convert-closures prgm comp-config)
  (define (cc-expr mk-uvar)
    (define (uvar->code-label i)
      (clabel (uvar-prefix i) (uvar-suffix i)))
    (define (mk-cp-var i)
      (mk-uvar (string-append "cp_" (uvar-prefix i))))
    (define (cc-prim cc-expr ty pexp)
      (match pexp
        [(Op:IntxInt (app recur fst) (app recur snd))
         (t:Prim ty (mk-op:int-int pexp fst snd))]
        [(Rel:IntxInt (app recur fst) (app recur snd))
         (t:Prim ty (mk-rel:int-int pexp fst snd))] 
        [otherwise (match-pass-error pass 'iic-prim otherwise)]))
    (define (recur exp)
      (match exp
        ;; This ugly line makes two new labels for each lambda
        ;; and creates a temporary binding for the soon to be
        ;; closure
        [(s:Let-Proc ty
                   (list (Bnd:Ty (and i*
                                      (app uvar->code-label cl*)
                                      (app mk-cp-var cp*))
                                 (s:Lambda tl* fl** fr** (app recur e*))
                                 lt*) ...)
                   (app recur exp))
       (t:Let-Proc ty
                   (map
                    (lambda (cl cp tl fl fr e lt)
                      (let ((f (Fml:Ty cp lt)))
                        (Bnd:Ty
                         cl
                         (t:Lambda tl (cons f fl) (cons f fr) e)
                         lt)))
                    cl* cp* tl* fl** fr** e* lt*)
                   (map
                    (lambda (i cl fr* lt)
                      (Bnd:Ty i (t:Close-Over cl fr*) lt))
                    i* cl* fr** lt*)
                   exp)]
      [(s:Let ty (list (Bnd:Ty i* (app recur e*) t*) ...) (app recur exp))
       (t:Let ty (map Bnd:Ty i* e* t*) exp)]
      [(s:Cast ty-cast (app recur exp) ty-exp label)
       (t:Cast ty-cast exp ty-exp label)]
      [(s:If ty (app recur tst) (app recur csq) (app recur alt))
       (t:If ty tst csq alt)]
      [(s:App ty (app recur exp) (list (app recur exp*) ...))
       (if (t:Var? exp)
           (t:App ty exp `(,exp . ,exp*))
           (let* ((tmp (mk-uvar "tmp_clos"))
                  (tmp-ty (t:Expr-ty exp))
                  (tmp-expr (t:Var tmp-ty tmp)))
             (t:Let ty `(,(Bnd:Ty tmp exp tmp-ty))
                    (t:App ty tmp-expr `(,tmp-expr . ,exp*)))))] 
      [(s:Prim ty pexp) (cc-prim recur ty pexp)]
      [(s:Var t i) (t:Var t i)]
      [(s:Const t k) (t:Const t k)]
      [e (match-pass-error pass 'recur e)]))
    recur)
  
  (match prgm
    [(s:Prog n u t e) (let* ((mk-uvar (get-uvar-maker u))
                             (e ((cc-expr mk-uvar) e)))
                        (t:Prog n (mk-uvar) t e))]
    [otherwise (match-pass-error pass 'body prgm)]))

