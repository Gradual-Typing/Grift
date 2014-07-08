#lang racket
#|------------------------------------------------------------------------------+
|Pass: compiler/closures/remove-anonymous-lambdas                                    |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
| Description: This pass is simplifies the runtime
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
  (define mk-uvar 'implicit-boxed-function)
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
            (Let ty-casted `(,(Bnd:Ty tmp exp ty-exp))
                 (Dyn:Closure:Make (Var ty-exp tmp) label ty-to ty-from))))))
  (define (efc-projected-function ty-casted ty-exp exp b-lbl)
    (define (help ty-casted ty-var var blame-label)
      (let* ((mk-arg-cast (lambda (cast-ty exp-ty uvar) 
			    (Cast exp-ty (Var cast-ty uvar) cast-ty b-lbl)))
	     (cast-from (Function-from ty-casted))
	     (cast-to (Function-to ty-casted))
	     (cast-arrity (length cast-to))
	     (uvars (map mk-cast-tmp cast-from))
	     (fmls  (map Fml:Ty uvars cast-from))
	     (dyn-tmp (mk-uvar "dyn_fn_struct_tmp"))
	     (unbox-dyn (Dyn:Cast-to-Closure var b-lbl))
	     (unbox-bnd* `(,(Bnd:Ty dyn-tmp unbox-dyn Dyn-Clos-Type)))
	     (exp*  (map mk-arg-cast exp-to cast-to uvar)))
	(Lambda ty-casted fmls
		(Let cast-to unbox-bnd*
		     (When/Blame cast-to b-lbl arrity-check
				 (Cast cast-to 
				       (App exp-return exp exp*) 
				       exp-to b-lbl)))))
    (if (Var? exp)
        (help ty-casted ty-exp exp blame-label)
        (let ((tmp (mk-uvar "fun_proj_tmp")))
          (Let ty-casted '((Bnd:Ty tmp exp ty-exp))
               (help ty-casted ty-exp (Var ty-exp tmp) blame-label)))))
  ;; If we are aiming at space efficieciency this line seems like the
  ;; logical place to start.
  (define (efc-function-cast ty-casted ty-exp exp b-lbl)
    (define (mk-cast-tmp _) (mk-uvar "fn_cast_tmp"))
    (let* ((mk-arg-cast (lambda (cast-ty exp-ty uvar) 
			  (Cast exp-ty (Var cast-ty uvar) cast-ty b-lbl)))
	   (cast-from (Function-from ty-casted))
	   (cast-to (Function-to ty-casted))
	   (exp-from (Function-from ty-exp))
	   (exp-to (Function-to ty-exp))
	   (uvars (map mk-cast-tmp cast-from))
	   (fmls  (map Fml:Ty uvars cast-from))
	   (exp*  (map mk-arg-cast exp-to cast-to uvar)))
      (Lambda ty-casted fmls
	      (Cast cast-to (App exp-return exp exp*) exp-to b-lbl))))
  
  (match prgm
    [(Prog n c t e)
     (let* ((_ (set! mk-uvar (get-uvar-maker c))) 
	    (e (efc-expr e))
	    (c (get-uvar-maker)))
       (Prog n c t e))]
    [otherwise (match-pass-error pass 'body prgm)]))


