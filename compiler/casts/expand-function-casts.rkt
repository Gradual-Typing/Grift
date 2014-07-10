#lang racket
#|------------------------------------------------------------------------------+
|Pass: compiler/closures/remove-anonymous-lambdas                                    |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
| Description: This pass is simplifies the runtime by eliminating function      |
| casts. This inlimination is really just an inlining of the code that would    |
| functions at runtime. It is absolutely necissary at this point because we have|
| no way of creating new functions at runtime. 
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
  (define (efc-cast ty-casted ty-exp exp b-lbl)
    (cond
      [(Dyn? ty-casted)
       (if (Function? ty-exp)
           (efc-injected-function ty-casted ty-exp exp b-lbl)
           (Cast ty-casted ty-exp exp b-lbl))]
      [(Dyn? ty-exp)
       (if (Function? ty-casted)
           (efc-projected-function ty-casted ty-exp exp b-lbl)
           (Cast ty-casted ty-exp exp b-lbl))]
      [(Function? ty-exp)
       (if (Function? ty-casted)
           (efc-function-cast ty-casted ty-exp exp b-lbl)
           (Cast ty-casted ty-exp exp b-lbl))]
      [else (Cast ty-casted ty-exp exp b-lbl)]))

  ;; When a function is injected into the dynamic type a
  ;; runtime dynamic-record is created to store the type information
  ;; of the function.

  ;; Functions cast to dynamic generate the type checking code so
  ;; that the function cast may be expanded 
  (define (efc-injected-function ty-casted ty-exp exp b-lbl)
    (let ((ty-to (Function-to ty-exp))
          (ty-from* (Function-from ty-exp)))
      (if (Var? exp)
          (Dyn:Closure:Make exp label ty-from ty-to)
          (let ((tmp (mk-uvar "fun_inj_tmp")))
            (Let ty-casted `(,(Bnd:Ty tmp exp ty-exp))
                 (Dyn:Closure 2 (lambda (rt-blame ty)
                                  ())) (Dyn:Closure:Make (Var ty-exp tmp) label ty-to ty-from))))))

  ;; When a function is projected from the dynamic type a
  ;; series of casts are performed to the arguments and
  ;; the return type. There is a dependance of the runtime value of
  ;; the dynamic-record so primitive operations must be exposed.
  ;; Additionally any casts created must be able to react to
  ;; abitrary origin types stored in the dynamic
  (define (efc-projected-function ty-casted ty-exp exp b-lbl)
    (define (help ty-casted ty-var var b-lbl)
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
	     (exp*  (map mk-arg-cast exp-to cast-to uvar)online c interpreter
                ))
	(Lambda ty-casted fmls
		(Let cast-to unbox-bnd*
		     (When/Blame cast-to b-lbl arrity-check
				 (Cast cast-to 
				       (App exp-return exp exp*) 
				       exp-to b-lbl))))))
    (if (Var? exp)
        (help ty-casted ty-exp exp b-lbl)
        (let ((tmp (mk-uvar "fun_proj_tmp")))
          (Let ty-casted '((Bnd:Ty tmp exp ty-exp))
               (help ty-casted ty-exp (Var ty-exp tmp) b-lbl)))))

  ;; normally 
  (define (cast-to-obscured-type ty obscured-ty exp lbl)
    (cond
      [(Dyn? ty)
       (If Obscured-Type (Prim Bool-Type (Type:Function? blind-ty-var))
           (dyn->higher-order-obscured obscured-ty ty exp lbl) ;; This is a oh damn clause
           (dyn->first-order-obscured obscured-ty ty exp lbl))]
      [(Function? ty)
       (If Obscured-Type (Prim Bool-Type (Type:Function? blind-ty-var))
           (higher-order->higher-order-obscured obscured-ty ty exp lbl)
           (When/Blame Obscured-Type lbl (Prim Bool-Type (Type:Dyn? blind-ty-var))
               (higher-order->dyn obscured-ty ty exp lbl)))]
      [else (obscured-first-order-cast obscured-ty ty exp lbl)]))
  (define (cast-from-obscured-type ty obscured-ty type-index exp lbl)
    (cond
      [(Dyn? ty)
       (If Obscured-Type (Prim Bool-Type (Type:Function? blind-ty-var))
           (higher-order-obscured->dyn ty obscured-ty exp lbl)
           (first-order-obscured->dyn ty obscured-ty exp lbl))]
      [(Function? ty)
       (If Obscured-Type (Prim Bool-Type (Type:Function? blind-ty-var))
           (higher-order-obscured->higher-order ty obscured-ty exp lbl)
           (When/Blame Obscured-Type lbl (Prim Bool-Type (Type:Dyn? blind-ty-var))
               (obscured-higher-order-injection ty obscured-ty exp lbl)))]
      [else (obscured-first-order-cast ty obscured-ty exp lbl)]))
  
  ;; Needs higher-order-cast-from-obscured  and to
  ;;       higher-order-cast-to-dyn
  ;;       higher-order-injection-fromcast-to-dyn
  ;;       first-order-cast-from-obscured
  ;;       first-order-cast-to-obscured
  
  ;; For the time being this is one way to perform function casts
  ;; but ultimately in order to gain space efficiency a mechinism
  ;; that allows to mutate casted functions may allow for space
  ;; efficiency
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
           (exp*  (map mk-arg-cast cast-to exp-to uvar)))
      (Lambda ty-casted fmls
              (Cast cast-to (App exp-return exp exp*) exp-to b-lbl))))

  (match prgm
    [(Prog n c t e)
     (let* ((_ (set! mk-uvar (get-uvar-maker c))) 
	    (e (efc-expr e))
	    (c (get-uvar-maker)))
       (Prog n c t e))]
    [otherwise (match-pass-error pass 'body prgm)]))


