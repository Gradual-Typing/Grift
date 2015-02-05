#lang typed/racket

(require schml/src/helpers
         schml/src/errors
	 schml/src/language)

;; Only the pass is provided by this module
(provide introduce-castable-references)

;; The entry point for this pass it is called by impose-casting semantics
(: introduce-castable-references (Cast0-Lang Config . -> . Cast1-Lang))
(define (introduce-castable-references prgm config)
  (match-let (((Prog annotation expression) prgm))
    (let ([exp (icr-expr expression)])
      (Prog annotation exp))))

(: icr-expr (-> C0-Expr C1-Expr))
(define (icr-expr exp)
  (match exp
      [(Lambda f* r e) (Lambda f* r (icr-expr e))]
      [(Cast e t1 t2 l) (Cast (icr-expr e) t1 t2 l)]
      [(Letrec b* e) (Letrec (map icr-bnd b*) (icr-expr e))]
      [(Let b* e) (Let (map icr-bnd b*) (icr-expr e))]
      [(App e e*) (App (icr-expr e) (map icr-expr e*))]
      [(Op p e*) (Op p (map icr-expr e*))]
      [(If t c a) (If (icr-expr t) (icr-expr c) (icr-expr a))]
      [(Begin s* e) (TODO implement begin in introduce castable references)]
      [(Var i) (Var i)]
      [(Quote lit) (Quote lit)]))

(: icr-bnd (-> C0-Bnd C1-Bnd))
(define (icr-bnd b)
  (match-let ([(Bnd u t e) b]) 
    (Bnd u t (icr-expr e))))

#|
(: icr-stmt (-> C0-Expr C1-Stmt))
(define (icr-stmt s)
  (match-let (((Op p e*) s))
    (Op p (map icr-expr e*))))
|#
