#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: compiler/closures/lift-functions                           |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
| Description: Lifts lambda forms to the top level of the program now that they are
| flattened.
+-------------------------------------------------------------------------------+
| Grammer:
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require schml/src/helpers
         schml/src/errors
	 schml/src/language)


;; Only the pass is provided by this module
(provide lift-functions)

(: lift-functions (-> Lambda4-Lang Config Data0-Lang))
(define (lift-functions prgm comp-config)
  (match-let ([(Prog (list name count type) (app lf-expr exp bndc*)) prgm])
    (Prog (list name count type) (Labels bndc* exp))))

;; This can and will be much more efficient than appending everywhere
(: lf-expr (-> L4-Expr (values D0-Expr D0-Bnd-Code*)))
(define (lf-expr exp)
  (match exp
    [(Labels (app lf-bnd-code* bndc*) 
	     (Let (app lf-bnd* bnd* b*-bndc*) 
		  (Begin (app lf-stmt* e* e*-bndc*) 
			 (app lf-expr e e-bndc*))))
     (values (Let bnd* (Begin e* e)) (append b*-bndc* bndc* e*-bndc* e-bndc*))]
    [(Let (app lf-bnd* bnd* bndc*) (app lf-expr e e-bndc*))
     (values (Let bnd* e) (append bndc* e-bndc*))]
    [(If (app lf-expr tst t-b*) (app lf-expr csq c-b*) (app lf-expr alt a-b*))
     (values (If tst csq alt) (append t-b* c-b* a-b*))]
    [(App  (app lf-expr exp exp-bndc*) (app lf-expr* exp* exp*-bndc*))
     (values (App exp exp*) (append exp-bndc* exp*-bndc*))]
    [(Op p (app lf-expr* exp* exp*-bnd*)) 
     (values (Op (if (eq? p 'Fn-cast) 
		     (error 'type) 
		     p) 
		 exp*) exp*-bnd*)]
    [(Begin (app lf-stmt* stm* stm*-bnd) (app lf-expr exp exp-bnd))
     (values (Begin stm* exp) (append stm*-bnd exp-bnd))]
    ;;[(Halt) (values (Halt) '())]
    [(Var i) (values (Var i) '())]
    [(Code-Label i) (values (Code-Label i) '())]
    [(Quote l) (values (Quote l) '())]))

(: lf-bnd-code* (-> L4-Bnd-Code* D0-Bnd-Code*))
(define (lf-bnd-code* b*)
    (if (null? b*)
	'()
	(match-let ([(cons (cons uid (Code params* (app lf-expr exp bndc^*))) 
			   (app lf-bnd-code* bndc*)) 
		     b*])
	  (let ([bnd (cons uid (Code params* exp))]
		[bnd* (append bndc^* bndc*)])
	    (cons bnd bnd*)))))

(: lf-bnd* (-> L4-Bnd-Data* (values D0-Bnd* D0-Bnd-Code*)))
(define (lf-bnd* b*)
  (if (null? b*)
      (values '() '())
      (match-let ([(cons (cons uid (app lf-expr exp bndc^*)) 
			 (app lf-bnd* bndd* bndc*)) 
		   b*])
	(let ([bndd (cons uid exp)])
	  (values (cons bndd bndd*) (append bndc^* bndc*))))))

(: lf-expr* (-> L4-Expr* (values D0-Expr* D0-Bnd-Code*)))
(define (lf-expr* e*) 
    (if (null? e*)
        (values '() '())
	(match-let ([(cons (app lf-expr e e-bnd*)
			   (app lf-expr* e* e*-bnd*)) 
		     e*])
	  (values (cons e e*) (append e-bnd* e*-bnd*)))))

(: lf-stmt* (-> L4-Stmt* (values D0-Stmt* D0-Bnd-Code*)))
(define (lf-stmt* e*)
  (if (null? e*)
      (values '() '())
      (let*-values ([(s s-bnd) (lf-stmt (car e*))]
                    [(s* s*-bnd) (lf-stmt* (cdr e*))])
	(values (cons s s*) (append s-bnd s*-bnd)))))

(: lf-stmt (-> L4-Stmt (values D0-Stmt D0-Bnd-Code*)))
(define (lf-stmt e)
  (match e
    [(Op p (app lf-expr* e* e*-bnd))
     (values (Op p e*) e*-bnd)] 
    [otherwise (error 'lf-stmt "unmatched statment ~a" e)]))
