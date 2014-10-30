#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: compiler/closures/convert-closures                                           |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
| Description: This pass sets up the structure of closures 
+-------------------------------------------------------------------------------+
| Grammer:
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require schml/framework/build-compiler
         schml/framework/helpers
         schml/framework/errors
         schml/compiler/language)

(provide convert-closures)

(: convert-closures (Lambda2-Lang Config . -> . Lambda3-Lang))
(define (convert-closures prgm conf)
  (match-let ([(Prog (list name count type) exp) prgm])
    (let-values ([(exp count) (cc-expr exp count)])
      (Prog (list name count type) exp))))

(: cc-expr (-> L2-Expr Natural (values L3-Expr Natural)))
(define (cc-expr exp next)
  (match exp
    [(Letrec b* exp)
     (let*-values ([(bp* bd* next) (cc-bnd-lambda* b* next)]
		   [(exp next) (cc-expr exp next)])
       (values (LetP bp* (LetC bd* exp)) next))] 
    [(Let b* exp)
     (let*-values ([(b* next) (cc-bnd-data* b* next)]
		   [(exp next) (cc-expr exp next)])
       (values (Let b* exp) next))]
    [(If t c a)
     (let*-values ([(t next) (cc-expr t next)]
		   [(c next) (cc-expr c next)]
		   [(a next) (cc-expr a next)])
       (values (If t c a) next))]
    [(Begin stm* exp)
     (let*-values ([(stm* next) (cc-stmt* stm* next)]
                   [(exp next) (cc-expr exp next)])
       (values (Begin stm* exp) next))]
    [(App exp exp*)
     (let*-values ([(exp next) (cc-expr exp next)]
		   [(exp* next) (cc-expr* exp* next)])
       (if (Var? exp)
           (values (App (cons exp exp) exp*) next)
           (let*-values ([(tmp-u next) (next-uid "tmp_clos" next)]
			 [(tmp-v) (Var tmp-u)])
             (values (Let (list (cons tmp-u exp))
			  (App (cons tmp-v tmp-v) exp*))
		     next))))]
      [(Op p exp*) 
       (let-values ([(exp* next) (cc-expr* exp* next)])
	 (values (Op p exp*) next))]
      [(Fn-Caster exp)
       (let-values ([(exp next) (cc-expr exp next)])
         (values (Fn-Caster exp) next))]
      [(Halt) (values (Halt) next)]
      [(Var u) (values (Var u) next)]
      [(Quote k) (values (Quote k) next)]))

(: cc-expr* (-> (Listof L2-Expr) Natural (values (Listof L3-Expr) Natural)))
(define (cc-expr* exp* next)
  (if (null? exp*)
      (values '() next)
      (let ([exp (car exp*)] [exp* (cdr exp*)])
	(let*-values ([(exp* next) (cc-expr* exp* next)]
		      [(exp next) (cc-expr exp next)])
	  (values (cons exp exp*) next)))))

(: cc-bnd-lambda* (-> L2-Bnd-Lambda* Natural 
		      (values L3-Bnd-Procedure* L3-Bnd-Closure* Natural)))
(define (cc-bnd-lambda* bnd* next)
  (if (null? bnd*)
      (values '() '() next)
      (let ([bnd (car bnd*)] [bnd* (cdr bnd*)])
	(let-values ([(bndp* bndc* next) (cc-bnd-lambda* bnd* next)])
	  (match-let* ([(cons u1 lam) bnd]
		       [(Lambda fml* _ (Free ctr? fv* exp)) lam])
	   (let*-values ([(exp next)  (cc-expr exp next)]
			 [(u2 next) (mk-clos-ptr-uid u1 next)]
			 [(u3 next) (mk-code-ptr-uid u1 next)])
	    (let* ([proc (Procedure u2 fml* ctr? fv* exp)]
		   [bndp (cons u3 proc)]
		   [clos (Closure-Data u3 ctr? fv*)]
		   [bndc (cons u1 clos)])
	      (values (cons bndp bndp*) (cons bndc bndc*) next))))))))

(: cc-bnd-data* (-> L2-Bnd-Data* Natural
		    (values L3-Bnd-Data* Natural)))
(define (cc-bnd-data* bnd* next)
  (let ((Bnd* (inst cons L3-Bnd-Data L3-Bnd-Data*)))
    (if (null? bnd*)
      (values '() next)
      (let ([bnd (car bnd*)] [bnd* (cdr bnd*)])
	(let*-values ([(bnd* next) (cc-bnd-data* bnd* next)])
	  (match-let ([(cons uid exp) bnd])
	    (let*-values ([(exp next) (cc-expr exp next)]
			  [(bnd) (cons uid exp)])
	      (values (Bnd* bnd bnd*) next))))))))


(define-syntax-rule (mk-uid/uid u s next)
  (next-uid (string-append (Uid-prefix u) s) next))

(: mk-code-ptr-uid (Uid Natural . -> . (values Uid Natural)))
(define (mk-code-ptr-uid u next) (mk-uid/uid u "_code" next))
(: mk-clos-ptr-uid (Uid Natural . -> . (values Uid Natural)))
(define (mk-clos-ptr-uid u next) (mk-uid/uid u "_clos" next))

(: cc-stmt* (-> L2-Stmt* Natural (values L3-Stmt* Natural)))
(define (cc-stmt* stm* next)
  (if (null? stm*)
      (values '() next)
      (let*-values ([(stm next) (cc-stmt (car stm*) next)]
                    [(stm* next) (cc-stmt* (cdr stm*) next)])
	  (values (cons stm stm*) next))))

(: cc-stmt (-> L2-Stmt Natural (values L3-Stmt Natural)))
(define (cc-stmt stm next)
  (match stm
    [(Op p e*)
     (let-values ([(e* next) (cc-expr* e* next)])
       (values (Op p e*) next))]
    [otherwise (error 'cc-stmt "Unmatched statement ~a" stm)]))
