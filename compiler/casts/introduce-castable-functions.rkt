#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: compiler/casts/introduce-castable-procedure                                           |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
|Discription:                                                                   |
|                                                                               |
+-------------------------------------------------------------------------------+
|Input Grammar Cast Language 0                                                  |
|Prog   = (Prog File-Name Next-Uid-Suffix Type Expr)                            |                               
|Expr   = (Lambda {(Fml Uid Type)}* {Type} {Expr})                              |
|       | (Var {Uvar})                                                          |
|       | (App {Expr} {Expr}*)                                                  |
|       | (Op Prim {Expr}*)                                                     |
|       | (Cast {Expr} {Type} {Type} {String})                                  |
|       | (If {Expr} {Expr} {Expr})                                             |
|       | (Let {(Bnd Uid Expr)}* {Expr})                                        |
|       | (Letrec {(Bnd Uid Expr)}* {Expr})                                     |
|       | (Quote {Literal})                                                     |
|Uid    = A Unique Identifier                                                   |
|Literal= Fixnums and Booleans                                                  |
|Prim   = op:fix:* | op:fix:+ | op:fix:- | op:fix:and | op:fix:or | op:fix:>>   |
|       | op:fix:<< | relop:fix:< | relop:fix:<= | relop:fix:=  | relop:fix:>=  |
|       | relop:fix:>                                                           |
|Type   = Fix | Bool | Dyn | ({Type}* -> {Type})                                |
+-------------------------------------------------------------------------------+
|Output Grammar                                                                 |
|                                                                               |
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require Schml/framework/build-compiler
         Schml/framework/helpers
         Schml/framework/errors
	 Schml/compiler/language)

;; Only the pass is provided by this module
(provide introduce-castable-functions)

(define-type Caster-State (Pair Natural (HashTable Index C1-Bnd)))

(: get-caster (Index Caster-State . -> . (Values Uid Caster-State)))
(define (get-caster arity state)
  (match-let ([(cons next casters) state])
    (let ([bnd? (hash-ref casters arity #f)])
	  (if bnd?
	      (values (Bnd-identifier bnd?) state)
	      (let-values ([(bnd next) (mk-caster-bnd arity next)])
		(values (Bnd-identifier bnd) (cons next (hash-set casters arity bnd))))))))

(: extract-state (-> Caster-State (Values Natural C1-Bnd*)))
(define (extract-state s) (values (car s) (hash-values (cdr s))))

(: new-state (-> Natural Caster-State))
(define (new-state n) (cons n ((inst hasheq Index C1-Bnd))))

(: introduce-castable-functions (Cast0-Lang Config . -> . Cast1-Lang))
(define (introduce-castable-functions prgm config)
  (match-let ([(Prog (list name next type) exp) prgm])
    (let*-values ([(exp state) (icf-expr exp (new-state next))]
		  [(next caster-bnd*) (extract-state state)])
      (Prog (list name next type) (Letrec caster-bnd* exp) 
	    ;(Ann  type)
	    ))))

(: icf-expr (-> C0-Expr Caster-State (values C1-Expr Caster-State)))
(define (icf-expr exp state)
  ; (match-let ([(Ann exp type) exp]))
  (match exp
      [(Lambda f* returns exp)
       (let*-values ([(uid state) (get-caster (length f*) state)]
		     [(exp state) (icf-expr exp state)])
	 (values (Lambda f* returns (Castable uid exp));(Ann  type)
		 state
		 ))]
      [(Cast exp t1 t2 lbl)
       (let*-values ([(exp state) (icf-expr exp state)]
		     [(t1) (Quote t1)]
		     [(t2) (Quote t2)]
		     [(lbl) (Quote lbl)])
	 (values (Cast exp t1 t2 lbl) state))]
      [(Letrec bnd* exp)
       (let*-values ([(bnd* state) (icf-bnd* bnd* state)]
		     [(exp state) (icf-expr exp state)])
	 (values (Letrec bnd* exp) state))]
      [(Let bnd* exp)
       (let*-values ([(bnd* state) (icf-bnd* bnd* state)]
		     [(exp state) (icf-expr exp state)])
	 (values (Let bnd* exp) state))]
      [(App exp exp*)
       (let*-values ([(exp state) (icf-expr exp state)]
		     [(exp* state) (icf-expr* exp* state)])
	 (values (App exp exp*) state))]
      [(Op p exp*)
       (let*-values ([(exp* state) (icf-expr* exp* state)])
	 (values (Op p exp*) state))]
      [(If tst csq alt)
       (let*-values ([(tst state) (icf-expr tst state)]
		     [(csq state) (icf-expr csq state)]
		     [(alt state) (icf-expr alt state)])
	 (values (If tst csq alt) state))]
      [(Var id) (values (Var id) state)]
      [(Quote lit) (values (Quote lit) state)]))

(: icf-expr* (-> C0-Expr* Caster-State (Values C1-Expr* Caster-State)))
(define (icf-expr* e* s)
  (if (null? e*)
      (values e* s)
      (let*-values ([(e s) (icf-expr (car e*) s)]
		    [(e* s) (icf-expr* (cdr e*) s)])
	(values (cons e e*) s))))

(: icf-bnd* (-> C0-Bnd* Caster-State (values C1-Bnd* Caster-State)))
(define (icf-bnd* b* s)
  (if (null? b*)
      (values '() s)
      (match-let ([(Bnd u t r) (car b*)])
	(let*-values ([(b* s) (icf-bnd* (cdr b*) s)]
		      [(r s) (icf-expr r s)])
	  (values (cons (Bnd u t r) b*) s)))))
    


(: mk-caster-bnd (-> Index Natural (values C1-Bnd Natural)))
(define (mk-caster-bnd ary next)
  (let*-values ([(uid next) (next-uid (format "cast-fn~a" ary) next)]
		[(type)     (mk-caster-type ary)]
		[(rhs next) (mk-caster-fn ary next type uid)])
    (values (Bnd uid type rhs) next)))

(define-type Caster-Type (Fn Index (Listof Cast-Type) Cast-Type))
(: mk-caster-type (-> Index Caster-Type))
(define (mk-caster-type ary)
  (Fn 2 
      (list ANY-VALUE ANY-TYPE ANY-TYPE STRING-PTR) 
      (Fn ary (make-list ary ANY-VALUE) ANY-VALUE)))

(: mk-caster-fn (-> Index Natural Caster-Type Uid (values C1-Expr Natural)))
(define (mk-caster-fn ary next type name)
  (: mk-caster-ids (-> Index Natural (Listof Uid) 
		       (values (Listof Uid) Natural)))
  (define (mk-caster-ids ary next vars)
    (if (zero? ary)
	(values vars next)
	(mk-caster-ids (sub1 ary) (add1 next) (cons (Uid "v" next) vars))))
  (: mk-casted-args (-> C1-Expr C1-Expr C1-Expr Uid* C1-Expr*))
  (define (mk-casted-args t1 t2 lbl uid*)
    (: loop (-> Uid* Index C1-Expr*))
    (define (loop uid* index)
      (if (null? uid*)
	  '()
	  (let ([val : C1-Expr (Var (car uid*))]
		[t1 : C1-Expr (Op 'Type:Fn-arg-ref 
				       (list t2 (Quote index)))]
		[t2 : C1-Expr (Op 'Type:Fn-arg-ref 
				       (list t1 (Quote index)))])
	    (cons (Cast val t1 t2 lbl)
		  (loop (cdr uid*) (cast (add1 index) Index))))))
    (loop uid* 0))
  (let*-values ([(fn next) (next-uid "f" next)]
		[(t1 next) (next-uid "t1" next)]
		[(t2 next) (next-uid "t2" next)]
		[(lbl next) (next-uid "lbl" next)]
		[(uid* next) (mk-caster-ids ary next '())])
    (let* ([fn-var : C1-Expr (Var fn)]
	   [t1-var : C1-Expr (Var t1)]
	   [t2-var : C1-Expr (Var t2)]
	   [lbl-var : C1-Expr (Var lbl)]
	   [fml* : Cast-Fml* (map (lambda([u : Uid]) (Fml u ANY-VALUE)) uid*)]
	   [casted-args : C1-Expr* (mk-casted-args t1-var t2-var lbl-var uid*)]
	   [fn-type : Cast-Type (Fn-ret type)]
	   [caster-fml : Cast-Fml* (list (Fml fn fn-type) 
					 (Fml t1 ANY-TYPE)
					 (Fml t2 ANY-TYPE) 
					 (Fml lbl STRING-PTR))]
	   [cast-call-cast : C1-Expr (Cast
				      (App fn-var casted-args)
				      (Op 'Type:Fn-return (list t1-var))
				      (Op 'Type:Fn-return (list t2-var)) 
				      lbl-var)]
	   [arity-test : C1-Expr (Op '= 
				     (list 
				      (Op 'Type:Fn-arity (list t1-var))
				      (Op 'Type:Fn-arity (list t2-var))))]
	   [apply-cast : C1-Expr (Lambda fml* ANY-VALUE 
					 (Castable name cast-call-cast))]
	   [apply-blame : C1-Expr (Lambda '() BOTTOM-TYPE 
					  (Castable 
					   name 
					   (Op 'Blame (list lbl-var))))]
	   [arity-check : C1-Expr (If arity-test apply-cast apply-blame)]
	   [caster  : C1-Expr (Lambda caster-fml fn-type (Castable #f arity-check))])
      (values caster next))))

