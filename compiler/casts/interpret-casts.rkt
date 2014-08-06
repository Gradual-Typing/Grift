#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: compiler/casts/interpret-casts                                           |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
|Discription:                                                                   |
|                                                                               |
+-------------------------------------------------------------------------------+
|Input Grammar "Explicitly-Typed-Core-Forms" found in Schml/languages/core.rkt  |
|Prog   = (Prog File-Name {TExpr} Type)                                         |
|TExpr  = {Expr}                                                                |
|Expr   = (Lambda {uvar}* {Type} {Expr} {Src})                                  |
|       | (Var {Uvar} {Type} {Src})                                             |
|       | (App {Expr} {Expr}* {Type} {Src})                                     |
|       | (Op Prim {Expr}* {Type} {Src})                                        |
|       | (Cast {Expr} {Type} {Type} {Blame})                                   |
|       | (If {Expr} {Expr} {Expr} {Type} {Src})                                |
|       | (Let {BndSet} {Expr} {Type} {Src})                                    |
|       | (Const {Imdt} {Type})                                                 |
|BndSet = ({Uvar} {Expr})                                                       |
|Fml    = {Uvar} | ({Uvar} {Type})                                              |
|Src    = (Src File Line Col Span)                                              |
|Blame  = {Src} | {String}                                                      |
|Uvar   = A Symbol with the format 'original$uniqueness                         |
|Imdt   = Fixnums and Booleans                                                  |
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
(provide interpret-casts)

(define-type CF Cast-Form)
(define-type LF Lambda-Form)
(define-type CT Cast-Type)
(define-type LT Lambda-Type)
(define NUMBER-OF-NEW-UVARS 10)
(define VP VALUE-PTR)
(define TP TYPE-PTR)
(define LP LABEL-PTR)
(define INTERP-TY (Fn/a 4 `(,VP ,TP ,TP ,LP) VP))

(: interpret-casts (Cast-Prog Config . -> . Lambda-Prog))
(define (interpret-casts prgm comp-config)
  (match-let ([(Cast-Prog n c e t) prgm])
    (Lambda-Prog n (+ c NUMBER-OF-NEW-UVARS) 
		 (mk-cast-interp c e t) t)))

(: mk-cast-interp (Natural CF CT . -> . LF))
(define (mk-cast-interp next exp ty)
  (let ((u (Uvar "interp_cast" next)))
    (Letrec `(,(Bnd u VP (finalize-interp-code u next)))
	    (convert-casts exp (Var u INTERP-TY))
	    ty)))

(: finalize-interp-code (Uvar Natural . -> . LF))
(define (finalize-interp-code uvar next)
  ;;The code that follows is rather strange but remember
  ;;that utimately the return type is a Lambda-Form.
  ;;First I create all the variables that are needed for
  ;;the code. Next I define a series of macros that allow
  ;;me to program in the Lambda Language. This is an aim to
  ;;simplify and reduce errors in constructing the ast.
  (let* ([v  (Uvar "val" (+ next 1))]     [v-var (Var v VP)]
	 [v2 (Uvar "val" (+ next 2))]     [v2-var (Var v2 VP)]
	 [t1 (Uvar "type1" (+ next 3))]   [t1-var (Var t1 TP)]
	 [t11 (Uvar "type11" (+ next 4))] [t11-var (Var t11 TP)]
	 [t12 (Uvar "type12" (+ next 5))] [t12-var (Var t12 TP)]
	 [t2 (Uvar "type1" (+ next 6))]   [t2-var (Var t2 TP)]
	 [t21 (Uvar "type11" (+ next 7))] [t21-var (Var t21 TP)]
	 [t22 (Uvar "type12" (+ next 8))] [t22-var (Var t22 TP)]
	 [l (Uvar "label" (+ next 9))]    [l-var (Var l TP)])
    (define-syntax-rule (lambda ((x t) ...) b)
      (Lambda (list (Fml x t) ...) VP b INTERP-TY))
    (define-syntax-rule (let ((v e) ...) b)
      (Let (list (Bnd v TP e) ...) b VP))
    (define-syntax-rule (? p a ...)
      (Op 'p (list a ...) BOOL-TYPE))
    (define-syntax-rule (V p a ...)
      (Op 'p (list a ...) VP))
    (define-syntax-rule (T p a ...)
      (Op 'p (list a ...) TP))
    ;; cond is barely usable but it makes the code more compact
    (define-syntax-rule (if t c a)
      (If t c a VP))
    (define-syntax-rule (when t v)
      (When/blame l-var t v VP))
    (define-syntax cond
      (syntax-rules (else)
	[(_ (else p e)) (when p e)]
	[(_ (p r) c ...) (if p r (cond c ...))]))
    (define-syntax-rule (recur v t1 t2 l)
      (App (Var uvar INTERP-TY) `(,v ,t1 ,t2 ,l) VP))
    (define-syntax-rule (app p a ...)
      (App p `(,a ...) VP))
    ;; Building the components
    (let* ([cast-int 
	    : LF 
	    (cond
	     [(? Type:Int? t2-var) v-var]
	     [else (? Type:Dyn? t2-var) 
		   (V Dyn:Int v-var)])]
	   [cast-bool 
	    : LF
	    (cond
	     [(? Type:Bool? t2-var) v-var]
	     [else (? Type:Dyn? t2-var) 
		   (V Dyn:Bool v-var)])]
	   [cast-dyn 
	    : LF
	    (cond
	     [(? Type:Dyn? t2-var) v-var]
	     [(? Dyn:Int? v-var) 
	      (recur (V Dyn:Int! v-var) (Quote 'Int TP) t2-var l-var)]
	     [(? Dyn:Bool? v-var)
	      (recur (V Dyn:Bool! v-var) (Quote 'Bool TP) t2-var l-var)]
	     [else (? Dyn:Fn? v-var)
		   (recur (V Dyn:Fn! v-var) (T Dyn:FnT v-var) t2-var l-var)])]
	   [cast-fn  
	    : LF
	    (cond
	     [(? Type:Dyn? t2-var) (V Dyn:Fn v-var t1-var)]
	     [else 
	      (? Type:Fn? t2-var)
	      (let ([t11 (T Type:Fn-arg t1-var)]
		    [t12 (T Type:Fn-ret t1-var)]
		    [t21 (T Type:Fn-arg t2-var)]
		    [t22 (T Type:Fn-ret t2-var)])
		(lambda ((v2 VP))
		  (recur (app v-var (recur v2-var t21-var t11-var l-var))
			 t12-var t22-var l-var)))])])
      (lambda ((v VP) (t1 TP) (t2 TP) (l LP))
	(cond
	 [(? Type:Int? t1-var) cast-int]
	 [(? Type:Bool? t1-var) cast-bool]
	 [(? Type:Dyn? t1-var) cast-dyn]
	 [else (? Type:Fn? t1-var) cast-fn])))))

(: convert-casts (CF LF . -> . LF))
(define (convert-casts exp cast-interp-var)
  (: cast (LF CT CT String . -> . LF))
  (define (cast e t1 t2 l)
    (App cast-interp-var
	 (list e (type->rt-type t1) (type->rt-type t2) (Quote l LP))
	 t2))
  ;; This is kinda important because it is were types are converted
  ;; Values
  (: type->rt-type (CT . -> . LF))
  (define (type->rt-type t)
    (cond 
     [(Dyn? t)  (Quote 'Dyn TP)]
     [(Bool? t) (Quote 'Bool TP)]
     [(Int? t)  (Quote 'Int TP)]
     [(and (Fn/a? t) (= (Fn/a-arity t) 1))
      (let ((arg (car (Fn/a-fmls t)))
	    (ret (Fn/a-ret t)))
	(Op 'Type:Fn 
	    (list (Quote 1 INT-TYPE) (type->rt-type arg) (type->rt-type ret)) 
	    TP))]
     [else (raise-limited-types-exn t)]))
  (: recur (CF . -> . LF))
  (define (recur exp)
    (match exp
      [(Lambda f* t b a) (Lambda f* t (recur b) a)]
      [(Letrec b* e a) (Letrec (cata-bnd* b*) (recur e) a)]
      [(Let b* e a) (Let (cata-bnd* b*) (recur e) a)]
      [(App e e* a) (App (recur e) (recur* e*) a)]
      [(Op p e* a) (Op p (recur* e*) a)]
      [(Cast e t1 t2 l) (cast (recur e) t1 t2 l)]
      [(If t e1 e2 a) (If (recur t) (recur e1) (recur e2) a)]
      [(Var i a) (Var i a)]
      [(Quote k a) (Quote k a)]))
  (: recur* ((Listof CF) . -> . (Listof LF)))
  (define (recur* e*) (for/list ([e e*]) (recur e)))
  (: cata-bnd* ((Listof (Bnd CF CT)) . -> . (Listof (Bnd LF CT))))
  (define (cata-bnd* b*) (for/list ([b b*]) (Bnd (Bnd-identifier b)
						 (Bnd-type b)
						 (recur (Bnd-expression b)))))
  (recur exp))







#| This is the original draft for the interpreter	
(Lambda `(,(Fml v VP) 
	      ,(Fml t1 TP)
	      ,(Fml t2 TP)
	      ,(Fml l Label-PTR))
	    VP
	    (IF (Op 'Type:Int? '(,t1-var) Bool-Type)
		(IF (Op 'Type:Int? '(,t2-var) Bool-Type)
		    v
		    (When/Blame l-var (Op 'Type:Dyn? '(,t2-var) Bool-Type)
				(Op 'Dyn:Box-Int '(,v-var) VAL-PRT)))
		(IF (Op 'Type:Bool? '(,t1-var) Bool-Type)
		    (IF (Op 'Type:Bool? '(,t2-var) Bool-Type)
			v
			(When/Blame l-var (Op 'Type:Dyn? '(,t2-var) Bool-Type) l-var
				    (Op 'Dyn:Box-Int '(,v-var) VAL-PRT)))
		    (IF (Op 'Type:Dyn? '(,t1-var) Bool-Type)
			(IF (Op 'Type:Dyn? '(,t2-var) Bool-Type)
			    v
			    (IF (Op 'Dyn:Int? '(,v-var) Bool-Type)
				(App recur `(,(Op 'Dyn:Unbox-Immediate `(,v-var) VP)
					     ,(Quote INT-TYPE-VAL TP)
					     ,t2-var
					     ,l-var))
				(IF (Op 'Dyn:Bool? '(,v-var) Bool-Type)
				    (App recur `(,(Op 'Dyn:Unbox-Immediate `(,v-var) VP)
						 ,(Quote BOOL-TP TP)
						 ,t2-var
						 ,l-var))
				    (When/blame l-var (Op 'Dyn:Fn? '(,v-var) Bool-Type)
						(App recur `(,(Op 'Dyn:Fn:value `(,v-var) VP)
							     ,(Op 'Dyn:Fn:type `(,v-var) TP)
							     ,t2-var
							     ,l-var) VP)))))
			(When/Blame l-var (Op 'Type:Fn? `(,t1-var) Bool-Type)
				    (IF (Op 'Type:Dyn? '(,t2-var) Bool-Type)
					(Op 'Dyn:Box-Fn '(,v-var ,t2-var) VP)
					(When/blame l-var (Op 'Type:Fn? `(,t2-var) Bool-Type)
						    (Let `(,(Bnd t11 (Op 'Type:Fn:argument `(,t1-var) TP))
							   ,(Bnd t12 (Op 'Type:Fn:return   `(,t1-var) TP))
							   ,(Bnd t21 (Op 'Type:Fn:argument `(,t2-var) TP))
							   ,(Bnd t22 (Op 'Type:Fn:return   `(,t2-var) TP)))
							 (Lambda `(,(Fml fn-v VP)) VP
								 (App recur 
								      `(,(App v-var 
									      `(,(App recur `(,fn-v-var ,t21-var ,t11-var ,l-var) VP))
									      VP)
									,t12
									,t22
									,l-var)
								      VP))))))))))))
|#

    

