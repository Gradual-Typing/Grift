#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: compiler/casts/interpret-casts                                           |
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
(provide interpret-casts)

(define NUMBER-OF-NEW-UVARS 10)
(define INTERP-TYPE
  (Fn 4 (list ANY-VALUE ANY-TYPE ANY-TYPE STRING-PTR) ANY-VALUE))

(: interpret-casts (Cast0-Lang Config . -> . Lambda0-Lang))
(define (interpret-casts prgm config)
  (match-let ([(Prog (list name next type) exp) prgm])
    ;; I am not following the usual pattern with next and uvars
    ;; becuase there is a fixed number being created in this pass.
    (Prog (list "" 1 INT-TYPE) 
	  (Ann (Quote 1) INT-TYPE))
    ;;(Prog (list name (+ next NUMBER-OF-NEW-UVARS) type) 
    ;; (mk-cast-interp next exp type)
    ))

;; (: mk-cast-interp (Natural C0-Expr Cast-Type . -> . L0-Expr))
;; (define (mk-cast-interp next exp type)
;;   (let*-values ([(interp-uid next) (next-uid "interp_cast" next)]
;; 		[(interp-var) (Ann (Var interp-uid) INTERP-TYPE)])
;;     (Ann (Letrec (list (Bnd interp-uid 
;; 			    INTERP-TYPE 
;; 			    (finalize-interp-code interp-var next))) 
;; 		 (convert-casts exp interp-var))
;; 	 type)))

;; (: finalize-interp-code (L0-Expr Natural . -> . L0-Expr))
;; (define (finalize-interp-code interp-var next)
;;   ;;The code that follows is rather strange but remember
;;   ;;that utimately the return type is a Lambda-Form.
;;   ;;First I create all the variables that are needed for
;;   ;;the code. Next I define a series of macros that allow
;;   ;;me to program in the Lambda Language. This is an aim to
;;   ;;simplify and reduce errors in constructing the ast.
;;   (let*-values ([(v next)  (next-uid "val" next)]     
;; 		[(v2 next) (next-uid "val" next)]     
;; 		[(t1 next) (next-uid "type1" next)]   
;; 		[(t11 next)(next-uid "type11" next)] 
;; 		[(t12 next) (next-uid "type12" next)] 
;; 		[(t2 next) (next-uid "type1" next)]   
;; 		[(t21 next) (next-uid "type11" next)] 
;; 		[(t22 next) (next-uid "type12" next)] 
;; 		[(l next) (next-uid "label" next)])    
;;     (let ([v-var   (Ann (Var v) ANY-VALUE)]
;; 	  [v2-var  (Ann (Var v2) ANY-VALUE)]  
;; 	  [t1-var  (Ann (Var t1) ANY-TYPE)]  
;; 	  [t11-var (Ann (Var t11) ANY-TYPE)]
;; 	  [t12-var (Ann (Var t12) ANY-TYPE)]
;; 	  [t2-var  (Ann (Var t2) ANY-TYPE)]  
;; 	  [t21-var (Ann (Var t21) ANY-TYPE)]
;; 	  [t22-var (Ann (Var t22) ANY-TYPE)]
;; 	  [l-var   (Ann (Var l) STRING-PTR)])
;;       (define-syntax-rule (lambda ((x t) ...) b)
;; 	(Ann (Lambda (list (Fml x t) ...) ANY-VALUE b ) INTERP-TYPE))
;;       (define-syntax-rule (let ((v e) ...) b)
;; 	(Ann (Let (list (Bnd v ANY-TYPE e) ...) b) ANY-VALUE))
;;       (define-syntax-rule (? p a ...)
;; 	(Ann (Op 'p (list a ...)) BOOL-TYPE))
;;       (define-syntax-rule (V p a ...)
;; 	(Ann (Op 'p (list a ...)) ANY-VALUE))
;;       (define-syntax-rule (T p a ...)
;; 	(Ann (Op 'p (list a ...)) ANY-TYPE))
;;       ;; cond is barely usable but it makes the code more compact
;;       (define-syntax-rule (if t c a)
;; 	(Ann (If t c a) ANY-VALUE))
;;     (define-syntax-rule (when t v)
;;       (Ann (If t v (Ann 'Blame (list l-var) BOTTOM-TYPE)) ANY-VALUE))
;;     (define-syntax cond
;;       (syntax-rules (else)
;; 	[(_ (else p e)) (when p e)]
;; 	[(_ (p r) c ...) (if p r (cond c ...))]))
;;     (define-syntax-rule (recur v t1 t2 l)
;;       (Ann (App (Var interp-var INTERP-TYPE) (list v t1 t2 l)) ANY-VALUE))
;;     (define-syntax-rule (app p a ...)
;;       (Ann (App p (list a ...)) ANY-VALUE))
;;     (define-syntax-rule (type t)
;;       (Ann (Quote (t)) ANY-TYPE))
;;     ;; Building the components
;;     (let* ([cast-int 
;; 	    : LF 
;; 	    (cond
;; 	     [(? Type:Int? t2-var) v-var]
;; 	     [else (? Type:Dyn? t2-var) 
;; 		   (V Dyn:Int v-var)])]
;; 	   [cast-bool 
;; 	    : LF
;; 	    (cond
;; 	     [(? Type:Bool? t2-var) v-var]
;; 	     [else (? Type:Dyn? t2-var) 
;; 		   (V Dyn:Bool v-var)])]
;; 	   [cast-dyn 
;; 	    : LF
;; 	    (cond
;; 	     [(? Type:Dyn? t2-var) v-var]
;; 	     [(? Dyn:Int? v-var) 
;; 	      (recur (V Dyn:Int! v-var) (type Int) t2-var l-var)]
;; 	     [(? Dyn:Bool? v-var)
;; 	      (recur (V Dyn:Bool! v-var) (type Bool) t2-var l-var)]
;; 	     [else (? Dyn:Fn? v-var)
;; 		   (recur (V Dyn:Fn! v-var) (T Dyn:Fn-type v-var) t2-var l-var)])]
;; 	   [cast-fn  
;; 	    : LF
;; 	    (cond
;; 	     [(? Type:Dyn? t2-var) (V Dyn:Fn v-var t1-var)]
;; 	     [else 
;; 	      (? Type:Fn? t2-var)
;; 	      (let ([t11 (T Type:Fn-arg t1-var)]
;; 		    [t12 (T Type:Fn-ret t1-var)]
;; 		    [t21 (T Type:Fn-arg t2-var)]
;; 		    [t22 (T Type:Fn-ret t2-var)])
;; 		(lambda ((v2 ANY-VALUE))
;; 		  (recur (app v-var (recur v2-var t21-var t11-var l-var))
;; 			 t12-var t22-var l-var)))])])
;;       (lambda ((v ANY-VALUE) (t1 ANY-TYPE) (t2 ANY-TYPE) (l STRING-PTR))
;; 	(cond
;; 	 [(? Type:Int? t1-var) cast-int]
;; 	 [(? Type:Bool? t1-var) cast-bool]
;; 	 [(? Type:Dyn? t1-var) cast-dyn]
;; 	 [else (? Type:Fn? t1-var) cast-fn]))))))

;; (: convert-casts (C0-Expr L0-Expr . -> . L0-Expr))
;; (define (convert-casts exp cast-interp-var)
;; (: cc-cast (L0-Expr Schml-Type Schml-Type String . -> . L0-Expr))
;;   (define (cc-cast e t1 t2 l)
;;     (let* ([t1 ;(type->expr t1)
;; 	    : L0-Expr (Ann (Quote DYN-TYPE ANY-TYPE))]
;; 	   [t2 ;(type->expr t2)
;; 	    : L0-Expr (Ann (Quote DYN-TYPE ANY-TYPE))]
;; 	   [lbl : L0-Expr (Ann (Quote l) STRING-PTR)]
;; 	   [args : (Listof L0-Expr) (list e t1 t2 l)]
;; 	   [rator : L0-Expr (Ann (Var (Uid "Do not use" 5)) INT-TYPE)])
;;       (Ann (App rator args) t2))
;;      ;; (Ann 
;;      ;; (App (Ann (Var (Uid "Do not use" 1) INT-TYPE)) ;; cast-interp-var 
;;      ;; 	  (list e (type->expr t1) (type->expr t2) (Ann (Quote l) STRING-PTR)))
;;      ;;t2
;;     )
  ;; This is kinda important because it is were types are converted
  ;; Values
  (: type->expr (Schml-Type . -> . L0-Expr))
  (define (type->expr t)
    (Ann (Quote t) ANY-TYPE))
(: recur (C0-Expr . -> . L0-Expr))
(define (recur exp)
  (let ((type (Ann-data exp)))
    (Ann 
     (match (Ann-value exp)
       [(Lambda f* t b) (Lambda f* t (recur b))]
       [(Letrec b* e) (Letrec (cata-bnd* b*) (recur e))]
       [(Let b* e) (Let (cata-bnd* b*) (recur e))]
       [(App e e*) (App (recur e) (recur* e*))]
       [(Op p e*) (Op p (recur* e*))]
       ;; [(Cast e t1 t2 l)		;(cc-cast (recur e) t1 t2 l)
  	;;(Ann (Quote "foof") INT-TYPE)
        ;;
       [(If t e1 e2) (If (recur t) (recur e1) (recur e2))]
       [(Var i) (Var i)]
       [(Quote k) (Quote k)])
     type)))
  
(: recur* ((Listof C0-Expr) . -> . (Listof L0-Expr)))
(define (recur* e*) (for/list ([e e*]) (recur e)))
(: cata-bnd* (C0-Bnd* . -> . L0-Bnd*))
(define (cata-bnd* b*) 
  (if (null? b*)
      '()
      (let ([b (car b*)])
	(cons (Bnd (Bnd-identifier b)
		   (Bnd-type b)
		   (recur (Bnd-expression b)))
	      (cata-bnd* (cdr b*))))))
;;   (recur exp))





    

