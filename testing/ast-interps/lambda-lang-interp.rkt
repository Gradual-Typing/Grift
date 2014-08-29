#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: testing/ast-interps/lambda-forms-interp                                  |
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
+------------------------------------------------------------------------------|#

(require Schml/framework/build-compiler
         Schml/framework/helpers
         Schml/framework/errors
	 Schml/testing/values
	 Schml/compiler/language)

(provide lambda-lang-interp)

(define-type LL-Value (U LL-Dyn LL-Fn Integer Boolean Schml-Type String))
(struct LL-Dyn ([value : LL-Value]
	     [type : Schml-Type]))
(struct LL-Fn ([value : (-> (Listof LL-Value) LL-Value)]))
(struct LL-Castable-Fn LL-Fn 
	([caster : LL-Value]))

(: ll-fn-apply (-> LL-Value (Listof LL-Value) LL-Value))
(define (ll-fn-apply fn arg*)
  (if (LL-Fn? fn)
      ((LL-Fn-value fn) arg*)
      (error 'apply "fn : ~a" fn)))


(: env-lookup (-> Env Uid LL-Value))
(define (env-lookup e u)
  (let ((ref (hash-ref e u (lambda () 
			     : (Boxof LL-Value) 
			     (error 'cfi "Unbound var ~a" u)))))
    (let ([val ((unbox ref))])
      (set-box! ref (lambda () : LL-Value val))
      val)))

(: env-extend* (-> Env (Listof Uid) (Listof LL-Value) Env))
(define (env-extend* env u* v*)
  (if (or (null? u*) (null? v*))
      env
      (env-extend* 
       ((inst hash-set Uid (Boxof (-> LL-Value)))
	env (car u*) (box (lambda () (car v*)))) 
       (cdr u*) 
       (cdr v*))))


(define-type Env (HashTable Uid (Boxof (-> LL-Value))))

(define-syntax-rule (empty-env)
  (hash))


(: lambda-lang-interp (-> Lambda0-Lang Config Test-Value))
(define (lambda-lang-interp prgm comp-config)
  (let ([observe observe-lazy])
    (match-let ([(Prog (list n c t) e) prgm])
      (observe (ll-expr ll-fn-apply) e (hash)))))

(: ll-expr (-> (-> LL-Value (Listof LL-Value) LL-Value)
	       (-> L0-Expr Env LL-Value)))
(define (ll-expr apply)
  (: recur (-> L0-Expr Env LL-Value))
  (define (recur exp env)
    (: recur/env (-> L0-Expr LL-Value))
    (define (recur/env e) (recur e env))
    (match exp
      [(Lambda id* _  (Castable ctr? body))
       (let ((clos (lambda ([arg* : (Listof LL-Value)]) 
		     (recur body (env-extend* env id* arg*)))))
	 (if ctr?
	     (LL-Castable-Fn clos (env-lookup env ctr?))
	     (LL-Fn clos)))]
      [(Letrec b* body) 
       (letrec ([rec-env : Env 
		 (foldr (lambda ([uid : Uid] [rhs : L0-Expr] [env : Env])
			  : Env
			  (hash-set env uid (box (lambda () : LL-Value (recur rhs rec-env)))))
			env 			    
			(map (inst car Uid L0-Expr) b*) 
			(map (inst cdr Uid L0-Expr) b*))])
	 (recur body rec-env))]
      [(Let b* body) 
       (recur body (env-extend* env 
				(map (inst car Uid L0-Expr)  b*) 
				(map (lambda ([b : L0-Bnd]) : LL-Value
				       (recur/env (cdr b))) b*)))]
      [(If (app recur/env tst) csq alt)
       (if tst (recur/env csq) (recur/env alt))]
      [(App e e*) (apply (recur/env e) (map recur/env e*))]
      ;; Special case were the Op needs env informations
      [(Op p e*) (delta p (map recur/env e*))]
      [(Var id) (env-lookup env id)]
      [(Quote k) k]
      [e (error 'll "Umatched expression ~a" e)]))
  recur)

(: delta (-> Symbol (Listof LL-Value) LL-Value))
(define (delta p v*)
  (define-syntax (app* stx)
    (syntax-case stx ()
      [(_ p t? ...)
       (with-syntax ([(tmp ...) (generate-temporaries #'(t? ...))])
	 #'(match-let ([(list tmp ...) v*])
	     (if (and (t? tmp) ...)
		 (p tmp ...)
		 (error 'delta "call ~a\nargs ~a" '(app* p t? ...) v*))))]))
  (define-syntax-rule (IxI p) (app* p integer? integer?))
  (define-syntax-rule (dyn-make v* t? t)
    (match-let ([(list v) v*])
      (if (t? v) (LL-Dyn v t) (error 'delta "dyn-make"))))
  (define-syntax-rule (dyn? v* t?)
    (match-let ([(list (LL-Dyn v t)) v*])
      (t? t)))
  (define-syntax-rule (dyn-value v* t?)
    (match-let ([(list (LL-Dyn v t)) v*])
      (if (t? t) v (error 'delta "dyn-value"))))
  (define-syntax-rule (app1 p)
    (match-let ([(list v) v*]) (p v)))
  (case p
    [(+) (IxI +)] 
    [(-) (IxI -)] 
    [(*) (IxI *)]
    [(=) (IxI =)] 
    [(<) (IxI <)] 
    [(>) (IxI >)] 
    [(>=) (IxI >=)] 
    [(<=) (IxI <=)]
    [(Dyn:Int-make) (dyn-make v* integer? INT-TYPE)]   
    [(Dyn:Int-value) (dyn-value v* Int?)]
    [(Dyn:Int?) (dyn? v* Int?)]
    [(Dyn:Bool-make) (dyn-make v* boolean? BOOL-TYPE)] 
    [(Dyn:Bool?) (dyn? v* Bool?)]
    [(Dyn:Bool-value) (dyn-value v* Bool?)]
    [(Dyn:Fn-make) (match-let ([(list f t) v*]) 
		     (if (and (LL-Fn? f) (Fn? t)) (LL-Dyn f t) (error 'delta "Fn-make")))]
    [(Dyn:Fn-value) (match-let ([(list (LL-Dyn f t)) v*]) f)]
    [(Dyn:Fn-type) (match-let ([(list (LL-Dyn f t)) v*]) t)]
    [(Dyn:Fn?)     (match-let ([(list (LL-Dyn f t)) v*]) (Fn? t))]
    [(Type:Int?)  (app1 Int?)]
    [(Type:Bool?) (app1 Bool?)]
    [(Type:Dyn?)  (app1 Dyn?)]
    [(Type:Fn?)   (app1 Fn?)]
    [(Type:Fn-arg-ref) 
     (app* (lambda ([t : (Fn Index (Listof Schml-Type) Schml-Type)] [n : Index]) 
	     (list-ref (Fn-fmls t) n))
	   Fn? index?)]
    [(Type:Fn-return)  
     (app* (lambda ([t : (Fn Index (Listof Schml-Type) Schml-Type)])
	     (Fn-ret t))
	   Fn?)]
    [(Type:Fn-arity) 
     (app* (lambda ([t : (Fn Index (Listof Schml-Type) Schml-Type)])
	     (Fn-arity t))
	   Fn?)]
    [(Fn-cast) (app* LL-Castable-Fn-caster LL-Castable-Fn?)]
    [(Blame) 
     (match-let ([(list l) v*])
       (if (string? l)
	   (raise (exn:schml:type:dynamic l (current-continuation-marks)))
	   (error 'delta "recieve non string label")))]
    [else (error 'delta "~a" p)]))


(: observe-lazy (-> (-> L0-Expr Env LL-Value) L0-Expr Env Test-Value))
(define (observe-lazy interp exp env)
  (with-handlers ([exn:schml:type:dynamic? 
		   (lambda ([e : exn:schml:type:dynamic]) 
		     (blame #f (exn-message e)))])
    (let ([v (interp exp env)])
      (cond 
       [(integer? v) (integ v)]
       [(boolean? v) (boole v)]
       [(LL-Fn? v) (function)]
       [(LL-Dyn? v) (dynamic)]
       [else  (error 'observe)]))))

