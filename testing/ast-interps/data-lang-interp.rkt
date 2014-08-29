#lang typed/racket

(require Schml/framework/build-compiler
         Schml/framework/helpers
         Schml/framework/errors
	 Schml/testing/values
	 Schml/compiler/language)

(provide data-lang-interp)



(define-type DL-Value 
  (U DL-Dyn DL-Clos DL-Code Integer Boolean Schml-Type String))

(struct DL-Dyn 
  ([value : DL-Value] 
   [type : Schml-Type])
  #:transparent)


(define-type DL-Clos (Vectorof DL-Value))
(struct DL-Code 
  ([name : Uid]
   [addr : (-> ValOf-Type (Listof DL-Value) DL-Value)])
  #:transparent)

(define-type Env (HashTable Uid DL-Value))
(define-type ValOf-Type (-> D0-Expr Env DL-Value))


(: data-lang-interp (-> Data0-Lang Config Test-Value))
(define (data-lang-interp prgm comp-config)
  (let ([observe observe-lazy])
    (print prgm) (newline) (newline)
    (match-let ([(Prog (list n c t) (Labels l* e)) prgm])
      (observe (dl-expr dl-fn-apply (init-global-env l*)) e (hash)))))

(: dl-expr (-> (-> ValOf-Type DL-Value (Listof DL-Value) DL-Value) 
	       Env
	       (-> D0-Expr Env DL-Value)))
(define (dl-expr apply global-env)
  (: recur (-> D0-Expr Env DL-Value))
  (define (recur exp local-env)
    (: recur/env (-> D0-Expr DL-Value))
    (define (recur/env e) (recur e local-env))
    (print exp) (newline) (newline)
    (match exp
      [(Let b* body) 
       (recur body (env-extend* local-env 
    				(map (inst car Uid D0-Expr)  b*) 
    				(map (lambda ([b : D0-Bnd]) : DL-Value
    					     (recur/env (cdr b))) 
    				     b*)))]
      [(App e e*) (apply recur (recur/env e) (map recur/env e*))]
      [(Op p e*) (delta p (map recur/env e*))]
      [(If (app recur/env tst) csq alt)
       (if (boolean? tst)
    	   (if tst (recur/env csq) (recur/env alt))
    	   (error 'dli "if test returns non boolean"))]
      [(Begin e* e) (begin (dl-effect* e* recur/env) (recur/env e))]
      [(Code-Label id) (env-lookup global-env id)]
      [(Var id) (env-lookup local-env id)]
      [(Quote k) k]
      [e (error 'll "Umatched expression ~a" e)])
    )
  recur)

(: dl-effect* (-> (Listof D0-Effect) (-> D0-Expr DL-Value) Void))
(define (dl-effect* e* recur)
  (for-each (dl-effect recur) e*))

(: dl-effect (-> (-> D0-Expr DL-Value) (-> D0-Effect Void)))
(define (dl-effect recur)
  (lambda ([e : D0-Effect])
    (match e
      [(Op p e*) (delta! p (map recur e*))])))

(: delta (-> Symbol (Listof DL-Value) DL-Value))
(define (delta p v*)
  (define-syntax-rule (IxI p v1 v2) 
    (if (and (integer? v1) (integer? v2))
	(p v1 v2)
	(delta-error)))
  (define-syntax-rule (dyn-make v t t?)
    (if (t? v) (DL-Dyn v t) (delta-error)))
  (define-syntax-rule (delta-error) (error 'delta "~a" (cons p v*)))
(match* (p v*)
   [('+ (list v v^)) (IxI + v v^)] 
   [('- (list v v^)) (IxI - v v^)] 
   [('* (list v v^)) (IxI * v v^)]
   [('= (list v v^)) (IxI = v v^)] 
   [('< (list v v^)) (IxI < v v^)] 
   [('> (list v v^)) (IxI > v v^)] 
   [('>= (list v v^)) (IxI >= v v^)] 
   [('<= (list v v^)) (IxI <= v v^)]
   [((or 'Dyn:Int-value 'Dyn:Bool-value 'Dyn:Fn-value) 
     (list (DL-Dyn v _))) v]
   [('Dyn:Int-make (list v)) (dyn-make v INT-TYPE integer?)]
   [('Dyn:Bool-make (list v)) (dyn-make v BOOL-TYPE boolean?)]
   [('Dyn:Fn-make _)
    (if (= (length v*) 2)
	(let ((f (car v*))
	      (t (cadr v*)))
	  (if (schml-type? t)
	      (DL-Dyn f t)
	      (delta-error)))
	(delta-error))]
   [('Dyn:Int? (list (DL-Dyn _ t))) (Int? t)]
   [('Dyn:Bool? (list (DL-Dyn _ t))) (Bool? t)]
   [('Dyn:Fn? (list (DL-Dyn _ t))) (Fn? t)]
   [('Dyn:Fn-type (list (DL-Dyn _ t))) t]
   [('Type:Int? (list v)) (Int? v)]
   [('Type:Bool? (list v)) (Bool? v)]
   [('Type:Dyn? (list v)) (Dyn? v)]
   [('Type:Fn? (list v))  (Fn? v)]
   [('Type:Fn-arg-ref _)
    (if (= 2 (length v*))
	(let ([t (car v*)]
	      [i (cadr v*)])
	  (if (and  (Fn? t) (index? i))
	      (let ((fmls (Fn-fmls t)))
		(if (list? fmls)
		    (let ((t (list-ref fmls i)))
		      (if (schml-type? t)
			  t
			  (delta-error)))
		    (delta-error)))
	      (delta-error)))
	(delta-error))]
   [('Type:Fn-return _)
    (if (= 1 (length v*))
	(let ((t (car v*)))
	  (if (Fn? t)
	      (let ((t (Fn-ret t)))
		(if (schml-type? t)
		    t
		    (delta-error)))
	      (delta-error)))
	(delta-error))]
   [('Type:Fn-arity (list (Fn a _ _)))
    (if (index? a) a (delta-error))]
   [('Blame (list l))
    (if (string? l)
 	(raise (exn:schml:type:dynamic l (current-continuation-marks)))
 	(delta-error))]
   [('Clos:make (list v))
    (if (index? v) ((inst make-vector DL-Value) v) (delta-error))]
   [('Clos:ref (list v i))
    (if (and (vector? v) (index? i))
 	(vector-ref v i)
 	(delta-error))]
   [(_ _) (delta-error)]))

(: delta! (-> Symbol (Listof DL-Value) Void))
(define (delta! p v*)
  (define-syntax-rule (delta-error) (error 'delta "~a" (cons p v*)))
  (match* (p v*)
   [('Clos:set! (list c i v)) 
    (if (and (vector? c) (index? i))
	(vector-set! c i v)
	(delta-error))]))

(: observe-lazy (-> (-> D0-Expr Env DL-Value) D0-Expr Env Test-Value))
(define (observe-lazy interp exp env)
  (with-handlers ([exn:schml:type:dynamic? 
		   (lambda ([e : exn:schml:type:dynamic]) 
		     (blame #f (exn-message e)))])
    (let ([v : DL-Value (interp exp env)])
      (integ 1)
 (cond 
  [(integer? v) (integ v)]
  [(boolean? v) (boole v)]
  [(vector? v) (function)]
  ;; this was tricky but now casted closures are closures
  [(DL-Dyn? v) (dynamic)] 
  [else  (error 'observe)])
 )))

(: dl-fn-apply (-> ValOf-Type DL-Value (Listof DL-Value) DL-Value))
(define (dl-fn-apply valof code arg*)
  (if (DL-Code? code)
      ((DL-Code-addr code) valof arg*)
      (error 'apply "code : ~a" code)))

(: env-lookup (-> Env Uid DL-Value))
(define (env-lookup env uid)
  (let ((v (hash-ref env uid (lambda () (error 'dfi "Unbound id ~a" uid)))))
    (printf "~a -> ~a\n\n" uid v)
    v))

(: env-extend (-> Uid DL-Value Env Env))
(define (env-extend uid val env)
  (hash-set env uid val))

(: env-extend* (-> Env Uid* (Listof DL-Value) Env))
(define (env-extend* env uid* val*)
  (fold-2-left env-extend env uid* val*))

(: env (-> Uid* (Listof DL-Value) Env))
(define (env u* v*)
  (env-extend* (hash) u* v*))

(: init-global-env (-> D0-Bnd-Code* Env))
(define (init-global-env l*)
  (: code->dl-code (-> (Pairof Uid D0-Code) DL-Code))
  (define (code->dl-code p)
    (match-let ([(cons uid (Code u* e)) p])
      (DL-Code uid
       (lambda ([valof : ValOf-Type] 
		[v* : (Listof DL-Value)])
	 (valof e (env u* v*))))))
  (env (map (inst car Uid D0-Code) l*) 
       (map code->dl-code l*)))

