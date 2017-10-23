#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: testing/ast-interps/lambda-forms-interp                                  |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
|Discription:                                                                   |
|                                                                               |
+-------------------------------------------------------------------------------+
+------------------------------------------------------------------------------|#

(require grift/src/helpers
         grift/src/errors
	 grift/testing/values
	 grift/src/language
         racket/fixnum)

(provide lambda-lang-interp)


(define-type LL-Value (U LL-Fn Integer Boolean String))
(struct LL-Fn ([value : (-> (Listof LL-Value) LL-Value)]))
(struct LL-Castable-Fn LL-Fn ([caster : LL-Value]))

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

(define-type Heap (HashTable Integer (Vectorof LL-Value)))


(: lambda-lang-interp (-> Lambda0-Lang Config Test-Value))
(define (lambda-lang-interp prgm comp-config)
  (let ([observe observe-lazy])
    (match-let ([(Prog (list n c t) exp) prgm])
      (let* ([fst : LL-Value #b1000] 
             [aptr : (Pair Integer (Vectorof LL-Value)) (cons 0 (vector fst))]
             [seed : (Listof (Pair Integer (Vectorof LL-Value))) (list aptr)]
             [heap : Heap (make-hasheq seed)])
        (observe (lambda () (ll-expr ll-fn-apply heap exp (hasheq))))))))

(: ll-expr (-> (-> LL-Value (Listof LL-Value) LL-Value) 
               Heap L0-Expr Env
               LL-Value))
(define (ll-expr apply heap exp env)
  ((inst call/cc LL-Value LL-Value)
   (lambda ([exit : (-> LL-Value Nothing)])
     (: recur (-> L0-Expr Env LL-Value))
     (define (recur exp env)
       (: recur/env (-> L0-Expr LL-Value))
       (define (recur/env e) (recur e env))
       (when (trace? 'Vomit)
         (logf "ll-expr:\n~v\n\n" exp))
       (match exp
         [(Lambda id* _  (Castable ctr? body))
          (let ((clos (lambda ([arg* : (Listof LL-Value)]) 
                        (recur body (env-extend* env id* arg*)))))
            (if ctr?
                (LL-Castable-Fn clos (env-lookup env ctr?))
	     (LL-Fn clos)))]
         [(Fn-Caster (app recur/env e)) 
          (if (LL-Castable-Fn? e) 
              (LL-Castable-Fn-caster e)
              (error 'Fn-Caster "Tried to extract caster from non-castble"))]
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
       (cond
        [(eq? TRUE-IMDT tst) (recur/env csq)]
        [(eq? FALSE-IMDT tst) (recur/env alt)]
        [else (error 'If "test value is unexpected: ~a" tst)])]
      [(Begin s* e) (begin (ll-stmt* s* recur/env heap exit) (recur/env e))]
      [(App e e*) (apply (recur/env e) (map recur/env e*))]
      [(Op p e*) (delta p heap (map recur/env e*))]
      [(Halt) (exit 0)]
      [(Var id) (env-lookup env id)]
      [(Quote k)  k]
      [e (error 'll "Umatched expression ~a" e)]))
     (recur exp env))))


(: ll-stmt* (-> (Listof L0-Stmt) (-> L0-Expr LL-Value) Heap (-> LL-Value Nothing) Void))
(define (ll-stmt* s* ll-expr heap exit)
  (if (null? s*)
      (void)
      (match-let ([(Op p e*) (car s*)])
        (delta! p heap (map ll-expr e*) exit)
        (ll-stmt* (cdr s*) ll-expr heap exit))))

(: delta (-> Symbol Heap (Listof LL-Value) LL-Value))
(define (delta p heap v*)
  (define-syntax (app* stx)
    (syntax-case stx ()
      [(_ p t? ...)
       (with-syntax ([(tmp ...) (generate-temporaries #'(t? ...))])
	 #'(match v* 
             [(list tmp ...)
              (if (and (t? tmp) ...)
		 (p tmp ...)
		 (error 'delta "call ~a\nargs ~a" '(app* p t? ...) v*))]
             [otherwise (error 'delta "call ~a\nargs ~a" '(app* p t? ...) v*)]))]))
  (define-syntax-rule (IxI p) (app* p integer? integer?))
  (define-syntax-rule (FxF p) (app* p fixnum? fixnum?))
  (define-syntax-rule (IxI>B p) (IxI (lambda (n m) (if (p n m) TRUE-IMDT FALSE-IMDT))))
  (when (trace? 'vomit)
    (logf "ll-delta ~v" (cons p v*)))
  (case p
    [(+) (IxI +)] 
    [(-) (IxI -)] 
    [(*) (IxI *)]
    [(=) (IxI>B =)] 
    [(<) (IxI>B <)] 
    [(>) (IxI>B >)] 
    [(>=) (IxI>B >=)] 
    [(<=) (IxI>B <=)]
    [(%/) (IxI quotient)]
    [(%<<) (FxF fxlshift)]
    [(%>>) (FxF fxrshift)]
    [(binary-and) (FxF fxand)]
    [(binary-or) (FxF fxior)]
    [(Alloc)      (delta-alloc heap v*)]
    [(Array-ref)  (delta-ref heap v*)]
    [else (error 'delta "~a" (cons p v*))]))

(: delta! (-> Symbol Heap (Listof LL-Value) (-> LL-Value Nothing) Void))
(define (delta! p heap v* exit)
  (when (trace? 'Vomit)
    (logf "ll-delta!\n~v\nwith heap ~v\n\n" (cons p v*) heap))
  (case p
    [(Array-set!) (delta-set! heap v*)]
    [(Printf)     (delta-printf heap v*)]
    [(Print)      (delta-print heap v*)]
    [(Exit)       (exit -1)]))
    
(define-type delta-rule (-> Heap (Listof LL-Value) LL-Value))
(define-type delta!-rule (-> Heap (Listof LL-Value) Void))

(: delta-alloc delta-rule)
(define (delta-alloc h v*)
  (let* ((hpv (hash-ref h 0))
         (hp  (vector-ref hpv 0))
         (new (if (integer? hp) hp (error 'delta-alloc "Heap: ~a" h)))
         (size (car v*)))
    (vector-set! hpv 0 (+ new #b1000))
    (if (integer? size)
        (let ([mem : (Vectorof LL-Value) (make-vector size)])
          (hash-set! h new mem)
          new)
        
        (error 'delta-alloc "~a" (cons 'alloc v*)))))

(: delta-ref delta-rule)
(define (delta-ref h v*)
  (match v*
    [(list ptr index)
     (if (and (integer? ptr) (index? index))
         (vector-ref (hash-ref h ptr) index)
         (error 'delta-alloc "~a" (cons 'array-ref v*)))]
    [otherwise (error 'delta-alloc "~a" (cons 'array-ref v*))]))

(: delta-set! delta!-rule)
(define (delta-set! h v*)
  (match v*
    [(list ptr index value)
     (if (and (integer? ptr) (index? index))
         (vector-set! (hash-ref h ptr) index value)
         (error 'delta-alloc "~a" (cons 'array-ref v*)))]
    [otherwise (error 'delta-alloc "~a" (cons 'array-ref v*))]))

(: delta-printf delta!-rule)
(define (delta-printf h v*)
  (: loop (-> String Integer (Listof LL-Value) Boolean Void))
  (define (loop s i a e?)
    (cond
     [(or (>= i (string-length s)) (not (index? i)))  (void)]
     [e?
      (case (string-ref s i)
        [(#\d) (display (car a))]
        [else  (print '<?>)])
      (loop s (add1 i) (cdr a) #f)]
     [else 
      (let ((c (string-ref s i)))
        (case c
          [(#\%) (loop s (add1 i) a #t)]
          [else (display c) (loop s (add1 i) a #f)]))]))
  (let ((fmt (car v*)))
    (if (string? fmt)
        (loop fmt 0 (cdr v*) #f)
        (error 'delta-printf "~a" (cons 'printf v*)))))


(: delta-print delta!-rule)
(define (delta-print h v*)
  (match v*
    [(list s)
     (if (string? s)
         (display s)
         (error 'delta-print "~a" (cons 'print v*)))]
    [otherwise (error 'delta-print "~a" (cons 'print v*))]))

(: observe-lazy (-> (-> LL-Value) Test-Value))
(define (observe-lazy th)
  (let ([o (open-output-string)])
      (parameterize ([current-output-port o])
        (th)
        (let ([s (get-output-string o)])
          (cond
           [(regexp-match #rx".*Int : ([0-9]+)" s) => 
            (lambda (r)
              (int (cast (string->number (cadr (cast r (Listof String)))) Integer)))]
           [(regexp-match #rx".*Bool : #(t|f)" s) =>
            (lambda (r)
              (bool (not (equal? "f" (cadr (cast r (Listof String)))))))]
           [(regexp-match #rx".*Function : \\?" s) (function)]
           [(regexp-match #rx".*Dynamic : \\?" s) (dyn)]
           [else (blame #f s)])))))

