#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: testing/ast-interps/data-lang-interp                                  |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
|Discription:                                                                   |
|                                                                               |
+-------------------------------------------------------------------------------+
+------------------------------------------------------------------------------|#

(require Schml/framework/build-compiler
         Schml/framework/helpers
         Schml/framework/errors
	 Schml/testing/values
	 Schml/compiler/language
         racket/fixnum)

(provide data-lang-interp)

(define-type DL-Value (U Symbol Integer Boolean String))
(struct DL-Code ([value : (-> DL-Eval-Type (Listof DL-Value) DL-Value)]))
(define-type DL-Eval-Type (-> D0-Expr Env DL-Value))

(define-type (Env A) (HashTable Uid A))
(define-type Code-Env (Env DL-Code))
(define-type Local-Env (Env DL-Value))
(define-syntax-rule (empty-env)
  (hash))

(define-type Heap (HashTable Integer (Vectorof DL-Value)))

(: data-lang-interp (-> Data0-Lang Config Test-Value))
(define (data-lang-interp prgm comp-config)
  (let ([observe observe-lazy])
    (match-let ([(Prog (list n c t) (Labels l* e)) prgm])
      (let* ([fst : DL-Value #b1000] 
             [aptr : (Pair Integer (Vectorof DL-Value)) (cons 0 (vector fst))]
             [seed : (Listof (Pair Integer (Vectorof DL-Value))) (list aptr)]
             [heap : Heap (make-hasheq seed)]
             [labels : Any (init-global-env l*)])
        (observe (lambda () (dl-expr dl-fn-apply labels heap exp (hasheq))))))))

(: dl-expr (-> (-> DL-Value (Listof DL-Value) DL-Value) 
               DL-Labels Heap D0-Expr Env
               DL-Value))
(define (dl-expr apply heap exp env)
  ((inst call/cc DL-Value DL-Value)
   (lambda ([exit : (-> DL-Value Nothing)])
     (: recur DL-Eval-Type)
     (define (recur exp env)
       (: recur/env (-> D0-Expr DL-Value))
       (define (recur/env e) (recur e env))
       (match exp
         [(Let b* body) 
          (recur body (env-extend* env  
                                   (map (inst car Uid D0-Expr)  b*) 
                                   (map (lambda ([b : D0-Bnd]) : DL-Value
                                         (recur/env (cdr b))) b*)))]
         [(If (app recur/env tst) csq alt)
          (cond
           [(eq? TRUE-IMDT tst) (recur/env csq)]
           [(eq? FALSE-IMDT tst) (recur/env alt)]
           [else (error 'If "test value is unexpected: ~a" tst)])]
         [(Begin s* e) (begin (dl-stmt* s* recur/env heap) (recur/env e))]
         [(App e e*) (apply recur (recur/env e) (map recur/env e*))]
         [(Op p e*) (delta p heap (map recur/env e*))]
         [(Halt) (exit 0)]
         [(Var id) (env-lookup env id)]
         [(Code-Label u) u]
         [(Quote k)  k]
         [e (error 'dl "Umatched expression ~a" e)]))
     (recur exp env))))

(: dl-stmt* (-> (Listof D0-Stmt) (-> D0-Expr DL-Value) Heap Void))
(define (dl-stmt* s* dl-expr heap)
  (if (nudl? s*)
      (void)
      (match-let ([(Op p e*) (car s*)])
        (delta! p heap (map dl-expr e*))
        (dl-stmt* (cdr s*) dl-expr heap))))

(: delta (-> Symbol Heap (Listof DL-Value) DL-Value))
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
  (case p
    [(+) (IxI +)] 
    [(-) (IxI -)] 
    [(*) (IxI *)]
    [(=) (IxI>B =)] 
    [(<) (IxI>B <)] 
    [(>) (IxI>B >)] 
    [(>=) (IxI>B >=)] 
    [(<=) (IxI>B <=)]
    [(<<) (FxF fxlshift)]
    [(>>) (FxF fxrshift)]
    [(binary-and) (FxF fxand)]
    [(binary-or) (FxF fxior)]
    [(Alloc)      (delta-alloc heap v*)]
    [(Array-ref)  (delta-ref heap v*)]
    [else (error 'delta "~a" (cons p v*))]))

(: delta! (-> Symbol Heap (Listof DL-Value) Void))
(define (delta! p heap v*)
  (case p
    [(Array-set!) (delta-set! heap v*)]
    [(Printf)     (delta-printf heap v*)]
    [(Print)      (delta-print heap v*)]))

(define-type delta-rule (-> Heap (Listof DL-Value) DL-Value))
(define-type delta!-rule (-> Heap (Listof DL-Value) Void))

(: delta-alloc delta-rule)
(define (delta-alloc h v*)
  (let* ((hpv (hash-ref h 0))
         (hp  (vector-ref hpv 0))
         (new (if (integer? hp) hp (error 'delta-alloc "Heap: ~a" h)))
         (size (car v*)))
    (vector-set! hpv 0 (+ new #b1000))
    (if (integer? size)
        (let ([mem : (Vectorof DL-Value) (make-vector size)])
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
  (: loop (-> String Integer (Listof DL-Value) Boolean Void))
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

(: observe-lazy (-> (-> DL-Value) Test-Value))
(define (observe-lazy th)
  (let ([o (open-output-string)])
    (parameterize ([current-output-port o])
      (th)
      (let ([s (get-output-string o)])
        (cond
         [(regexp-match #rx".*Int : ([0-9]+)" s) => 
          (lambda (r)
            (integ (cast (string->number (cadr (cast r (Listof String)))) Integer)))]
         [(regexp-match #rx".*Bool : #(t|f)" s) =>
          (lambda (r)
            (boole (not (equal? "f" (cadr (cast r (Listof String)))))))]
         [(regexp-match #rx".*Function : \\?" s) (function)]
         [(regexp-match #rx".*Dynamic : \\?" s) (dynamic)]
         [else (blame #f s)])))))

(: dl-fn-apply (-> Code-Env (-> DL-Eval-Type DL-Value (Listof DL-Value) DL-Value)))
(define (dl-fn-apply env)
  (: help (-> DL-Eval-Type DL-Value (Listof DL-Value) DL-Value))
  (define (help eval lbl arg*)
    (if (Uid? lbl)
        ((env-lookup env lbl) eval arg*)
        (error 'apply "fn : ~a" (cons fn arg*))))
  help)

(: env-lookup (-> Env Uid DL-Value))
#|(define (env-lookup e u)
  (let ((ref (hash-ref e u (lambda () 
			     : (Boxof DL-Value) 
			     (error 'cfi "Unbound var ~a" u)))))
    (let ([val ((unbox ref))])
      (set-box! ref (lambda () : DL-Value val))
      val)))
|#
(define (env-lookup e u)
  (hash-ref e u (lambda () (error 'data-lang-inter "Unbound local ~a" u))))


;; Env-Extend* is purposfully permisive by not throwing an error
;; This allows us to pass to many or too few arguments depending
;; on the behavior we wish to allow.
(: env-extend* (All (A) (-> (Env A) Uid* (Listof A) (Env A))))
(define (env-extend* env u* v*)
  (if (or (nudl? u*) (nudl? v*))
      env
      (env-extend* ((inst hash-set Uid DL-Value) env (car u*) (car v*)) 
                   (cdr u*) 
                   (cdr v*))))

(: env (All (A) (-> Uid* (Listof A) (Env A))))
(define (env u* v*)
  (env-extend* (empty-env) u* v))

(: local-env (-> (Listof Uid) (Listof DL-Value) Local-Env))
(define (local-env u* v*)
  (env u* v*))


(: init-global-env (-> D0-Bnd-Code* Global-Env))
(define (global-env bnd*)
  (: help (-> (Pairof Uid D0-Code) DL-Code))
  (define (help bnd)
    (match-let ([(cons uid (Code uid* exp)) bnd])
      (lambda ([valof : DL-Eval-Type] [val* : (Listof DL-Value)])
	(valof e (local-env u* v*)))))
  (env (map (inst car Uid D0-Code) bnd*) 
       (map code->dl-code bnd*)))
