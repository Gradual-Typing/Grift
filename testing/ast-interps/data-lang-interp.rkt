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

(require schml/framework/build-compiler
         schml/framework/helpers
         schml/framework/errors
	 schml/testing/values
	 schml/compiler/language
         racket/fixnum)

(provide data-lang-interp)

(define-type DL-Value (U Uid Integer Boolean String))
(define-type DL-Code (-> DL-Eval-Type (Listof DL-Value) DL-Value))
(define-type DL-Eval-Type (-> D0-Expr Local-Env DL-Value))

(define-type (Env A) (HashTable Uid A))
(define-type Code-Env  (Env DL-Code))
(define-type Local-Env (Env DL-Value))
(define-type Heap-Env  (HashTable Integer (Vectorof DL-Value)))

(: data-lang-interp (-> Data0-Lang Config Test-Value))
(define (data-lang-interp prgm comp-config)
  (let ([observe observe-lazy])
    (match-let ([(Prog (list n c t) (Labels code-labels exp)) prgm])
      (let* ([heap   : Heap-Env (heap-env)]
             [code   : Code-Env (code-env code-labels)]
             [locals : Local-Env (env)])
        (log "prog: \n\n~a\n" n)
        (log "labels: ~a\nmain: ~a\n" code-labels exp)
        (observe (lambda () (dl-expr (dl-fn-apply code) heap exp locals)))))))

(: dl-expr (-> (-> DL-Eval-Type DL-Value (Listof DL-Value) DL-Value) 
               Heap-Env D0-Expr Local-Env
               DL-Value))
(: log (->* (String) #:rest Any Void))
(define log 
  (let ((l (open-output-file "log.txt" #:exists 'replace)))
    (lambda (fmt . a)
      (apply fprintf l fmt a))))

(define (dl-expr apply heap exp env)
  ;; This is the exit implementation for this interpreter
  ((inst call/cc DL-Value DL-Value)
   (lambda ([exit : (-> DL-Value Nothing)])
     (: recur DL-Eval-Type)
     (define (recur exp env)
       (: recur/env (-> D0-Expr DL-Value))
       (define (recur/env e) (recur e env))
       (log "dl-expr: ~a\n" exp)
       (match exp
         [(Let b* body)
          (let ([id*  (map (inst car Uid D0-Expr)  b*)]
                [rhs* (map (lambda ([b : D0-Bnd]) : DL-Value
                                   (recur/env (cdr b))) 
                           b*)])
            (recur body (env-extend* env id* rhs*)))]
         [(If (app recur/env tst) csq alt)
          (cond
           [(eq? TRUE-IMDT tst) (recur/env csq)]
           [(eq? FALSE-IMDT tst) (recur/env alt)]
           [else (error 'If "test value is unexpected: ~a" tst)])]
         [(Begin s* e) (begin (dl-stmt* s* recur/env heap exit) (recur/env e))]
         [(App e e*) 
          (let ([rator (recur/env e)] 
                [rand (map recur/env e*)])
            (log "Applying ~a\n" (cons rator rand))
            (apply recur  rator rand))]
         [(Op p e*) (delta p heap (map recur/env e*))]
         [(Halt) (exit 0)]
         [(Var id) (env-lookup env id)]
         [(Code-Label u) u]
         [(Quote k)  k]
         [e (error 'dl "Umatched expression ~a" e)]))
     (recur exp env))))

(: dl-stmt* (-> (Listof D0-Stmt) (-> D0-Expr DL-Value) Heap-Env (-> DL-Value Nothing) Void))
(define (dl-stmt* s* dl-expr heap exit)
  (if (null? s*)
      (void)
      (match-let ([(Op p e*) (car s*)])
        (delta! p heap (map dl-expr e*) exit)
        (dl-stmt* (cdr s*) dl-expr heap exit))))

(: delta (-> Symbol Heap-Env (Listof DL-Value) DL-Value))
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

(: delta! (-> Symbol Heap-Env (Listof DL-Value) (-> DL-Value Nothing) Void))
(define (delta! p heap v* exit)
  (case p
    [(Array-set!) (delta-set! heap v*)]
    [(Printf)     (delta-printf heap v*)]
    [(Print)      (delta-print heap v*)]
    [(Exit)       (exit -1)]))

(define-type delta-rule (-> Heap-Env (Listof DL-Value) DL-Value))
(define-type delta!-rule (-> Heap-Env (Listof DL-Value) Void))

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
  (log "ref: ~a \n\t~a\n" v* h)
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
        (error 'apply "~a" (cons lbl arg*))))
  help)

(: env-lookup (All (A) (-> (Env A) Uid A)))
(define (env-lookup e u)
  (hash-ref e u (lambda () (error 'data-lang-inter "Unbound ~a" u))))

;; Env-Extend* is purposfully permisive by not throwing an error
;; This allows us to pass to many or too few arguments depending
;; on the behavior we wish to allow.
(: env-extend* (All (A) (-> (Env A) Uid* (Listof A) (Env A))))
(define (env-extend* env u* v*)
  (if (or (null? u*) (null? v*))
      env
      (env-extend* (hash-set env (car u*) (car v*)) 
                   (cdr u*) 
                   (cdr v*))))

#|
Type Checker: Could not infer types for applying polymorphic function `hasheq'
  (: env (All (A) (-> Uid* (Listof A) (Env A))))
  (define (env u* v*) (env-extend* (hasheq) u* v*))
|#

(define-syntax env
  (syntax-rules ()
    [(_ t u* v*) (let ([p : t (hasheq)]) (env-extend* p u* v*))]
    [(_) (hasheq)]))

(: local-env (-> (Listof Uid) (Listof DL-Value) Local-Env))
(define (local-env u* v*)
  (env Local-Env u* v*))


;; Code-env folds over code-label bindings creating a code enviroment
;; that maps a symbol to function.
(: code-env (-> D0-Bnd-Code* Code-Env))
(define (code-env bnd*)
  (: help (-> (Pairof Uid D0-Code) DL-Code))
  (define (help bnd)
    (match-let ([(cons uid (Code u* e)) bnd])
      (lambda ([eval : DL-Eval-Type] [v* : (Listof DL-Value)])
	(eval e (env Local-Env u* v*)))))
  (env Code-Env (map (inst car Uid D0-Code) bnd*) (map help bnd*)))

;; Creates an empty heap
(: heap-env (-> Heap-Env))
(define (heap-env)
  ;; The let* here is mostly to provide type annotations for typed racket
  (let* ([fst : DL-Value #b1000] 
         [aptr : (Pair Integer (Vectorof DL-Value)) (cons 0 (vector fst))]
         [seed : (Listof (Pair Integer (Vectorof DL-Value))) (list aptr)])
    (make-hasheq seed)))
