#lang typed/racket/base
#|------------------------------------------------------------------------------+
|Pass: src/schml/parse                                                          |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
|Discription: The parse pass takes racket's native syntax objects to a set of   |
|core structures that are annotated with the source location.                   |
+-------------------------------------------------------------------------------+
+------------------------------------------------------------------------------|#

(require racket/match
         racket/list
         "../helpers.rkt"
         "../errors.rkt"
         "../configuration.rkt"
         "../language/schml0.rkt")

(require typed/syntax/stx)

(if-in-construction (require typed/rackunit))

(provide (all-from-out "../language/schml0.rkt")
         syntax->schml0
         parse-type)

(: syntax->schml0 (Syntax-Lang . -> . Schml0-Lang))
(define (syntax->schml0 prgm)
  (match-let ([(Prog name stx*) prgm])
    (let-values
	([((exp : S0-Expr) (next : Natural))
          (run-state (parse-top-level name stx* (hasheq)) FIRST-UID-SUFFIX)])
      (Prog (list name next) exp))))

#| The type of the enviroment bindings and symbol-sets|#
(define-type Env (HashTable Symbol Uid))
(: env-lookup (Env Symbol Src . -> . Uid))
(define (env-lookup e s l)
  (hash-ref e s (lambda () (raise-unbound-variable-exn s l))))

(: env-extend (Env Symbol Uid . -> . Env))
(define env-extend hash-set)

#| pattern helpers |#
(: seq? (Stx Symbol . -> . Boolean))
(define (seq? s p) (eq? (syntax-e s) p))

(: colon? (Stx . -> . Boolean))
(define (colon? x) (seq? x ':))

(: arrow? (Stx . -> . Boolean))
(define (arrow? x) (seq? x '->))

#| The type of source locations and syntax helpers|#

(: syntax-unroll (Stx . -> . (U Symbol String Schml-Literal (Listof Stx))))
(define (syntax-unroll stx)
  (let ((a (syntax-e stx)))
    (if ((make-predicate (U Symbol Schml-Literal String (Listof Stx)))
	 a)
	a
	(raise-unsupported-syntax-exn stx))))

(: reserved-symbol? (Symbol . -> . Boolean))
(define (reserved-symbol? x)
  (case x
    [(lambda letrec let if :) #t]
    [(gbox gunbox gbox-set! gvector gvector-ref gvector-set!) #t]
    [(mbox munbox mbox-set! mvector mvector-ref mvector-set!) #t]
    [(tuple tuple-proj) #t]
    [(+ - * binary-and binary-or < <= = >= > %/ %<< %>>) #t]
    [(timer-start timer-stop timer-report repeat) #t]
    [(read-int) #t]
    [else #f]))

(: parse-top-level (String (Listof Stx) Env . -> . (State Natural S0-Expr)))
(define (parse-top-level name stx* env) : (State Natural S0-Expr)
  (cond
    [(null? stx*) (return-state (raise-file-empty-exn name))]
    [(not (null? (cdr stx*))) (return-state (raise-<1-exp-exn name))]
    [else (parse-expr (car stx*) env #f)]))

#| The parser |#
(: parse-expr (Stx Env (U False Schml-Type) . -> . (State Natural S0-Expr)))
(define (parse-expr stx env bound-type)
  (lambda ((next : Natural))
    (let ((src (syntax->srcloc stx))
          (exp (syntax-unroll stx)))
      (cond
        [(symbol? exp) (run-state (parse-variable exp src env) next)]
        [(pair? exp)
         (let* ((rator (car exp))
                (rand* (cdr exp))
                (exp^  (syntax-unroll rator)))
           (cond
             [(symbol? exp^)
              (case exp^
                [(lambda) (run-state (parse-lambda rand* src env bound-type) next)]
                [(letrec) (parse-letrec rand* src env next)]
                [(let)    (parse-let rand* src env next)]
                [(if)     (parse-if rand* src env next)]
                [(:)      (parse-ascription rand* src env next)]
                [(begin)  (run-state (parse-begin rand* src env) next)]
                [(box)          (error 'syntax->schml0/todo)]
                [(box-set!)     (error 'syntax->schml0/todo)]
                [(unbox)        (error 'syntax->schml0/todo)]
                [(vector)       (error 'syntax->schml0/todo)]
                [(vector-set!)  (error 'syntax->schml0/todo)]
                [(vector-ref)   (error 'syntax->schml0/todo)]
                [(gbox)         (run-state (parse-gbox rand* src env) next)]
                [(gunbox)       (run-state (parse-gunbox rand* src env) next)]
                [(gbox-set!)    (run-state (parse-gbox-set! rand* src env) next)]
                [(mbox)         (run-state (parse-mbox rand* src env) next)]
                [(munbox)       (run-state (parse-munbox rand* src env) next)]
                [(mbox-set!)    (run-state (parse-mbox-set! rand* src env) next)]
                [(gvector)      (run-state (parse-gvector rand* src env) next)]
                [(gvector-set!) (run-state (parse-gvector-set! rand* src env) next)]
                [(gvector-ref)  (run-state (parse-gvector-ref rand* src env) next)]
                [(mvector)      (run-state (parse-mvector rand* src env) next)]
                [(mvector-set!) (run-state (parse-mvector-set! rand* src env) next)]
                [(mvector-ref)  (run-state (parse-mvector-ref rand* src env) next)]
                [(tuple)        (run-state (parse-tuple rand* src env) next)]
                [(tuple-proj)   (run-state (parse-tuple-proj rand* src env) next)]
                [(repeat)       (run-state (parse-repeat rand* src env) next)]
                [else
                 (if (schml-primitive? exp^)
                     (run-state (parse-primitive exp^ rand* src env) next)
                     (run-state (parse-application rator rand* src env) next))])]
             [(pair? exp^) (run-state (parse-application rator rand* src env) next)]
             [else (bad-syntax-application stx)]))]
        [(schml-literal? exp) (values (Ann (Quote exp) src) next)]
        [else (raise-unsupported-syntax-exn stx)]))))

(: parse-repeat ((Listof Stx) Src Env . -> . (State Nat S0-Expr)))
(define (parse-repeat s* s e)
  (match s*
    [(list (app stx->list (list (app syntax-e id) snd trd)) eff)
     (let ([_ (id-check id '() s)])
       (do (bind-state : (State Nat S0-Expr))
           (uid   : Uid <- (uid-state (symbol->string id)))
         (start : S0-Expr <- (parse-expr snd e #f))
         (stop  : S0-Expr <- (parse-expr trd e #f))
         (eff   : S0-Expr <- (parse-expr eff (env-extend e id uid) #f))
         (return-state (Ann (Repeat uid start stop eff) s))))]
    [other (error 'parse/repeat "invalid syntax with iritants ~a ~a ~a" s* s e)]))

(: parse-variable (Symbol Src Env . -> . (State Nat S0-Expr)))
(define (parse-variable v s e)
  (return-state (Ann (Var (env-lookup e v s)) s)))

(: parse-blame-label (Stx . -> . String))
(define (parse-blame-label s)
  (let ([e (syntax-unroll s)])
    (if (string? e) e (raise-blame-label-exn s))))

#| Begin the section about parsing effects |#
(: parse-begin (-> (Listof Stx) Src Env (State Natural S0-Expr)))
(define (parse-begin stx* src env)
  (let* ([s* (reverse stx*)])
    (if (null? s*)
        (error 'syntax->schml0/todo)
        (do (bind-state : (State Natural S0-Expr))
            (e* : S0-Expr* <- (parse-expr* (reverse (cdr s*)) env #f))
          (e  : S0-Expr  <- (parse-expr  (car s*) env #f))
          (return-state (Ann (Begin e* e) src))))))

;; parsing code for guard references
(: parse-gbox (-> Stx* Src Env (State Natural S0-Expr)))
(define (parse-gbox stx* src env)
  (if (and (pair? stx*) (null? (cdr stx*)))
      (do (bind-state : (State Natural S0-Expr))
          (e : S0-Expr <- (parse-expr (car stx*) env #f))
        (return-state (Ann (Gbox e) src)))
      (error 'syntax->schml0/todo "come up with a better error msg")))

(: parse-gunbox (-> Stx* Src Env (State Natural S0-Expr)))
(define (parse-gunbox stx* src env)
  (if (and (pair? stx*) (null? (cdr stx*)))
      (do (bind-state : (State Natural S0-Expr))
          (e : S0-Expr <- (parse-expr (car stx*) env #f))
        (return-state (Ann (Gunbox e) src)))
      (error 'syntax->schml0/todo "come up with a better error")))

(: parse-gbox-set! (-> Stx* Src Env (State Natural S0-Expr)))
(define (parse-gbox-set! stx* src env)
  (match stx*
    [(list s1 s2)
     (do (bind-state : (State Natural S0-Expr))
         (e1 : S0-Expr <- (parse-expr s1 env #f))
       (e2 : S0-Expr <- (parse-expr s2 env #f))
       (return-state (Ann (Gbox-set! e1 e2) src)))]
    [otherwise (error 'syntax->schml0/todo)]))

;; parsing code for monotonic references
(: parse-mbox (-> Stx* Src Env (State Natural S0-Expr)))
(define (parse-mbox stx* src env)
  (if (and (pair? stx*) (null? (cdr stx*)))
      (do (bind-state : (State Natural S0-Expr))
          (e : S0-Expr <- (parse-expr (car stx*) env #f))
        (return-state (Ann (Mbox e) src)))
      (error 'syntax->schml0/todo)))

(: parse-munbox (-> Stx* Src Env (State Natural S0-Expr)))
(define (parse-munbox stx* src env)
  (if (and (pair? stx*) (null? (cdr stx*)))
      (do (bind-state : (State Natural S0-Expr))
          (e : S0-Expr <- (parse-expr (car stx*) env #f))
        (return-state (Ann (Munbox e) src)))
      (error 'syntax->schml0/todo)))

(: parse-mbox-set! (-> Stx* Src Env (State Natural S0-Expr)))
(define (parse-mbox-set! stx* src env)
  (match stx*
    [(list s1 s2)
     (do (bind-state : (State Natural S0-Expr))
         (e1 : S0-Expr <- (parse-expr s1 env #f))
       (e2 : S0-Expr <- (parse-expr s2 env #f))
       (return-state (Ann (Mbox-set! e1 e2) src)))]
    [otherwise (error 'syntax->schml0/todo)]))

;; parsing guarded vectors
(: parse-gvector (-> Stx* Src Env (State Natural S0-Expr)))
(define (parse-gvector stx* src env)
  (match stx*
    [(list s1 s2)
     (do (bind-state : (State Natural S0-Expr))
         (e1 : S0-Expr <- (parse-expr s1 env #f))
       (e2 : S0-Expr <- (parse-expr s2 env #f))
       (return-state (Ann (Gvector e1 e2) src)))]
    [else (error 'syntax->schml0/todo)]))

(: parse-gvector-ref (-> Stx* Src Env (State Natural S0-Expr)))
(define (parse-gvector-ref stx* src env)
  (match stx*
    [(list s1 s2)
     (do (bind-state : (State Natural S0-Expr))
         (e1 : S0-Expr <- (parse-expr s1 env #f))
       (e2 : S0-Expr <- (parse-expr s2 env #f))
       (return-state (Ann (Gvector-ref e1 e2) src)))]
    [else (error 'syntax->schml0/todo)]))

(: parse-gvector-set! (-> Stx* Src Env (State Natural S0-Expr)))
(define (parse-gvector-set! stx* src env)
  (match stx*
    [(list s1 s2 s3)
     (do (bind-state : (State Natural S0-Expr))
         (e1 : S0-Expr <- (parse-expr s1 env #f))
       (e2 : S0-Expr <- (parse-expr s2 env #f))
       (e3 : S0-Expr <- (parse-expr s3 env #f))
       (return-state (Ann (Gvector-set! e1 e2 e3) src)))]
    [else (error 'syntax->schml0/todo)]))

;; parsing monotonic vectors
(: parse-mvector (-> Stx* Src Env (State Natural S0-Expr)))
(define (parse-mvector stx* src env)
  (match stx*
    [(list s1 s2)
     (do (bind-state : (State Natural S0-Expr))
         (e1 : S0-Expr <- (parse-expr s1 env #f))
       (e2 : S0-Expr <- (parse-expr s2 env #f))
       (return-state (Ann (Mvector e1 e2) src)))]
    [else (error 'syntax->schml0/todo)]))

(: parse-mvector-ref (-> Stx* Src Env (State Natural S0-Expr)))
(define (parse-mvector-ref stx* src env)
  (match stx*
    [(list s1 s2)
     (do (bind-state : (State Natural S0-Expr))
         (e1 : S0-Expr <- (parse-expr s1 env #f))
       (e2 : S0-Expr <- (parse-expr s2 env #f))
       (return-state (Ann (Mvector-ref e1 e2) src)))]
    [else (error 'syntax->schml0/todo)]))

(: parse-mvector-set! (-> Stx* Src Env (State Natural S0-Expr)))
(define (parse-mvector-set! stx* src env)
  (match stx*
    [(list s1 s2 s3)
     (do (bind-state : (State Natural S0-Expr))
         (e1 : S0-Expr <- (parse-expr s1 env #f))
       (e2 : S0-Expr <- (parse-expr s2 env #f))
       (e3 : S0-Expr <- (parse-expr s3 env #f))
       (return-state (Ann (Mvector-set! e1 e2 e3) src)))]
    [else (error 'syntax->schml0/todo)]))



(define-type Acc2
  (List Schml-Fml* (Listof Symbol) Env Natural))

(: parse-lambda ((Listof Stx) Src Env (U False Schml-Type) -> (State Nat S0-Expr)))
(define (parse-lambda stx* src env bound-type)
  (define (help [stx : Stx] [t : (U Stx False)] [b : Stx] [s : Src] [e : Env])
    : (State Nat S0-Expr)
    (let ([f* (syntax-unroll stx)])
      (if (stx-list? f*)
          (do (bind-state : (State Nat S0-Expr))
              (n : Nat <- get-state)
            (let ([a : Acc2 (list '() '() e n)]
                  [bound-type (cond
                                [(Fn? bound-type)
                                 (match-let ([(Fn _ bound-fml*-type bound-return-type) bound-type])
                                   (cons bound-fml*-type bound-return-type))]
                                [(or (eq? bound-type #f) (eq? bound-type DYN-TYPE))
                                 (cons (make-list (length f*) #f) #f)]
                                [else (error 'syntax->schml0/todo)])])
              (if (eq? (length (car bound-type)) (length f*))
                  (match-let ([(list f* _ e n) (foldr parse-fml a f* (car bound-type))])
                    (let ([t (and t (parse-type t))])
                      (_ : Null <- (put-state n))
                      (b : S0-Expr <- (parse-expr b e #f))
                      (return-state (Ann (Lambda f* (Ann b (if t t (cdr bound-type)))) s))))
                  (raise-fml-exn stx))))
	  (raise-fml-exn stx))))
  (match stx*
    [(list fmls body) (help fmls #f body src env)]
    [(list fmls (? colon?) type body) (help fmls type body src env)]
    [otherwise (raise-lambda-exn stx* src)]))

(: parse-fml (Stx (U False Schml-Type) Acc2 . -> . Acc2))
(define (parse-fml stx bound-type acc)
  (define (help [s : Symbol] [t : Schml-Type] [l : Src] [a : Acc2])
    : Acc2
    (match-let ([(list f* s* e n) a])
      (let* ([u (Uid (symbol->string s) n)]
	     [f (Fml u t)]
	     [e (env-extend e s u)])
	(cond
          [(memq s s*) (raise-duplicate-binding s l)]
          [(reserved-symbol? s) (raise-reserved-symbol s l)]
          [else (list (cons f f*) (cons s s*) e (add1 n))]))))
  (let ((src (syntax->srcloc stx)))
    (match (syntax-unroll stx)
      [(list (app syntax-unroll (? symbol? sym)) (? colon?) type)
       (help sym (parse-type type) src acc)]
      [(? symbol? sym) (help sym (if bound-type bound-type DYN-TYPE) src acc)]
      [otherwise (raise-fml-exn stx)])))

(define-type Tmp-Bnd (List Uid (U Stx False) Stx))
(define-type Acc0 (List (Listof Tmp-Bnd) (Listof Symbol) Env Natural))
(define-type Acc1 (Pair S0-Bnd* Natural))

(: parse-tuple (-> Stx* Src Env (State Natural S0-Expr)))
(define (parse-tuple stx* src env)
  (if (null? stx*)
      (error 'schml/syntax-to-schml0/todo)
      (do (bind-state : (State Natural S0-Expr))
          (e* : S0-Expr* <- (parse-expr* stx* env #f))
        (return-state (Ann (Create-tuple e*) src)))))

(: parse-tuple-proj (-> Stx* Src Env (State Natural S0-Expr)))
(define (parse-tuple-proj stx* src env)
  (match stx*
    [(list s1 (app syntax->datum s2))
     (if (index? s2)
         (do (bind-state : (State Natural S0-Expr))
             (e : S0-Expr <- (parse-expr s1 env #f))
           (return-state (Ann (Tuple-proj e s2) src)))
         (error 'schml/syntax-to-schml0/todo "not an integer: ~a" s2))]
    [else (error 'schml/syntax-to-schml0/todo)]))


(define-syntax-rule (parse-let-form sym ctor env env-body env-bnd)
  (lambda (stx* src env next)
    (match stx*
      [(list bindings body)
       (let ([b* (syntax-unroll bindings)])
	 (if (not (stx-list? b*))
	     (raise-bnd-exn bindings)
	     (let ([a0 : Acc0 (list '() '() env next)]) ;;no unification in type checker
	       (match-let ([(list b* _ env-body n) (foldr unsplice-bnd a0 b*)])
		 (let ([a1 : Acc1 (cons '() n)]) ;; again
		   (match-let ([(cons b* n) (foldr (parse-bnd env-bnd) a1 b*)])
		     (let-values ([(e n) ((parse-expr body env-body #f) n)])
		       (values (Ann (ctor b* e) src) n))))))))]
      [otherwise (raise-let-exn form stx* src)])))

(: parse-letrec (-> (Listof Stx) Src Env Natural
		    (values S0-Expr Natural)))
(define parse-letrec (parse-let-form letrec Letrec e1 e2 e2))

(: parse-let (-> (Listof Stx) Src Env Natural
		 (values S0-Expr Natural)))
(define parse-let (parse-let-form let Let e1 e2 e1))

(: parse-bnd (Env . -> . (Tmp-Bnd Acc1 . -> . Acc1)))
(define (parse-bnd env)
  (lambda ([b : Tmp-Bnd] [a : Acc1]) : Acc1
          (match-let ([(cons bnd* next) a]
                      [(list uid type exp) b])
            (let ([type (and type (parse-type type))])
              (let-values ([(exp next) ((parse-expr exp env type) next)])
                (cons (cons (Bnd uid type exp) bnd*) next))))))

(: id-check (Any (Listof Symbol) Src -> (Listof Symbol) : #:+ Symbol))
(define (id-check x bnds src)
  (define (raise-non-identifier x s)
    (error 'syntax->schml0/todo))
  (and (or (symbol? x) (raise-non-identifier x src))
       (or (not (memq x bnds)) (raise-duplicate-binding x src))
       (or (not (reserved-symbol? x)) (raise-reserved-symbol x src))
       (cons x bnds)))

(: unsplice-bnd (Stx Acc0 -> Acc0))
(define (unsplice-bnd stx acc)
  (: help (-> Symbol (U Stx False) Stx Src Acc0
	      Acc0))
  (define (help s t r l a)
    (match-let ([(list b* s* e n) a])
      (let ([u (Uid (symbol->string s) n)])
	(cond
          [(memq s s*) (raise-duplicate-binding s l)]
          [(reserved-symbol? s) (raise-reserved-symbol s l)]
          [else (list (cons (list u t r) b*) (cons s s*)
                      (env-extend e s u)     (add1 n))]))))
  (let ([loc (syntax->srcloc stx)])
    (match (syntax-unroll stx)
      [(list (app syntax-unroll (? symbol? sym)) (? colon?) type rhs)
       (help sym type rhs loc acc)]
      [(list (app syntax-unroll (? symbol? sym)) rhs)
       (help sym #f rhs loc acc)]
      [otherwise (raise-bnd-exn stx)])))


(: parse-if (-> (Listof Stx) Src Env Natural
		(values S0-Expr Natural)))
(define (parse-if stx* src env next)
  (match stx*
    [(list test then else)
     (let*-values ([(tst next) ((parse-expr test env #f) next)]
		   [(csq next) ((parse-expr then env #f) next)]
		   [(alt next) ((parse-expr else env #f) next)])
       (values (Ann (If tst csq alt) src) next))]
    [othewise (raise-if-exn stx* src)]))

(: parse-ascription (-> (Listof Stx) Src Env Natural
			(values S0-Expr Natural)))
(define (parse-ascription stx* src env next)
  (define (help [s : Stx] [t : Stx] [l : (U Stx False)]
		[src : Src] [e : Env] [n : Natural])
    : (values S0-Expr Natural)
    (let*-values ([(exp next) ((parse-expr s e #f) n)]
		  [(type) (parse-type t)]
		  [(lbl) (and l (parse-blame-label l))])
      (values (Ann (Ascribe exp type lbl) src) next)))
  (match stx*
    [(list exp type) (help exp type #f src env next)]
    [(list exp type lbl) (help exp type lbl src env next)]
    [othewise (raise-ascribe-exn stx* src)]))

(: parse-primitive (-> Schml-Primitive (Listof Stx) Src Env (State Natural S0-Expr)))
(define (parse-primitive sym stx* src env)
  (do (bind-state : (State Natural S0-Expr))
      (args : S0-Expr* <- (parse-expr* stx* env #f))
    (return-state (Ann (Op sym args) src))))

(: parse-application (-> Stx (Listof Stx) Src Env (State Natural S0-Expr)))
(define (parse-application stx stx* src env)
  (do (bind-state : (State Natural S0-Expr))
      (rator : S0-Expr <- (parse-expr stx env #f))
    (rand* : S0-Expr* <- (parse-expr* stx* env #f))
    (return-state (Ann (App rator rand*) src))))

(: parse-expr* (-> (Listof Stx) Env (U False Schml-Type)  (State Natural S0-Expr*)))
(define (parse-expr* stx* env bound-type)
  (if (null? stx*)
      (return-state '())
      (do (bind-state : (State Natural S0-Expr*))
          (let ([a : Stx  (car stx*)]
                [d : Stx* (cdr stx*)])
            (a : S0-Expr  <- (parse-expr a env bound-type))
            (d : S0-Expr* <- (parse-expr* d env bound-type))
            (return-state (cons a d))))))

#|
Parse Type converts syntax objects to the core forms that
represents types in the schml abstract syntax tree.
|#

(: parse-type (Stx . -> . Schml-Type))
(define (parse-type stx)
  (match (syntax-unroll stx)
    ['Int INT-TYPE]
    ['Bool BOOL-TYPE]
    ['Dyn  DYN-TYPE]
    ['()   UNIT-TYPE]
    [(list (app syntax-unroll fst) stx* ...)
     (match* (fst stx*)
       #| References and Vectors are the same in a general sense |#
       ;; TODO make a flag to determine which semantics is used by default
       [('Ref (list stx)) (GRef  (parse-type stx))]
       [('Vect (list stx)) (GVect (parse-type stx))]
       ;; Guarded
       [('GRef  (list stx))  (GRef (parse-type stx))]
       [('GVect (list stx))  (GVect (parse-type stx))]
       ;; Monotonic
       [('MRef  (list stx))  (MRef (parse-type stx))]
       [('MVect (list stx))  (MVect (parse-type stx))]
       [('Tuple stx*) (STuple (length stx*) (map parse-type stx*))]
       #| I want to do gradual universal types |#
       ;;[('All (list id stx* ...)) (TODO actually parse universal types)]
       [(otherwise _) (parse-fn-type stx)])]
    [othewise (raise-type-exn stx)]))

(: parse-fn-type (Stx . -> . Schml-Type))
(define (parse-fn-type stx)
  (let ([stx* (syntax-unroll stx)])
    (if (not (stx-list? stx*))
	(raise-type-exn stx)
	(letrec ([loop : ((Listof Stx) . -> . (values (Listof Stx) Stx))
                       (lambda ([s* : (Listof Stx)])
                         (if (null? s*)
                             (raise-type-exn stx)
                             (let ((fst : Stx (car s*))
                                   (rst : (Listof Stx) (cdr s*)))
                               (if (arrow? fst)
                                   (match rst
                                     [(list stx) (values '() stx)]
                                     [other (raise-type-exn stx)])
                                   (let-values ([(from to) (loop rst)])
                                     (values (cons fst from) to))))))])
	  (let-values ([(from to) (loop stx*)])
	    (: from (Listof Stx)) (: to Stx)
	    (Fn (length from) (map parse-type from) (parse-type to)))))))

(if-in-construction
 (check-equal? (parse-fn-type #'(Int -> Int)) (Fn 1 `(,INT-TYPE) INT-TYPE))
 (check-equal? (parse-fn-type #'(-> Int)) (Fn 0 `() INT-TYPE))
 (check-equal? (parse-fn-type #'(Bool -> Bool)) (Fn 1 `(,BOOL-TYPE) BOOL-TYPE))
 (check-equal? (parse-fn-type #'(Int Bool -> Int)) (Fn 2 `(,INT-TYPE ,BOOL-TYPE) INT-TYPE))
 (check-equal? (parse-fn-type #'((-> Bool) -> (-> Int)))
               (Fn 1 `(,(Fn 0 '() BOOL-TYPE)) (Fn 0 '() INT-TYPE)))
 (check-exn exn:schml:parse? (lambda () (parse-fn-type #'(-> Int Int))))
 (check-exn exn:schml:parse? (lambda () (parse-fn-type #'(-> (-> Bool Int))))))
