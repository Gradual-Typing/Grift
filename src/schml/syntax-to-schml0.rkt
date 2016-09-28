#lang racket/base
#|------------------------------------------------------------------------------+
Pass: src/schml/syntax->schml0.rkt
+-------------------------------------------------------------------------------+
Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)
+-------------------------------------------------------------------------------+
Discription: The parse pass takes racket's native syntax objects to a set of 
the only reason that syntax objects are used in to correlate source location
information. Namely the syntax->schml0 tranformation does not handle expansion
whatsoever identifiers maintain lexical scope according to symbolic equality.
+------------------------------------------------------------------------------|#

;; TODO Make sure that we are correlating errors with source code.

(require (for-syntax racket/base
                     syntax/parse)
         racket/match
         racket/list
         syntax/parse 
         syntax/location
         "../unique-counter.rkt"
         "../logging.rkt"
         ;;"../helpers-untyped.rkt"
         "../errors.rkt"
         "../configuration.rkt"
         "../language/forms.rkt"
         "../language/schml0.rkt"
         "../language/make-begin.rkt")

(require syntax/stx)

(provide syntax->schml0
         parse-type)

(define/match (syntax->schml0 prgm)
  [((Prog name s*))
   (define who 'syntax->schml0)
   (define uc (make-unique-counter 0))
   (define e
     (parameterize ([current-unique-counter uc])
       ((parse-top-level name top-level-env)  s*)))
   (debug who (Prog (list name (unique-counter-next! uc)) e))]
  [(other) (error 'syntax->schml0 "unmatched: ~a" other)])

(define (syntax->srcloc stx)
  (srcloc (syntax-source stx)
          (syntax-line stx)
          (syntax-column stx)
          (syntax-position stx)
          (syntax-span stx)))


;; String (Hashtable Symbol Uid) -> (Listof Syntax) -> Expr
(define ((parse-top-level name env) stx*)
  (match stx*
    [(list) NO-OP]
    [(list s-final) ((parse-expr env) s-final)]
    [(list s* ... s-final)
     (define/match (expr/ann e)
       [((Ann _ s)) (Ann (Expression e) s)]
       [(other) (error 'expression "unmatched: ~a" other)])
     (define e* (map (parse-expr env) s*))
     (define ef ((parse-expr env) s-final))
     (make-begin (map expr/ann e*) ef)]
    [other (error 'parse-top-level "unmatched: ~a" other)]))

(define ((parse-expr env) stx)
  (debug off 'parse-expr stx env)
  (define src (syntax->srcloc stx))
  (define cf
    (syntax-parse stx
      [var:id
       (match (env-lookup env #'var)
         [(lexical-var var) var]
         [other (syntax-error 'parse-expr "needed variable" stx)])]
      [(var:id rest ...)
       (match (env-lookup env #'var)
         [(lexical-var var)
          (define var-src (quote-srcloc #'var))
          (define as (map (parse-expr env) (syntax->list #'(rest ...))))
          (App (Ann var var-src) as)]
         [(core parse) (parse stx env)]
         [other (error 'parse-expr "unmatched: ~a" other)])]
      [(fst rest ...)
       (define p ((parse-expr env) #'fst))
       (define as (map (parse-expr env) (syntax->list #'(rest ...))))
       (App p as)]
      [other
       (define dat (syntax->datum #'other))
       (unless (datum? dat)
         (syntax-error 'parse-expr "expected datum" #'other))
       (Quote dat)]))
  (Ann cf src))

(define (datum? x)
  (or (exact-integer? x)
      (boolean? x)
      (null? x)))

(define-syntax syntax-error
  (syntax-rules ()
    [(_ name message) (error name message)]
    [(_ name message expr)
     (let ([msg (format "~a\n\tin:~a" message expr)])
       (error name msg))]
    [(_ name message expr sub)
     (let ([msg (format "~a\n\tin:~a\n\tin particular: ~a" message expr sub)])
       (error name msg))])) 

(define (schml-core-form? x)
  (or (Lambda? x) (Letrec? x) (Let? x) (Var? x) (App? x)
      (Op? x) (Quote? x) (Ascribe? x)
      (Begin? x) (If? x) (Repeat? x)
      ;; Constructor, Destructor, Mutator should be factored out
      ;; (Ctr 'Gbox (list arg)) (Ctr 'Gvector (list arg1 arg2))
      (Gbox? x) (Gunbox? x) (Gbox-set!? x)
      (Gvector? x) (Gvector-ref? x) (Gvector-set!? x)
      (Mbox? x) (Munbox? x) (Mbox-set!? x)
      (Mvector? x) (Mvector-ref? x) (Mvector-set!? x)))

(struct lexical-var  (symbol))
(struct core (parser))
(struct core-atomic (atom))

(define (env-lookup env x)
  (debug off 'env-lookup env x)
  (define (err)
    (syntax-error 'environment "unbound variable" x))
  (hash-ref env (syntax->datum x) err))

(define (env-extend env x u)
  (hash-set env (syntax->datum x) (lexical-var (Var u))))


(define-syntax-class binding
  [pattern (var:id (~datum :) ty rhs)]
  [pattern (var:id rhs) #:with ty #f])

(define (id->uid x)
  (next-uid! (symbol->string (syntax->datum x))))
(define ((parse-optional-type env) x)
  (and (syntax? x) (syntax-e x) (parse-type x env)))

(define (destruct-and-parse-bnds-lhs ids env)
  (define uids (map id->uid ids))
  (define new-env (foldl (lambda (i u e) (env-extend e i u)) env ids uids))
  (values new-env uids))

(define (parse-let-expression stx env)
  (syntax-parse stx
    [(_ (bnd:binding ...) body)
     (define-values (new-env uids)
       (destruct-and-parse-bnds-lhs (syntax->list #'(bnd.var ...)) env))
     (define tys  (map (parse-optional-type env) (syntax->list #'(bnd.ty  ...))))
     (define rhss (map (parse-expr env) (syntax->list #'(bnd.rhs ...))))
     (Let (map Bnd uids tys rhss)
       ((parse-expr new-env) #'body))]))

(define (parse-letrec-expression stx env)
  (syntax-parse stx
    [(_ (bnd:binding ...) body)
     (define-values (new-env uids)
       (destruct-and-parse-bnds-lhs (syntax->list #'(bnd.var ...)) env))
     (define tys  (map (parse-optional-type env) (syntax->list #'(bnd.ty  ...))))
     (define rhss (map (parse-expr new-env) (syntax->list #'(bnd.rhs ...))))
     (Letrec (map Bnd uids tys rhss)
       ((parse-expr new-env) #'body))]))

(module+ test
  (require "../language/forms-equal.rkt")
  (current-unique-counter (make-unique-counter 0))
  (check-forms=?
   ((parse-expr top-level-env) #'(let ([x 1]) x))
   (Ann (Let (list (Bnd 'x #f (Ann (Quote 1) (quote-srcloc))))
          (Ann (Var 'x) (quote-srcloc)))
        (quote-srcloc)))
  (check-forms=?
   ((parse-expr top-level-env) #'(letrec ([x x]) x))
   (Ann (Letrec (list (Bnd 'x #f (Ann (Var 'x) (quote-srcloc))))
          (Ann (Var 'x) (quote-srcloc)))
        (quote-srcloc))))

(define-syntax-class formal-parameter
  [pattern [var:id (~datum :) ty]]
  [pattern var:id #:with ty #'Dyn])

(define (parse-lambda-expression stx env)
  (syntax-parse stx
    [(_ (fml:formal-parameter ...)
        (~optional (~seq (~datum :) ty) #:defaults ([ty #'#f]))
        body)
     (define-values (new-env uids)
       (destruct-and-parse-bnds-lhs (syntax->list #'(fml.var ...)) env))
     (define (pt s) (parse-type s env))
     (define tys  (map pt (syntax->list #'(fml.ty  ...))))
     (Lambda (map Fml uids tys)
       (Ann ((parse-expr new-env) #'body)
            ((parse-optional-type new-env) #'ty)))]))

(module+ test
  (check-forms=?
   ((parse-expr top-level-env) #'(lambda (x y) x))
   (Ann (Lambda (list (Fml 'x (Dyn)) (Fml 'y (Dyn)))
          (Ann (Ann (Var 'x) (quote-srcloc)) #f))
        (quote-srcloc)))
  (check-forms=?
   ((parse-expr top-level-env) #'(lambda ([x : Int]) x))
   (Ann (Lambda (list (Fml 'x (Int)))
          (Ann (Ann (Var 'x) (quote-srcloc)) #f))
        (quote-srcloc)))
  (check-forms=?
   ((parse-expr top-level-env) #'(lambda ([x : Int]) : Int x))
   (Ann (Lambda (list (Fml 'x (Int)))
          (Ann (Ann (Var 'x) (quote-srcloc)) (Int)))
        (quote-srcloc))))

(define (parse-op stx env)
  (syntax-parse stx
    [(op rands ...)
     (define o (syntax->datum #'op))
     (define r* (syntax->list #'(rands ...)))
     (Op o (map (parse-expr env) r*))]))

(define (parse-ascription stx env)
  (syntax-parse stx
    [(_ expr ty (~optional (~seq l:str) #:defaults ([l #'#f])))
     (define lbl (syntax->datum (attribute l)))
     (unless (or (not lbl) (string? lbl))
       (error 'parse-ascription "this doesn't work like I thought it would: ~a ~a" lbl #'l))
     (Ascribe ((parse-expr env) #'expr) (parse-type #'ty env) lbl)]))

(module+ test
  (check-forms=? (parse-ascription #'(ann 3 Int) top-level-env)
                 (Ascribe (Ann (Quote 3) (quote-srcloc)) (Int) #f))
  
  (check-forms=? (parse-ascription #'(ann #f Int "Bad") top-level-env)
                 (Ascribe (Ann (Quote #f) (quote-srcloc)) (Int)  "Bad")))

(define (parse-begin stx env)
  (syntax-parse stx
    [(_ expr* ... expr)
     (Begin (map (parse-expr env) (syntax->list #'(expr* ...)))
            ((parse-expr env) #'expr))]))

(define (env-extend* env . bnds)
  (let loop ([env env] [b* bnds])
    (match b*
      [(list) env]
      [(cons id (cons uid others))
       (loop (env-extend env id uid) others)]
      [other (error 'env-extend* "unmatched ~a" other)])))

(define (parse-repeat stx env)
  (debug off 'parse-repeat env)
  (define-syntax-class index-binding
    [pattern ((~var id id) (~var start) (~var stop))])
  (define-syntax-class accumulator-binding
    #:datum-literals (:)
    [pattern ((~var id id)
              (~optional (~seq : (~var type))
                         #:defaults ([type #'#f]))
              (~var init))])
  (syntax-parse stx
    [(_ i:index-binding a:accumulator-binding M)
     (define i (id->uid #'i.id))
     (define a (id->uid #'a.id))
     (unless (and (Uid? i) (Uid? a))
       (error 'crap))
     (define new-env (env-extend* env #'i.id i #'a.id a))
     (define recur (parse-expr env))
     (Repeat i (recur #'i.start) (recur #'i.stop)
       (Ann a ((parse-optional-type env) #'a.type))
       ((parse-expr env) #'a.init)
       ((parse-expr new-env) #'M))]))

(module+ test
  (parse-repeat #'(repeat (foo 0 10) (bar : Int 0) foo) top-level-env))

(define (parse-switch stx env)
  (define-syntax-class switch-clause
    [pattern [(case*:integer ...+) rhs]])
  (define recur (parse-expr env))
  (syntax-parse stx
    #:datum-literals (else)
    [(_ e c*:switch-clause ... [else default])
     (Switch (recur #'e)
             (map cons
                  (syntax->datum #'((c*.case* ...) ...))
                  (map recur (syntax->list #'(c*.rhs ...))))
             (recur #'default))]))

(define ((parse-simple-form sym ctr arg-count) stx env)
  (syntax-parse stx
    #:context sym
    [(_ params ...)
     #:do [(define p* (syntax->list #'(params ...)))]
     #:when (= (length p*) arg-count)
     (apply ctr (map (parse-expr env) p*))]))

(define ((parse-simple*-form sym ctr) stx env)
  (syntax-parse stx
    #:context sym
    [(_ params ...)
     #:do [(define p* (syntax->list #'(params ...)))]
     (ctr (map (parse-expr env) p*))]))

(define (parse-tuple-proj stx env)
  (syntax-parse stx
    [(_ t i:integer)
     (Tuple-proj ((parse-expr env) #'t) (syntax->datum #'i))]))


(module+ test
  (define parse-tuple (parse-simple*-form 'tuple Create-tuple))
  (check-forms=? (parse-tuple #'(tuple 0 1 2) top-level-env)
                 (Create-tuple
                  (list (Ann (Quote 0) (quote-srcloc))
                        (Ann (Quote 1) (quote-srcloc))
                        (Ann (Quote 2) (quote-srcloc))))))

(define (parse-time stx env)
  ;; this should be implemented as a macro once we have an expander
  (syntax-parse stx
    [(_ exp)
     (define src (syntax->srcloc stx))
     (define (/src e) (Ann e src))
     (define/match (begin . e*)
       [((list e* ... e)) (Begin (map /src e*) (/src e))])
     (define (op s) (Op s '()))
     (define tmp (next-uid! "tmp_s2s0"))
     (begin (op 'timer-start)
            (Let (list (Bnd tmp #f ((parse-expr env) #'exp)))
              (/src (begin (op 'timer-stop)
                           (op 'timer-report)
                           (Var tmp)))))]))





;; Constructor, Destructor, Mutator should be factored out
;; (Ctr 'Gbox (list arg)) (Ctr 'Gvector (list arg1 arg2))
;; (Gbox? x) (Gunbox? x) (Gbox-set!? x)
;; (Gvector? x) (Gvector-ref? x) (Gvector-set!? x)
;; (Mbox? x) (Munbox? x) (Mbox-set!? x)
;;       (Mvector? x) (Mvector-ref? x) (Mvector-set!? x)

#|
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
|#






#|

(: reserved-symbol? (Symbol . -> . Boolean))
(define (reserved-symbol? x)
  (case x
    [(lambda letrec let if :) #t]
    [(gbox gunbox gbox-set! gvector gvector-ref gvector-set!) #t]
    [(mbox munbox mbox-set! mvector mvector-ref mvector-set!) #t]
    [(tuple tuple-proj) #t]
    [(+ - * binary-and binary-or < <= = >= > %/ %% %<< %>>) #t]
    [(time timer-start timer-stop timer-report repeat) #t]
    [(read-int switch) #t]
    [else #f]))



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
                [(box)          (error 'syntax->schml0/box "unimplemented")]
                [(box-set!)     (error 'syntax->schml0/box-set! "unimplemented")]
                [(unbox)        (error 'syntax->schml0/unbox "unimplemented")]
                [(vector)       (error 'syntax->schml0/vecotor "unimplemented")]
                [(vector-set!)  (error 'syntax->schml0/vector-set! "unimplemented")]
                [(vector-ref)   (error 'syntax->schml0/vector-ref "unimplemented")]
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
                [(time) (run-state (parse-time rand* src env) next)]
                [(switch)       (run-state (parse-switch rand* src env) next)]
                [else
                 (if (schml-primitive? exp^)
                     (run-state (parse-primitive exp^ rand* src env) next)
                     (run-state (parse-application rator rand* src env) next))])]
             [(pair? exp^) (run-state (parse-application rator rand* src env) next)]
             [else (bad-syntax-application stx)]))]
        [(schml-literal? exp) (values (Ann (Quote exp) src) next)]
        [else (raise-unsupported-syntax-exn stx)]))))

(: parse-switch ((Listof Stx) Src Env . -> . (State Nat S0-Expr)))
(define (parse-switch s* s e)
  (define (parse-clauses s* e)
    (match s*
      [(list (app stx->list ()))]))
  (match s*
    [(list-rest exp 
           (app stx->list (cons (app syntax-e acc-id) acc-rest))
           exp)
     (let* ([e1 (id-check index-id '() s)]
            [e2 (id-check acc-id '() s)])
       (do (bind-state : (State Nat S0-Expr))
           (index-uid   : Uid <- (uid-state (symbol->string index-id)))
           (acc-uid : Uid <- (uid-state (symbol->string acc-id)))
           (let ([e^ (env-extend (env-extend e index-id index-uid)
                                 acc-id acc-uid)])
             (start : S0-Expr <- (parse-expr snd e #f))
           (stop  : S0-Expr <- (parse-expr trd e #f))
           (exp   : S0-Expr <- (parse-expr exp e^ #f))
           (match acc-rest
             [(list (? colon?) acc-type acc-init)
              (acc-init : S0-Expr <- (parse-expr acc-init e #f))
              (let ([acc-type (parse-type acc-type)])
                (return-state
                 (Ann (Repeat index-uid start stop (Ann acc-uid acc-type)
                              acc-init exp) s)))]
             [(list acc-init)
              (acc-init : S0-Expr <- (parse-expr acc-init e #f))
              (return-state
               (Ann (Repeat index-uid start stop (Ann acc-uid #f) acc-init exp) s))]
             [other (error 'parse/repeat
                           "invalid syntax in accumulator binding: ~a ~a ~a"
                           s* s e)]))))]
    [other (error 'parse/repeat "invalid syntax with iritants ~a ~a ~a" s* s e)]))


(: parse-repeat ((Listof Stx) Src Env . -> . (State Nat S0-Expr)))
(define (parse-repeat s* s e)
  (match s*
    [(list (app stx->list (list (app syntax-e index-id) snd trd))
           (app stx->list (cons (app syntax-e acc-id) acc-rest))
           exp)
     (let* ([e1 (id-check index-id '() s)]
            [e2 (id-check acc-id '() s)])
       (do (bind-state : (State Nat S0-Expr))
           (index-uid   : Uid <- (uid-state (symbol->string index-id)))
           (acc-uid : Uid <- (uid-state (symbol->string acc-id)))
           (let ([e^ (env-extend (env-extend e index-id index-uid)
                                 acc-id acc-uid)])
             (start : S0-Expr <- (parse-expr snd e #f))
           (stop  : S0-Expr <- (parse-expr trd e #f))
           (exp   : S0-Expr <- (parse-expr exp e^ #f))
           (match acc-rest
             [(list (? colon?) acc-type acc-init)
              (acc-init : S0-Expr <- (parse-expr acc-init e #f))
              (let ([acc-type (parse-type acc-type)])
                (return-state
                 (Ann (Repeat index-uid start stop (Ann acc-uid acc-type)
                              acc-init exp) s)))]
             [(list acc-init)
              (acc-init : S0-Expr <- (parse-expr acc-init e #f))
              (return-state
               (Ann (Repeat index-uid start stop (Ann acc-uid #f) acc-init exp) s))]
             [other (error 'parse/repeat
                           "invalid syntax in accumulator binding: ~a ~a ~a"
                           s* s e)]))))]
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
        (error 'syntax->schml0/begin "invalid syntax empty begin at ~a" src)
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
    (error 'syntax->schml0/id-check "not symbol: ~a" x))
  (and (or (symbol? x)
           (raise-non-identifier x src))
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
;|#

(define (parse-type stx [env top-level-env])
  (define (recur s) (parse-type s env))
  (syntax-parse stx
    #:datum-literals (-> Ref Vect GRef GVect MRef MVect Tuple)
    [atm:id
     (match (env-lookup env #'atm)
       [(lexical-var id) id]
       [(core-atomic cf) cf]
       [other
        (error 'parse-type
               "unmatched atom ~a resolved to ~a"
               (syntax->datum #'atm) other)])]
    [() UNIT-TYPE]
    ;; Nice syntax for types currently implemented with support from parser
    ;; Todo: should we provide an infix binding construct in general?
    [(t* ... -> t)
     (define a* (syntax->list #'(t* ...)))
     (Fn (length a*) (map recur a*) (recur #'t))]
    [(atm:id stx* ...)
     (match (env-lookup env #'atm)
       [(core t) (t stx env)]
       [other
        (error 'parse-type
               "unmatched type operator ~a resolved to ~a"
               (syntax->datum #'atm) other)])]))


(define ((make-parse-type-w/ctr sym ctr) stx env)
  (syntax-parse stx
    [(_ stx) (ctr (parse-type #'stx env))]
    [other (syntax-error sym "invalid syntax" stx)]))


(define (parse-tuple-type stx env)
  (define (map-type s) (parse-type s env))
  (syntax-parse stx
    [(Tuple) UNIT-TYPE]
    [(Tuple type ...)
     (define a* (syntax->list #'(type ...)))
     (STuple (length a*) (map map-type a*))]))

(module+ test
  (require rackunit)
  (define-syntax-rule (tpt f s)
    (check-equal? (parse-type f) s))
  (tpt #'Int INT-TYPE)
  (check-equal? (parse-type #'(Int -> Int))
                (Fn 1 `(,INT-TYPE) INT-TYPE))
  (check-equal? (parse-type #'(-> Int))
                (Fn 0 `() INT-TYPE))
  (check-equal? (parse-type #'(Bool -> Bool))
                (Fn 1 `(,BOOL-TYPE) BOOL-TYPE))
  (check-equal? (parse-type #'(Int Bool -> Int))
                (Fn 2 `(,INT-TYPE ,BOOL-TYPE) INT-TYPE))
  (check-equal? (parse-type #'((-> Bool) -> (-> Int)))
                (Fn 1 `(,(Fn 0 '() BOOL-TYPE)) (Fn 0 '() INT-TYPE)))
  (check-exn exn? (lambda () (parse-type #'(-> Int Int))))
  (check-exn exn? (lambda () (parse-type #'(-> (-> Bool Int)))))
   (check-equal? (parse-type #'Unit)
                UNIT-TYPE)
  (check-equal? (parse-type #'(Tuple))
                UNIT-TYPE)
  (check-equal? (parse-type #'(Tuple Int Bool))
                (STuple 2 `(,INT-TYPE ,BOOL-TYPE))))

(define core-parse-prim (core parse-op))

(define top-level-env
  (make-immutable-hash
   `((let    . ,(core parse-let-expression))
     (letrec . ,(core parse-letrec-expression))
     (lambda . ,(core parse-lambda-expression))
     (if     . ,(core (parse-simple-form 'if If 3)))
     (begin  . ,(core parse-begin))
     (* . ,core-parse-prim)
     (+ . ,core-parse-prim)
     (- . ,core-parse-prim)
     (%/ . ,core-parse-prim)
     (%% . ,core-parse-prim)
     (%>> . ,core-parse-prim)
     (%<< . ,core-parse-prim)
     (binary-and . ,core-parse-prim)
     (binary-or  . ,core-parse-prim)
     (binary-xor . ,core-parse-prim)
     (read-int . ,core-parse-prim)
     (<  . ,core-parse-prim)
     (<= . ,core-parse-prim)
     (=  . ,core-parse-prim)
     (>  . ,core-parse-prim)
     (>= . ,core-parse-prim)
     (tuple     . ,(core (parse-simple*-form 'tuple Create-tuple)))
     (tuple-proj . ,(core parse-tuple-proj))
     (repeat . ,(core parse-repeat))
     (switch . ,(core parse-switch))
     (Int    . ,(core-atomic INT-TYPE))
     (Bool   . ,(core-atomic BOOL-TYPE))
     (Unit   . ,(core-atomic UNIT-TYPE))
     (Dyn    . ,(core-atomic DYN-TYPE))
     (Tuple . ,(core parse-tuple-type))
     (Ref   . ,(core (make-parse-type-w/ctr 'GRef  GRef)))
     (Vect  . ,(core (make-parse-type-w/ctr 'GVect GVect)))
     (GRef   . ,(core (make-parse-type-w/ctr 'GRef  GRef)))
     (GVect  . ,(core (make-parse-type-w/ctr 'GVect GVect)))
     (MVect  . ,(core (make-parse-type-w/ctr 'MVect MVect)))
     (MRef   . ,(core (make-parse-type-w/ctr 'MRef  MRef)))
     ,@(let ([parse-gbox      (parse-simple-form 'gbox Gbox 1)]
             [parse-gbox-set! (parse-simple-form 'gbox-set! Gbox-set! 2)]
             [parse-gunbox    (parse-simple-form 'gunbox Gunbox 1)]
             [parse-gvector
              (parse-simple-form 'gvector Gvector 2)]
             [parse-gvector-ref
              (parse-simple-form 'gvector-ref Gvector-ref 2)]
             [parse-gvector-set!
              (parse-simple-form 'gvector-set! Gvector-set! 3)])
         `((gvector   . ,(core parse-gvector))
           (gvector-ref . ,(core parse-gvector-ref))
           (gvector-set! . ,(core parse-gvector-set!))
           (vector   . ,(core parse-gvector))
           (vector-ref . ,(core parse-gvector-ref))
           (vector-set! . ,(core parse-gvector-set!))
           (gbox      . ,(core parse-gbox))
           (gbox-set! . ,(core parse-gbox-set!))
           (gunbox    . ,(core parse-gunbox))
           (box       . ,(core parse-gbox))
           (box-set!  . ,(core parse-gbox-set!))
           (unbox    . ,(core parse-gunbox))))
     (mbox . , (core (parse-simple-form 'mbox Mbox 1)))
     (mumbox . ,(core (parse-simple-form 'munbox Munbox 1)))
     (mbox-set! . ,(core (parse-simple-form 'mbox-set! Mbox-set! 2)))
     (ann . ,(core parse-ascription))
     (: . ,(core parse-ascription))
     (time . ,(core parse-time))
     (timer-start . ,core-parse-prim)
     (timer-stop . ,core-parse-prim)
     (timer-report . ,core-parse-prim)
     )))


(module+ test
  (check-forms=? ((parse-expr top-level-env) #'(vector 0 2))
                 (Ann (Gvector (Ann (Quote 0) (quote-srcloc))
                               (Ann (Quote 2) (quote-srcloc)))
                      (quote-srcloc))))
