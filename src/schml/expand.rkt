#lang racket
#|------------------------------------------------------------------------------+
|Pass: src/schml/expand                                                         |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
|Discription: The parse pass takes racket's native syntax objects to a set of   |
|core structures that are annotated with the source location.                   |
+-------------------------------------------------------------------------------+
+------------------------------------------------------------------------------|#

(require schml/src/helpers
	 schml/src/errors)

(struct Uid ([prefix : String] [suffix : Natural]) #:transparent)
(define-type Uid* (Listof Uid))

(: uid->string (-> Uid String))
(define (uid->string u)
  (format "u_~a_~a" (Uid-suffix) (Uid-prefix)))

(: uid=? (-> Uid Uid Boolean))
(define (uid=? [u : Uid] [v : Uid])
  (= (Uid-suffix u) (Uid-suffix v)))

(define-syntax do
  (syntax-rules (<- : let)
    [(_ bind e) e]
    [(_ bind (v : t <- e0) e e* ...)
     (bind e0 (lambda ((v : t)) (do e e* ...)))]
    [(_ bind (let b ...) e e* ...)
     (let (b ...) (do bind e e*))]
    [(_ bind e0 e e* ...)  (bind e0 (lambda ((_ : Any)) (do e e* ...)))]))

(define-type (State s a) (s . -> . (values a s)))

(provide expand-schml-program)

;; A State monad for uniquefication
(struct (unique A) ((state : Natural) (value : A)))
(define-type (GesymM A) (-> Natural (unique Natural A)))

;; the cononical example of the gensym monad
(define-type GenSym (GensymM Symbol))

;; the representation used in this compiler
(define-type GenUid (GensymM Uid))

(define-type Mark Natural)

(define top-mark : Mark 0)

(define init-mark : Mark 1)

(define-type GenMark (GenM Mark))

(define (gensym (id : Symbol))
  : GenSym
  (lambda ((s : Natural))
    (unique (add1 s) (string->symbol (format "~a_~a" id s)))))

(define (genuid (id : String))
  : GenUid
  (lambda ((s : Natural))
    (unique (add1 s) (uid id s))))



(define (genmark)
  : GenMark
  (lambda ((s : Natural))
    (unique (add1 s) s)))

(struct label ())

(define-type (Exp A) (U Symbol Integer String Boolean
                        (Listof Stx)
                        (Boxof Stx)
                        (Vector Stx)))

(define-type SExp (Rec sexp (Exp sexp)))
(define-type Stx-E (Exp stx))



;; Syntax objects for the expander
(struct stx ((src : srcloc) (wrap : Any) (e : Stx-E)))

(define-type Stx stx)

(struct binding ())
(struct binding:macro   binding (value))
(struct binding:lexical binding (value))

(define list? pair?)

(define (strip (x : Stx)) : SExp
  (let ([e (stx-e x)])
    (cond
      [(or (symbol? x)
           (integer? x)
           (boolean? x)
           (string? x)) 
       x]
      [(list? e) (map strip e)]
      [(vector? e) (vector-map strip e)]
      [(box? e)    (box (strip (unbox e)))])))

(define (top-marked? (w : Wrap))
  (wrap-member? w top-mark))

(define (syntax-error (obj : Stx) (msg : String))
  (error #f "~a ~a" msg obj))

(define (identifier? x)
  (and (stx? x) (symbol? (stx-e x))))

(define (self-evaluating? x)
  (or (boolean? x) (integer? x) (string? x) (char? x)))

(define (add-mark mark x)
  (extend-wrap (list mark) x))

(define (add-subst id label x)
  (extend-wrap 
   (list (make-subst
          (stx-e id)
          (wrap-marks (stx-wrap id))
          label))
   x))

(define (extend-wrap w x)
  (if (stx? x)
      (stx (stx-e x)
           (join-wraps w (stx-wrap x)))
      (stx x wrap)))

(define (join-wraps x y)
  (cond
    [(null? x) y]
    [(null? y) x]
    [else 
     (let loop ((z (car x)) (z* (cdr x)))
       (if (null? w*)
           (if (and (mark? w) (eq? (car y) w))
               (cdr y)
               (cons w y))
           (cons w (f (car w*) (cdr w*)))))]))

(define (extend-env label binding env)
  (cons (cons label binding) env))

(define (id-binding id r)
  (label-binding id (id-label id) r))

(define (id-label id)
  (let ([sym (stx-e id)]
        [wrap (stx-wrap id)])
    (let search ((wrap wrap) (mark* (wrap-marks wrap)))
      (if (null? wrap)
          (syntax-error id "undefined identifier")
          (let ([w0 (car wrap)])
            (if (mark? W0)
                (search (cdr wrap) (cdr mark*))
                (if (and (eq? (subst-sym w0) sym)
                         (same-marks? (subst-mark* w0) mark*))
                    (subst-label w0)
                    (search (cdr wrap) (mark*)))))))))

(define (wrap-marks w*)
  (if (null? w*)
      '()
      (let ([w0 (car w*)])
        (if (mark? w0)
            (cons w0 (wrap-marks (cdr w)))
            (wrap-marks (cdr w))))))

(define (same-marks? m1* m2*)
  (if (null? m1*)
      (null? m1*)
      (and (not (null? m2*))
           (= (car m1*) (car m2*))
           (same-marks? (cdr m1*) (cdr m2*)))))

(define (label-binding id label r)
  (let ([a (assv label r)])
    (if a
        (cdr a)
        (syntax-error id "displaced lexical"))))

(define (expand-expr x r mr)
  (let ([e (stx-e x)])
    (cond
      [(symbol? x) 
       (let ([b (id-binding x r)])
         (cond
           [(lexical? b) (lexical-value v)]
           [(macro? b) (exp (exp-macro (macro-value b) r mr))]
           [else (syntax-error x "invalid syntax")]))]
      [(list? x)
       (let ([rator (car x)])
         (if (identifier? (car x))
             (let ([b (id-binding x)])
               (cond
                 [(macro? b) ]))))])))




(define #:forall (a b) (vector-map (f : (-> a b)) (v : (Vector a)))
  : (Vector b)
  (build-vector 
   (vector-length e)
   (lambda ((n : Natural))
     (f (vector-ref e n)))))

(define (racket-stx->schml-stx stx)
  (let rec ((obj : Stx))
    (let ([src : Src (syntax->srcloc stx)]
          [e (syntax-e stx)]))
     (cond
       [(symbol? e) (stx-id src '() e)]
       [(pair? e) 
        (let ([a (rec (car e))]
              [d (rec (cdr e))])
          (cond
            [(stx-list? d) 
             (stx-list src '() (cons a  (stx-list-e d)))]
            [(stx-null? d) (stx-list src '() '())]
            [else (stx-pair src '() a d)]))]
       [(null? e) (stx-null src '())]
       [(integer? e) (stx-int src '() e)]
       [(string? e)  (stx-str src '() e)]
       [(boolean? e) (str-bool src '() e)]
       [(box? e)     (stx-box src '() (box (rec (unbox e))))]
       [(vector? e)  (stx-vector src '() (vector-map rec e))]
       [else (TODO come up with a sensible error here)])))



(define #:forall (m a) (return-state (p : a))
  : (State m a)
  (lambda ((i : m)) (values p i)))

(define #:forall (m a b) (bind-state (pi : (State m a))
                                     (f : (-> a (State m b))))
  : (State m b)
  (lambda ((i : m))
    (let-values ([((p : a) (i : m)) (pi i)])
      ((f p) i))))

(define (uid-state (name : String))
  : (State Natural Uid)
  (lambda ((s : Natural))
    (values (Uid name s) (add1 s))))

(define #:forall (m a) (put-state (p : a))
  : (-> m (values a a))
  (lambda ((s : m))
    (values p p)))

(define #:forall (m) get-state : (State m m)
  (lambda ((i : m))
    (values i i)))


#| 
   This might be a plausable imperative implementation of Uid generation
   For the time being it should not be used. maybe we need a gensym function
   but for pride's sake I would like to remain functional.

(: uid (-> String Uid))
(define uid
  (let ([unique-number : Natural (box 0)])
    (lambda ([name : String])
      (let ((val (unbox unique-number)))
        (set-box! unique-number (+ val 1))
        (Uid name val)))))
|#

(: next-uid (-> String Natural (values Uid Natural)))
(define (next-uid prefix suffix) next-uid
  (values (Uid prefix suffix) (add1 suffix)))

(define-syntax let-uid*
  (syntax-rules ()
    [(_ (wrong ...) others ...) (raise-syntax-error 'let-uid "next must always be an identifier")]
    [(_ next () body ...)(let () body ...)]
    [(_ next ([name0 prefix0] [name* prefix*] ...) body ...)
     (let-values ([(name0 next) (next-uid prefix0 next)])
       (let-uid* next ([name* prefix*] ...)
		 body ...))]))

(define FIRST-UID-SUFFIX 0)



(define-type Schml0-Lang 
  (Prog (List name config unum) S0-Expr))

(define-type S0-Expr
  (Rec E (Ann (U (Lambda Schml-Fml* (Option Schml-Type) E)
		 (Letrec S0-Bnd* E)
		 (Let S0-Bnd* E)
		 (App E (Listof E))
		 (Op Schml-Prim (Listof E))
		 (If E E E)
		 (Ascribe E Schml-Type (Option String))
		 (Var Uid)
		 (Quote Schml-Literal)
                 (Begin (Listof E) E)
                 ;; Monotonic effects
                 (Mbox E)
                 (Munbox E)
                 (Mbox-set! E E)
                 (Mvector E E)
                 (Mvector-set! E E E)
                 (Mvector-ref E E)
                 ;; Guarded effects
                 (Gbox E)
                 (Gunbox E)
                 (Gbox-set! E E)
                 (Gvector E E)
                 (Gvector-set! E E E)
                 (Gvector-ref E E))
              Src))
  (Rec (U (list '))))

(: expand (Syntax-Lang Config . -> . Schml0-Lang))

(define (expand prgm)
  (match-let ([(Prog (list name config) stx*) prgm])
    (let-values
        ([(exp unum) (run-state (expand-top-level stx
                                                  default-env
                                                  transformer-env)
                                init-unum)])
      (Prog (list name unum config) exp))))


(define-type (State s a) (s . -> . (values a s)))



(define-syntax do-state
  (syntax-rules (<- : let)
    ((_ ts e) e)
    ((_ ts (v ta <- e0) e e* ...)
     (lambda ((s : ts))
       (let-values ([((v : ta) (s : ts)) (e0 s)])
         (do ts e e*))))
    ((_ ts (let (b ...)) e e* ...)
     (let (b ...)
       (do ts e e* ...)))
    ((_ ts e e* ...) 
     (bind e (lambda (_) (do ts e* ...))))))

(define-syntax return-state%
  (syntax-rules (:)
    [(_ : t a) (lambda ((s : t)) (values s a))]))


(define #:forall (m a) (return-state (p : a))
  : (State m a)
  (lambda ((i : m)) (values a s)))

(define #:forall (m a b) (bind-state (pi : (State m a)) 
                                     (f : (-> a (State m b))))
  : (State m b)
  (lambda ((i : m))
    (let-values ([((p : a) (i : m)) (ma i)])
      (f p))))

(define (uid-state (name : String)) 
  : (State UID Natural)
  (lambda ((s : Natural))
    (values (UID name s) (add1 s))))

(define #:forall (m a) (put-state (p : a))
  : (State Void m)
  (lambda ((s : m))
    (values (void) p)))

(define #:forall (m) get-state : (State m m)
  (lambda ((i : m))
    (values i i)))

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


;; stx-list? is equivalent to (and (list? o) (andmap syntax? o))
(define-predicate stx-list? (Listof Stx))	   

(: reserved-symbol? (Symbol . -> . Boolean))
(define (reserved-symbol? x)
  (case x
    [(lambda letrec let if :) #t]
    [(+ - * binary-and binary-or < <= = >= > %/ %<< %>>) #t]
    [else #f]))


   
#| The parser |#
(: parse-expr (Stx Env Natural . -> . (values S0-Expr Natural)))
(define (parse-expr stx env next)
  (let ((src (syntax->srcloc stx))
	(exp (syntax-unroll stx)))
    (cond
     [(symbol? exp) (values (parse-variable exp src env) next)]
     [(pair? exp) 
      (let* ((rator (car exp)) 
	     (rand* (cdr exp)) 
	     (exp^ (syntax-unroll rator)))
	(cond
	 [(symbol? exp^)
	  (case exp^
            [(lambda) (parse-lambda rand* src env next)]
            [(letrec) (parse-letrec rand* src env next)]
            [(let)    (parse-let rand* src env next)]
            [(if)     (parse-if rand* src env next)]
            [(:)      (parse-ascription rand* src env next)]
            [else
             (if (Schml-Prim? exp^) 
                 (parse-primitive exp^ rand* src env next)
                 (parse-application rator rand* src env next))])]
	 [(pair? exp^) (parse-application rator rand* src env next)]
	 [else (bad-syntax-application stx)]))]
     [(schml-literal? exp) (values (Ann (Quote exp) src) next)] 
     [else (raise-unsupported-syntax-exn stx)])))

(: parse-variable (Symbol Src Env . -> . S0-Expr))
(define (parse-variable v s e)
  (Ann (Var (env-lookup e v s)) s))

(: parse-blame-label (Stx . -> . String))
(define (parse-blame-label s)
  (let ([e (syntax-unroll s)])
    (if (string? e) e (raise-blame-label-exn s))))


(define-type Acc2 
  (List Schml-Fml* (Listof Symbol) Env Natural))
(: parse-lambda (-> (Listof Stx) Src Env Natural 
		    (values S0-Expr Natural)))
(define (parse-lambda stx* src env next)
  (define (help [stx : Stx] 
		[t : (U Stx False)]
		[b : Stx] 
		[s : Src] 
		[e : Env] 
		[n : Natural])
    : (values S0-Expr Natural)
    (let ([f* (syntax-unroll stx)])
      (if (stx-list? f*)
	  (let ([a : Acc2 (list '() '() e n)])
	    (match-let ([(list f* _ e n) (foldr parse-fml a f*)])
	      (let-values ([(t) (and t (parse-type t))]
			   [(b n) (parse-expr b e n)])
		(values (Ann (Lambda f* t b) s)  n))))
	  (raise-fml-exn stx))))
  (match stx*
    [(list fmls body)
     (help fmls #f body src env next)]
    [(list fmls (? colon?) type body)
     (help fmls type body src env next)]
    [otherwise (raise-lambda-exn stx* src)]))

(: parse-fml (Stx Acc2 . -> . Acc2))
(define (parse-fml stx acc)
  (define (help [s : Symbol] [t : (U Stx False)] [l : Src] [a : Acc2]) 
    : Acc2 
    (match-let ([(list f* s* e n) a])
      (let* ([t (if t (parse-type t) DYN-TYPE)]
	     [u (Uid (symbol->string s) n)]
	     [f (Fml u t)]
	     [e (env-extend e s u)])
	(cond
	 [(memq s s*) (raise-duplicate-binding s l)]
	 [(reserved-symbol? s) (raise-reservered-sym s l)]
	 [else (list (cons f f*) (cons s s*) e (add1 n))]))))
  (let ((src (syntax->srcloc stx)))
    (match (syntax-unroll stx)
      [(list (app syntax-unroll (? symbol? sym)) (? colon?) type) 
       (help sym type src acc)]
      [(? symbol? sym) (help sym #f src acc)]
      [otherwise (raise-fml-exn stx)])))

(define-type Tmp-Bnd (List Uid (U Stx False) Stx))
(define-type Acc0 (List (Listof Tmp-Bnd) (Listof Symbol) Env Natural))
(define-type Acc1 (Pair S0-Bnd* Natural))

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
		     (let-values ([(e n) (parse-expr body env-body n)])
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
  (lambda ([b : Tmp-Bnd] [a : Acc1]) 
    : Acc1
    (match-let ([(cons bnd* next) a]
		[(list uid type exp) b])
      (let-values ([(exp next) (parse-expr exp env next)]
		   [(type) (and type (parse-type type))])
	(cons (cons (Bnd uid type exp) bnd*) next)))))


(: unsplice-bnd (Stx Acc0 -> Acc0))
(define (unsplice-bnd stx acc)
  (: help (-> Symbol (U Stx False) Stx Src Acc0
	      Acc0))
  (define (help s t r l a)
    (match-let ([(list b* s* e n) a])
      (let ([u (Uid (symbol->string s) n)])
	(cond
	 [(memq s s*) (raise-duplicate-binding s l)]
	 [(reserved-symbol? s) (raise-reservered-sym s l)]
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
     (let*-values ([(tst next) (parse-expr test env next)]
		   [(csq next) (parse-expr then env next)]
		   [(alt next) (parse-expr else env next)])
       (values (Ann (If tst csq alt) src) next))]
    [othewise (raise-if-exn stx* src)]))

(: parse-ascription (-> (Listof Stx) Src Env Natural
			(values S0-Expr Natural)))
(define (parse-ascription stx* src env next)
  (define (help [s : Stx] [t : Stx] [l : (U Stx False)] 
		[src : Src] [e : Env] [n : Natural]) 
    : (values S0-Expr Natural) 
    (let*-values ([(exp next) (parse-expr s e n)]
		  [(type) (parse-type t)]
		  [(lbl) (and l (parse-blame-label l))])
      (values (Ann (Ascribe exp type lbl) src) next)))
  (match stx*
    [(list exp type) (help exp type #f src env next)]
    [(list exp type lbl) (help exp type lbl src env next)]
    [othewise (raise-ascribe-exn stx* src)]))

(: parse-primitive (-> Schml-Prim (Listof Stx) Src Env Natural 
		       (values S0-Expr Natural)))
(define (parse-primitive sym stx* src env next)
  (let-values ([(args next) (parse-expr* stx* env next)])
    (values (Ann (Op sym args) src) next)))

(: parse-application (-> Stx (Listof Stx) Src Env Natural
			 (values S0-Expr Natural)))
(define (parse-application stx stx* src env next)
  (let*-values ([(rator next) (parse-expr stx env next)]
		[(rands next) (parse-expr* stx* env next)])
    (values (Ann (App rator rands) src) next)))

(: parse-expr* (-> (Listof Stx) Env Natural 
		   (values (Listof S0-Expr) Natural)))
(define (parse-expr* s* e n) 
  (if (null? s*)
      (values '() n)
      (let*-values ([(cf new-n) (parse-expr (car s*) e n)]
		    [(cf* newer-n) (parse-expr* (cdr s*) e new-n)])
         (values (cons cf cf*) newer-n))))

(: parse-type (Stx . -> . Schml-Type))
(define (parse-type stx)
  (match (syntax-unroll stx)
    ['Int INT-TYPE]
    ['Bool BOOL-TYPE]
    ['Dyn  DYN-TYPE]
    [(list stx* ...) (parse-fn-type stx)]
    [othewise (raise-type-exn stx)]))

(: parse-fn-type (Stx . -> . Schml-Type))
(define (parse-fn-type stx)
  (let ([stx* (syntax-unroll stx)])
    (if (not (stx-list? stx*))
	(raise-type-exn stx)
	(letrec ([loop : ((Listof Stx) . -> . 
			  (values (Listof Stx) (Listof Stx)))
		  (lambda ([s* : (Listof Stx)])
		    (if (null? s*) 
			(raise-type-exn stx)
			(let ((fst : Stx (car s*))
			      (rst : (Listof Stx) (cdr s*)))
			  (if (arrow? fst)
			      (values '() rst)
			      (let-values ([(from to) (loop rst)])
				(values (cons fst from) to))))))])
	  (let-values ([(from to) (loop stx*)])
	    (: from (Listof Stx))
	    (: to (Listof Stx))
	    (Fn (length from) (map parse-type from) (car (map parse-type to))))))))
  
(: parse-top-level
   (String (Listof Stx) Env Natural . -> . 
	   (values S0-Expr Natural)))
(define (parse-top-level name stx* env next) 
  : (values S0-Expr Natural)
  (cond
   [(null? stx*) (raise-file-empty-exn name)]
   [(not (null? (cdr stx*))) (raise-<1-exp-exn name)]
   [else (parse-expr (car stx*) env next)]))

