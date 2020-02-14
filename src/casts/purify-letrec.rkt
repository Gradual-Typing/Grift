#lang typed/racket/base/no-check

#|------------------------------------------------------------------------------+
|Pass: src/insert-casts                                                         |
+-------------------------------------------------------------------------------+
|Author: Deyaaeldeen Almahallawi (dalmahal@indiana.edu)
|        Andre Kuhlenschmidt (akuhlens@indiana.edu)                             |
+-------------------------------------------------------------------------------+
|Description: This pass rewrites letrec expressions so that, in the output of   |
| the pass, all letrec expressions are "pure," i.e., bind only variables to     |
| lambda expressions.                                                           |
+-------------------------------------------------------------------------------+
|Input Grammar: Cast0-Lang                                                      |
+------------------------------------------------------------------------------|#
(require
 racket/match
 racket/list
 racket/set
 "../errors.rkt"
 "../language/forms.rkt"
 "../language/form-map.rkt"
 "../logging.rkt"
 "../type-equality.rkt")

(provide purify-letrec)

(: purify-letrec (Cast-or-Coerce3.1-Lang -> Cast-or-Coerce3.2-Lang))
(define (purify-letrec prgm)
  (match-define (Prog (list prgm-name prgm-next prgm-type)
                  (Static* (list
                            mu-type-bnd*
                            prgm-type-bnd*
                            mu-crcn-bnd*
                            prgm-crcn-bnd*
                            const-bnd*)
                           prgm-expr))
    prgm)

  (define unique (make-unique-counter prgm-next))

  (: expr/pure-letrec CoC3.2-Expr)
  (: const-bnd*/pure-letrec CoC3.2-Bnd*)
  (define-values (expr/pure-letrec const-bnd*/pure-letrec)
    (parameterize ([current-unique-counter unique])
      (values
       (pl-expr prgm-expr)
       (for/list ([b const-bnd*])
         (match-define (cons id expr) b)
         (cons id (pl-expr expr))))))
  
  (Prog (list prgm-name (unique-counter-next! unique) prgm-type)
    (Static* (list
              mu-type-bnd*
              prgm-type-bnd*
              mu-crcn-bnd*
              prgm-crcn-bnd*
              const-bnd*/pure-letrec)
             expr/pure-letrec)))

;; TODO: is it possible to merge simple? and pl-expr to make only one
;; pass over the AST?
(: simple? (CoC3.1-Expr (Setof Uid) Integer Boolean -> Boolean))
(define (simple? expr uid* depth outer-lambda?)
  
  (define (recur [x : CoC3.1-Expr])
    (simple? x uid* (add1 depth) outer-lambda?))

  (define (recur* [x* : CoC3.1-Expr*])
    (andmap recur x*))
  
  (: recur-all : (->* () #:rest CoC3.1-Expr Boolean))
  (define (recur-all . x*)
    (recur* x*))

  (debug 'simple? expr uid*)
  (match expr
    ;; The really interesting choices
    [(Var x) (set-member? uid* x)]
    [(Global _) #f]
    [(Assign _ (app recur e)) e]
    [(Lambda _ (Castable ctr e))
     (and (> depth 0) (simple? e uid* (+ 1 depth) #t))]
    [(App-Fn e e*) 
     (and outer-lambda? (recur e) (recur* e*))]
    [(App-Fn-or-Proxy u1 u2 e e*)
     (and outer-lambda? (recur e) (recur* e*))]
    [(App-Code e e*)
     (and outer-lambda? (recur e) (recur* e*))]
    ;; Constant data is simple
    [(or (Quote _) (Quote-Coercion _) (Type _) (Code-Label _) (Tag _) (No-Op)) #t]
    ;; All other forms are simple if their constituents are simple
    [(Letrec bnd* expr)
     (and (recur expr) (recur* (map (inst cdr Uid CoC3.1-Expr) bnd*)))]
    [(Let bnd* expr)
     (and (recur expr) (recur* (map (inst cdr Uid CoC3.1-Expr) bnd*)))]
    [(Labels b* e)
     (define (bnd-code-extract-e [b : CoC3.1-Bnd-Code])
       (match-let ([(cons _ (Code _ e)) b])
         e))
     (and (recur e) (recur* (map bnd-code-extract-e b*)))]
    [(Dyn-Object e t) (recur-all e t)]
    [(Dyn-Value e) (recur e)]
    [(Dyn-Type t) (recur t)]
    [(Dyn-Immediate-Value dyn) (recur dyn)]
    [(Dyn-Immediate-Tag=Huh dyn type) (recur dyn)]
    [(Dyn-Immediate-Object e t) (recur e)]
    [(Dyn-Box-Object value type) (recur-all value type)]
    [(Dyn-Box-Value dyn) (recur dyn)]
    [(Dyn-Box-Type dyn) (recur dyn)]
    [(Op _ e*) (recur* e*)]
    [(If e1 e2 e3) (recur-all e1 e2 e3)]
    [(Switch e c* d)
     (and (recur-all e d) (recur* (map (inst cdr Any CoC3.1-Expr) c*)))]
    [(Begin e* e)  (and (recur e) (recur* e*))]
    [(Repeat i e1 e2 a e3 e4) (recur-all e1 e2 e3 e4)]
    [(While e1 e2) (recur-all e1 e2)]
    [(Break-Repeat) #f] ;; TODO consider why this is false
    [(Lambda f* (Castable c e)) (recur e)]
    [(Fn-Caster e) (recur e)]
    [(Fn-Proxy i e1 e2) (recur-all e1 e2)]
    [(Fn-Proxy-Huh e) (recur e)]
    [(Fn-Proxy-Closure e) (recur e)]
    [(Fn-Proxy-Coercion e) (recur e)]
    [(Compose-Coercions e1 e2) (recur-all e1 e2)]
    [(HC p? t1 lbl i? t2 m) (recur-all p? t1 lbl i? t2 m)]
    [(HC-Inject-Huh h) (recur h)]
    [(HC-Project-Huh h) (recur h)]
    [(HC-Identity-Huh h) (recur h)]
    [(HC-Label h) (recur h)]
    [(HC-T1 h) (recur h)]
    [(HC-T2 h) (recur h)]
    [(HC-Med h) (recur h)]
    [(Id-Coercion-Huh e) (recur e)]
    [(Fn-Coercion-Huh e) (recur e)]
    [(Make-Fn-Coercion u e1 e2 e3) (recur-all e1 e2 e3)]
    [(Fn-Coercion e* e) (and (recur e) (recur* e*))]
    [(Fn-Coercion-Arity e) (recur e)]
    [(Fn-Coercion-Arg e1 e2) (recur-all e1 e2)]
    [(Fn-Coercion-Return e) (recur e)]
    [(Id-Fn-Coercion a) (recur a)]
    [(Fn-Coercion-Arg-Set! f i a) (recur-all f i a)]
    [(Fn-Coercion-Return-Set! f r) (recur-all f r)]
    [(Tuple-Coercion-Item-Set! t i e) (recur-all t i e)]
    [(Id-Tuple-Coercion a) (recur a)]
    [(Ref-Coercion e1 e2 flag) (recur-all e1 e2)]
    [(Ref-Coercion-Huh e) (recur e)]
    [(Ref-Coercion-Read e) (recur e)]
    [(Ref-Coercion-Write e) (recur e)]
    [(Ref-Coercion-Ref-Huh e) (recur e)]
    [(Sequence-Coercion e1 e2) (recur-all e1 e2)]
    [(Sequence-Coercion-Huh e) (recur e)]
    [(Sequence-Coercion-Fst e) (recur e)]
    [(Sequence-Coercion-Snd e) (recur e)]
    [(Project-Coercion e1 e2) (recur-all e1 e2)]
    [(Project-Coercion-Huh e) (recur e)]
    [(Project-Coercion-Type e) (recur e)]
    [(Project-Coercion-Label e) (recur e)]
    [(Inject-Coercion e) (recur e)]
    [(Inject-Coercion-Type e) (recur e)]
    [(Inject-Coercion-Huh e) (recur e)]
    [(Failed-Coercion e) (recur e)]
    [(Failed-Coercion-Huh e) (recur e)]
    [(Failed-Coercion-Label e) (recur e)]
    [(Type-Dyn-Huh e) (recur e)] 
    [(Type-Fn-arg e1 e2) (recur-all e1 e2)]
    [(Type-Fn-return e) (recur e)]
    [(Type-Fn-arity e) (recur e)]
    [(Type-Fn-Huh e) (recur e)]
    [(Type-GRef-Of e) (recur e)]
    [(Type-GVect-Of e) (recur e)]
    [(Type-GRef-Huh e) (recur e)]
    [(Type-GVect-Huh e) (recur e)]
    [(Type-Mu-Huh e) (recur e)]
    [(Type-Mu-Body e) (recur e)]
    [(Type-Tag e) (recur e)]
    [(Blame e) (recur e)]
    [(Observe e t) (recur e)]
    [(Unguarded-Box e) (recur e)]
    [(Unguarded-Box-On-Stack e) (recur e)] 
    [(Unguarded-Box-Ref e) (recur e)]
    [(Unguarded-Box-Set! e1 e2) (recur-all e1 e2)]
    [(Unguarded-Vect e1 e2) (recur-all e1 e2)]
    [(Unguarded-Vect-Ref e1 e2) (recur-all e1 e2)]
    [(Unguarded-Vect-Set! e1 e2 e3) (recur-all e1 e2 e3)]
    [(Guarded-Proxy-Huh e) (recur e)]
    [(Guarded-Proxy e1 r)
     (match r
       [(Twosome e2 e3 e4) (recur-all e1 e2 e3 e4)]
       [(Coercion e2) (recur-all e1 e2)])]
    [(Guarded-Proxy-Ref e) (recur e)]
    [(Guarded-Proxy-Source e) (recur e)]
    [(Guarded-Proxy-Target e) (recur e)]
    [(Guarded-Proxy-Blames e) (recur e)]
    [(Guarded-Proxy-Coercion e) (recur e)]
    [(Unguarded-Vect-length e) (recur e)]
    [(Mbox e t) (recur e)]
    [(Mbox-val-set! e1 e2) (recur-all e1 e2)]
    [(Mbox-val-ref e) (recur e)]
    [(Mbox-rtti-set! addr e) (recur-all addr e)]
    [(Mbox-rtti-ref addr) (recur addr)]
    [(Make-GLB-Two-Fn-Types u e1 e2) (recur-all e1 e2)]
    [(Make-GLB-Two-Tuple-Types u e1 e2) (recur-all e1 e2)]
    [(MRef-Coercion-Huh e) (recur e)]
    [(MRef-Coercion-Type e) (recur e)]
    [(MRef-Coercion e) (recur e)]
    [(Type-GRef e) (recur e)]
    [(Type-GVect e) (recur e)]
    [(Type-MRef e) (recur e)]
    [(Type-MRef-Huh e) (recur e)]
    [(Type-MRef-Of e) (recur e)]
    [(Mvector e1 e2 t) (recur-all e1 e2)]
    [(Mvector-length e) (recur e)]
    [(Mvector-val-set! e1 e2 e3 mode) (recur-all e1 e2 e3)]
    [(Mvector-val-ref e1 e2 mode) (recur-all e1 e2)]
    [(Mvector-rtti-set! addr e) (recur-all addr e)]
    [(Mvector-rtti-ref addr) (recur addr)]
    [(Type-MVect e) (recur e)]
    [(Type-MVect-Huh e) (recur e)]
    [(Type-MVect-Of e) (recur e)]
    [(MVect-Coercion-Huh e) (recur e)]
    [(MVect-Coercion-Type e) (recur e)]
    [(MVect-Coercion e) (recur e)]
    [(Error e) (recur e)]
    [(Create-tuple e*) (recur* e*)]
    [(Tuple-proj e i) (recur-all e i)]
    [(Tuple-Coercion-Huh e) (recur e)]
    [(Tuple-Coercion-Num e) (recur e)]
    [(Tuple-Coercion-Item e i) (recur-all e i)]
    [(Coerce-Tuple uid e1 e2 suspend-monotonic-heap-casts?) (recur-all e1 e2 suspend-monotonic-heap-casts?)]
    [(Cast-Tuple uid e1 e2 e3 e4 suspend-monotonic-heap-casts?) (recur-all e1 e2 e3 e4 suspend-monotonic-heap-casts?)]
    [(Type-Tuple-Huh e) (recur e)]
    [(Type-Tuple-num e) (recur e)]
    [(Type-Tuple-item e i) (recur-all e i)]
    [(Make-Tuple-Coercion uid t1 t2 lbl) (recur-all t1 t2 lbl)]
    [(Mediating-Coercion-Huh e) (recur e)]
    [(Construct t v e*) (recur* e*)]
    [(Access t f e i?)
     (if i? (recur-all e i?) (recur e))]
    [(Check t p e e*) (and (recur e) (recur* e*))]
    [other (error 'purify-letrec/simple? "unmatched ~a" other)]))

(: replace-ref :
   CoC3.2-Expr (Setof Uid) (Setof Uid) (U 'ok 'error Uid) -> CoC3.2-Expr)
;; takes an expression, the uids that refer to complex bindings, the
;; uids that refer to lambda bindings, and a mode flag.  If the mode
;; is ok the all references to complex bindings are replaced with
;; unboxing of the complex binding, and all lambda references are
;; allowed.  If the mode is error then all references to complex or
;; lambda bindings result in a compile time error.  If the mode is a
;; Uid then all references to complex or lambda bindings first check
;; to ensure that the letrec has finished initializing then retrieve
;; the referenced value.
(define (replace-ref expr c* l* uid-inited)
  (: recur : CoC3.2-Expr -> CoC3.2-Expr)
  (define (recur e)
    (cond
      [(Var? e)
       (match-define (Var x) e)
       (cond
         [(set-member? c* x)
          (cond
            [(eq? uid-inited 'ok) (Unguarded-Box-Ref e)] 
            ;; TODO the following error should be more precise and
            ;; indicates we should retain source locations for
            ;; expressions throughout the compiler.
            [(eq? uid-inited 'error)
             (error 'purify-letrec "reference to uninitalized binding: ~a" x)]
            [else
             (define err-msg (format "reference to uninitalized binding: ~a" x))
             (If (Unguarded-Box-Ref (Var uid-inited))
                 (Unguarded-Box-Ref e)
                 (Blame (Quote err-msg)))])]
         [(set-member? l* x)
          (cond
            [(eq? uid-inited 'ok) e]
            [(eq? uid-inited 'error) 
             (error 'purify-letrec "reference to uninitalized binding: ~a" x)]
            [else
             (define err-msg (format "reference to unintialized binding: ~a" x))
             (If (Unguarded-Box-Ref (Var uid-inited))
                 e
                 (Blame (Quote err-msg)))])]
         [else e])]
      ;; TODO this is currently less efficient than it should be. We
      ;; could fix this by implementing the algorithim in "fixing
      ;; letrec" ie a flow sensitive conservative approximation of
      ;; when it is ok to reference recursive values.
      [else (form-map e recur)]))
  (recur expr))

;; A specialized version of replace-ref that knows it takes and recieves
;; lambda forms. Since recursive references are always initialized
;; by the time a lambda on the rhs of a letrec is found recursive
;; reference are always allowed.
(: replace-ref-lam (CoC3.2-Lambda (Setof Uid) (Setof Uid) -> CoC3.2-Lambda))
(define (replace-ref-lam expr c* l*)
  (match-let ([(Lambda f* (Castable ctr? e)) expr])
    (Lambda f* (Castable ctr? (replace-ref e c* l* 'ok)))))


(: pl-expr (CoC3.1-Expr -> CoC3.2-Expr))
(define (pl-expr e)
  (cond
    [(Letrec? e)
     (match-define (Letrec bnd* (app pl-expr expr)) e)
     (define who 'purify-letrec/pl-expr/letrec-matched)
     (define who-return 'purify-letrec/pl-expr/letrec-match-returns)
     (debug who (Letrec bnd* expr))
     (define bound-uid* (list->set (map (inst car Uid CoC3.1-Expr) bnd*)))
     ;; TODO casted lambdas are never invoked by there cast
     ;; if we could determine which lambdas are the result of calling
     ;; the cast runtime procedure then we wouldn't have to take a
     ;; performance hit on these.
     ;; One simple way of doing this would be to purify-letrec before
     ;; lowering casts then implement the "fixing letrec" algorithm.
     (: lambda* CoC3.2-Bnd-Lambda*)
     (define-values (simple* complex* lambda*)
       (for/fold ([s* : CoC3.2-Bnd* '()]
                  [c* : CoC3.2-Bnd* '()]
                  [l* : CoC3.2-Bnd-Lambda* '()])
                 ([bnd : CoC3.1-Bnd bnd*])
         (match bnd
           [(cons i (Lambda f* (Castable ctr (app pl-expr expr)))) 
            (values s* c* `([,i . ,(Lambda f* (Castable ctr expr))] . ,l*))]
           [(cons i (app pl-expr expr))
            (cond
              [(simple? expr bound-uid* 0 #f)
               (debug 'is-simple? i expr bound-uid*)
               (values `([,i . ,expr] . ,s*) c* l*)]
              [else (values s* `([,i . ,expr] . ,c*) l*)])])))
     (debug 'purify-letrec complex* lambda*)
     (define t* : Uid* (map (lambda (x) (next-uid! "tmp")) complex*))
     (define c* : Uid* (map (inst car Uid Any) complex*))
     (define l* : Uid* (map (inst car Uid Any) lambda*))
     (define i  : Uid  (next-uid! "letrec_initialized"))
     (define setofc* : (Setof Uid) (list->set c*))
     (define setofl* : (Setof Uid) (list->set l*))
     ;; Don't traverse expression unless there is work to be done
     ;; Any replaced references do not need validity checks because
     ;; the body of a letrec always runs after everything is initialized. 
     (define expr^ : CoC3.2-Expr
       (if (null? complex*)
           expr 
           (replace-ref expr setofc* setofl* 'ok)))
     (define simple-bnd* : CoC3.2-Bnd* simple*)

     (define (bnd-unitialized-box [c : Uid]) : CoC3.2-Bnd
       (cons c (Unguarded-Box (Quote 0))))
     
     (define complex-bnd* : CoC3.2-Bnd* 
       (if (null? complex*)
           '()
           (cons (ann (cons i (Unguarded-Box (Quote #f))) CoC3.2-Bnd)
                 (debug (map bnd-unitialized-box c*)))))
     ;; Don't traverse lambdas unless there is work to be done
     (define lambda-bnd* : CoC3.2-Bnd-Lambda*
       (cond
         [(null? complex*) lambda*]
         [else
          (define (make-lambda-bnd [b : CoC3.2-Bnd-Lambda])
            (match-define (cons i e) b)
            ;; references to complex values in lambda bindings is unchecked because
            ;; by the time a function in applied the letrec intialization
            ;; must have completed.
            (cons i (replace-ref-lam e setofc* setofl*)))
          (map make-lambda-bnd lambda*)]))
     
     (define temp-bnd* : CoC3.2-Bnd*
       (map (lambda ([t : Uid] [c : CoC3.2-Bnd])
              : CoC3.2-Bnd
              (match-define (cons _ e) c)
              (cons t (replace-ref e setofc* setofl* i)))
            (debug t*) complex*))
     (define (make-move [c : Uid] [t : Uid]) : CoC3.2-Expr
       (Unguarded-Box-Set! (Var c) (Var t)))
     (define set-complex* : CoC3.2-Expr*
       (let ([move* (map make-move c* t*)])
         (if (null? complex*)
             '()
             (append move* (list (Unguarded-Box-Set! (Var i) (Quote #t)))))))
     (define return
       (let* ([let-tmps : CoC3.2-Expr
                        (debug
                         (cond
                           [(null? temp-bnd*) expr^]
                           [else (Let temp-bnd* (Begin set-complex* expr^))]))]
              
              [let-lambdas : CoC3.2-Expr
                           (debug
                            (cond
                              [(null? lambda-bnd*) let-tmps]
                              [else (Letrec lambda-bnd* let-tmps)]))]
              [let-complex : CoC3.2-Expr
                           (debug
                            (cond
                              [(null? complex-bnd*) let-lambdas]
                              [else (Let complex-bnd* let-lambdas)]))])
         (debug
          (cond
            [(null? simple*) let-complex]
            [else (Let simple* let-complex)]))))
     (debug who-return return)]
    [else (form-map e pl-expr)]))

