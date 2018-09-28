#lang typed/racket/base

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
 (submod  "../logging.rkt" typed)
 "../errors.rkt"
 "../language/lambda0.rkt"
 "../language/lambda1.rkt"
 racket/match
 racket/list
 racket/set)

;; Only the pass is provided by this module
(provide
 purify-letrec
 (all-from-out
  "../language/lambda0.rkt"
  "../language/lambda1.rkt"))

(: purify-letrec (Lambda0-Lang -> Lambda1-Lang))
(define (purify-letrec prgm)
  (match-define (Prog (list prgm-name prgm-next prgm-type)
                  (Let-Static* prgm-type-bnd* prgm-crcn-bnd* prgm-expr))
    prgm)

  (define unique (make-unique-counter prgm-next))

  (define expr/pure-letrec
    (parameterize ([current-unique-counter unique])
      (pl-expr prgm-expr)))
  
  (Prog (list prgm-name (unique-counter-next! unique) prgm-type)
    (Let-Static* prgm-type-bnd* prgm-crcn-bnd* expr/pure-letrec)))

;; TODO: is it possible to merge simple? and pl-expr to make only one
;; pass over the AST?
(: simple? (L0-Expr (Setof Uid) Integer Boolean -> Boolean))
(define (simple? expr uid* depth outer-lambda?)
  
  (define (recur [x : L0-Expr])
    (simple? x uid* (add1 depth) outer-lambda?))

  (define (recur* [x* : L0-Expr*])
    (andmap recur x*))
  
  (: recur-all : (->* () #:rest L0-Expr Boolean))
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
    [(App-Fn-or-Proxy u e e*)
     (and outer-lambda? (recur e) (recur* e*))]
    [(App-Code e e*)
     (and outer-lambda? (recur e) (recur* e*))]
    ;; Constant data is simple
    [(or (Quote _) (Quote-Coercion _) (Type _) (Code-Label _) (Tag _) (No-Op)) #t]
    ;; All other forms are simple if their constituents are simple
    [(Letrec bnd* expr)
     (and (recur expr) (recur* (map (inst cdr Uid L0-Expr) bnd*)))]
    [(Let bnd* expr)
     (and (recur expr) (recur* (map (inst cdr Uid L0-Expr) bnd*)))]
    [(Labels b* e)
     (define (bnd-code-extract-e [b : L0-Bnd-Code])
       (match-let ([(cons _ (Code _ e)) b])
         e))
     (and (recur e) (recur* (map bnd-code-extract-e b*)))]
    [(Op _ e*) (recur* e*)]
    [(If e1 e2 e3) (recur-all e1 e2 e3)]
    [(Switch e c* d)
     (and (recur-all e d) (recur* (map (inst cdr Any L0-Expr) c*)))]
    [(Begin e* e)  (and (recur e) (recur* e*))]
    [(Repeat i e1 e2 a e3 e4) (recur-all e1 e2 e3 e4)]
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
    [(Type-Tag e) (recur e)]
    [(Blame e) (recur e)]
    [(Observe e t) (recur e)]
    [(Unguarded-Box e) (recur e)]
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
    [(Mvector-val-set! e1 e2 e3) (recur-all e1 e2 e3)]
    [(Mvector-val-ref e1 e2) (recur-all e1 e2)]
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
    [(Coerce-Tuple uid e1 e2) (recur-all e1 e2)]
    [(Coerce-Tuple-In-Place uid e1 e2 e3 e4 e5) (recur-all e1 e2 e3 e4 e5)]
    [(Cast-Tuple uid e1 e2 e3 e4) (recur-all e1 e2 e3 e4)]
    [(Cast-Tuple-In-Place uid e1 e2 e3 e4 e5 e6 e7)
     (recur-all e1 e2 e3 e4 e5 e6 e7)]
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

;; A specialized version of replace-ref that knows it takes and recieves
;; lambda forms. Since recursive references are always initialized
;; by the time a lambda on the rhs of a letrec is found recursive
;; reference are always allowed.
(: replace-ref-lam (L1-Lambda (Setof Uid) (Setof Uid) -> L1-Lambda))
(define (replace-ref-lam expr c* l*)
  (match-let ([(Lambda f* (Castable ctr? e)) expr])
    (Lambda f* (Castable ctr? (replace-ref e c* l* 'ok)))))

(: replace-ref (L1-Expr (Setof Uid) (Setof Uid) (U 'ok 'error Uid) -> L1-Expr))
;; takes an expression, the uids that refer to complex bindings, the uids that
;; refer to lambda bindings, and a mode flag.
;; If the mode is ok the all references to complex bindings are replaced with
;; unboxing of the complex binding, and all lambda references are allowed.
;; If the mode is error then all references to complex or lambda bindings result
;; in a compile time error.
;; If the mode is a Uid then all references to complex or lambda bindings first
;; check to ensure that the letrec has finished initializing then retrieve the
;; referenced value.
(define (replace-ref expr c* l* uid-inited)
  (define (recur [e : L1-Expr])
    (replace-ref e c* l* uid-inited))
  (define (recur* [e* : L1-Expr*])
    (map recur e*))
  (match expr
    ;; TODO this is currently less efficient than it should be. We could fix this
    ;; by implementing the algorithim in "fixing letrec" ie a flow sensitive
    ;; conservative approximation of when it is ok to reference recursive values.
    [(and v (Var x))
     (cond
       [(set-member? c* x)
        (cond
          [(eq? uid-inited 'ok) (Unguarded-Box-Ref (Var x))] 
          ;; TODO the following error should be more precise and indicates
          ;; we should retain source locations for expressions throughout the compiler.
          [(eq? uid-inited 'error)
           (error 'purify-letrec "reference to uninitalized binding: ~a" x)]
          [else (If (Unguarded-Box-Ref (Var uid-inited))
                    (Unguarded-Box-Ref v)
                    (Blame (Quote (format "reference to uninitalized binding: ~a" x))))])]
       [(set-member? l* x)
        (cond
          [(eq? uid-inited 'ok) v]
          [(eq? uid-inited 'error) 
           (error 'purify-letrec "reference to uninitalized binding: ~a" x)]
          [else (If (Unguarded-Box-Ref (Var uid-inited))
                    v
                    (Blame (Quote (format "reference to unintialized binding: ~a" x))))])]
       [else v])]
    [(Global s) (Global s)]
    [(Assign u/s (app recur e)) (Assign u/s e)]
    ;; Every other case is just a boring flow agnostic tree traversal
    [(or (Quote _) (Quote-Coercion _) (Type _)) expr]
    [(or (Code-Label _) (Tag _) (No-Op)) expr]
    [(Lambda f* (Castable ctr e)) (Lambda f* (Castable ctr (recur e)))]
    ;; TODO examine this code and ensure it isn't just doing
    ;; wasted computation we are using unique identifiers
    ;; their shouldn't be binding overlaps
    [(Letrec b* e)
     (define (recur-bnd-lambda [b : L1-Bnd-Lambda])
       (match-let ([(cons i (Lambda i* (Castable c? e))) b])
         (cons i (Lambda i* (Castable c? (recur e))))))
     (Letrec (map recur-bnd-lambda b*) (recur e))]
    [(Let b* e)
     (define (recur-bnd [b : L1-Bnd])
       (match-let ([(cons i e) b])
         (cons i (recur e))))
     (Let (map recur-bnd b*) (recur e))]
    [(Labels b* e)
     (define (recur-bnd-code [b : L1-Bnd-Code])
       (match-let ([(cons i (Code i* e)) b])
         (cons i (Code i* (recur e)))))
     (Labels (map recur-bnd-code b*) e)]
    [(App-Code (app recur e) (app recur* e*))
     (App-Code e e*)]
    [(Lambda f* (Castable c (app recur e)))
     (Lambda f* (Castable c e))]
    [(Fn-Caster (app recur e))
     (Fn-Caster e)]
    [(App-Fn (app recur e) (app recur* e*))
     (App-Fn e e*)]
    [(App-Fn-or-Proxy u (app recur e) (app recur* e*))
     (App-Fn-or-Proxy u e e*)]
    [(Fn-Proxy i (app recur e1) (app recur e2))
     (Fn-Proxy i e1 e2)]
    [(Fn-Proxy-Huh (app recur e))
     (Fn-Proxy-Huh e)]
    [(Fn-Proxy-Closure (app recur e))
     (Fn-Proxy-Closure e)]
    [(Fn-Proxy-Coercion (app recur e))
     (Fn-Proxy-Coercion e)]
    [(Compose-Coercions (app recur e1) (app recur e2))
     (Compose-Coercions e1 e2)]
    [(HC (app recur p?) (app recur t1) (app recur lbl)
         (app recur i?) (app recur t2)
         (app recur m))
     (HC p? t1 lbl i? t2 m)]
    [(HC-Inject-Huh (app recur h)) (HC-Inject-Huh h)]
    [(HC-Project-Huh (app recur h)) (HC-Project-Huh h)]
    [(HC-Identity-Huh (app recur h)) (HC-Identity-Huh h)]
    [(HC-Label (app recur h)) (HC-Label h)]
    [(HC-T1 (app recur h)) (HC-T1 h)]
    [(HC-T2 (app recur h)) (HC-T2 h)]
    [(HC-Med (app recur h)) (HC-Med h)]
    [(Id-Coercion-Huh (app recur e))
     (Id-Coercion-Huh e)]
    [(Fn-Coercion-Huh (app recur e))
     (Fn-Coercion-Huh e)]
    [(Make-Fn-Coercion u (app recur e1)(app recur e2)e3)
     (Make-Fn-Coercion u e1 e2 e3)]
    [(Fn-Coercion (app recur* e*)(app recur e))
     (Fn-Coercion e* e)]
    [(Fn-Coercion-Arity (app recur e))
     (Fn-Coercion-Arity e)]
    [(Fn-Coercion-Arg (app recur e1)(app recur e2))
     (Fn-Coercion-Arg e1 e2)]
    [(Fn-Coercion-Return (app recur e))
     (Fn-Coercion-Return e)]
    [(Id-Fn-Coercion (app recur a)) (Id-Fn-Coercion a)]
    [(Fn-Coercion-Arg-Set! (app recur f) (app recur i) (app recur a))
     (Fn-Coercion-Arg-Set! f i a)]
    [(Fn-Coercion-Return-Set! (app recur f) (app recur r))
     (Fn-Coercion-Return-Set! f r)]
    [(Tuple-Coercion-Item-Set! (app recur t) (app recur i) (app recur e))
     (Tuple-Coercion-Item-Set! t i e)]
    [(Id-Tuple-Coercion (app recur a))
     (Id-Tuple-Coercion a)]
    [(Ref-Coercion (app recur e1) (app recur e2) (app recur flag))
     (Ref-Coercion e1 e2 flag)]
    [(Ref-Coercion-Huh (app recur e))
     (Ref-Coercion-Huh e)]
    [(Ref-Coercion-Read (app recur e))
     (Ref-Coercion-Read e)]
    [(Ref-Coercion-Write (app recur e))
     (Ref-Coercion-Write e)]
    [(Ref-Coercion-Ref-Huh (app recur e))
     (Ref-Coercion-Ref-Huh e)]
    [(Sequence-Coercion (app recur e1) (app recur e2))
     (Sequence-Coercion e1 e2)]
    [(Sequence-Coercion-Huh (app recur e))
     (Sequence-Coercion-Huh e)]
    [(Sequence-Coercion-Fst (app recur e))
     (Sequence-Coercion-Fst e)]
    [(Sequence-Coercion-Snd (app recur e))
     (Sequence-Coercion-Snd e)]
    [(Project-Coercion (app recur e1) (app recur e2))
     (Project-Coercion e1 e2)]
    [(Project-Coercion-Huh (app recur e))
     (Project-Coercion-Huh e)]
    [(Project-Coercion-Type (app recur e))
     (Project-Coercion-Type e)]
    [(Project-Coercion-Label (app recur e))
     (Project-Coercion-Label e)]
    [(Inject-Coercion (app recur e))
     (Inject-Coercion e)]
    [(Inject-Coercion-Type (app recur e))
     (Inject-Coercion-Type e)]
    [(Inject-Coercion-Huh (app recur e))
     (Inject-Coercion-Huh e)]
    [(Failed-Coercion (app recur e))
     (Failed-Coercion e)]
    [(Failed-Coercion-Huh (app recur e))
     (Failed-Coercion-Huh e)]
    [(Failed-Coercion-Label (app recur e))
     (Failed-Coercion-Label e)]
    [(Type-Dyn-Huh (app recur e))
     (Type-Dyn-Huh e)] 
    [(Type-Fn-arg (app recur e) (app recur i))
     (Type-Fn-arg e i)]
    [(Type-Fn-return (app recur e))
     (Type-Fn-return e)]
    [(Type-Fn-arity (app recur e))
     (Type-Fn-arity e)]
    [(Type-Fn-Huh (app recur e))
     (Type-Fn-Huh e)]
    [(Type-GRef-Of (app recur e))
     (Type-GRef-Of e)]
    [(Type-GVect-Of (app recur e))
     (Type-GVect-Of e)]
    [(Type-GRef-Huh (app recur e))
     (Type-GRef-Huh e)]
    [(Type-GVect-Huh (app recur e))
     (Type-GVect-Huh e)]
    [(Type-Tag (app recur e))
     (Type-Tag e)]
    [(Construct t v (app recur* e*))
     (Construct t v e*)]
    [(Access t f (app recur e) i?)
     (Access t f e (if i? (recur i?) #f))]
    [(Check t p (app recur e) (app recur* e*))
     (Check t p e e*)]
    [(If (app recur t) (app recur c) (app recur a))
     (If t c a)]
    [(Switch e c* d)
     (: recur-case : (Switch-Case L1-Expr) -> (Switch-Case L1-Expr))
     (define/match (recur-case x)
       [((cons l r)) (cons l (recur r))])
     (Switch (recur e) (map recur-case c*) (recur d))]
    [(Begin (app recur* e*) (app recur e))
     (Begin e* e)]
    [(Repeat i (app recur e1) (app recur e2) a (app recur e3) (app recur e4))
     (Repeat i e1 e2 a e3 e4)]
    [(Break-Repeat) (Break-Repeat)]
    [(Op p (app recur* e*))
     (Op p e*)]
    [(Blame (app recur e))
     (Blame e)]
    [(Observe (app recur e) t)
     (Observe e t)]
    [(Unguarded-Box (app recur expr))
     (Unguarded-Box expr)]
    [(Unguarded-Box-Ref (app recur expr))
     (Unguarded-Box-Ref expr)]
    [(Unguarded-Box-Set! (app recur expr1) (app recur expr2))
     (Unguarded-Box-Set! expr1 expr2)]
    [(Unguarded-Vect (app recur expr1) (app recur expr2))
     (Unguarded-Vect expr1 expr2)]
    [(Unguarded-Vect-Ref (app recur expr1) (app recur expr2))
     (Unguarded-Vect-Ref expr1 expr2)]
    [(Unguarded-Vect-Set! (app recur expr1) (app recur expr2) (app recur expr3))
     (Unguarded-Vect-Set! expr1 expr2 expr3)]
    [(Guarded-Proxy-Huh (app recur expr))
     (Guarded-Proxy-Huh expr)]
    [(Guarded-Proxy (app recur e1) r)
     (match r
       [(Twosome (app recur e2) (app recur e3) (app recur e4))
        (Guarded-Proxy e1 (Twosome e2 e3 e4))]
       [(Coercion (app recur e2))
        (Guarded-Proxy e1 (Coercion e2))])]
    [(Guarded-Proxy-Ref (app recur expr))
     (Guarded-Proxy-Ref expr)]
    [(Guarded-Proxy-Source (app recur expr))
     (Guarded-Proxy-Source expr)]
    [(Guarded-Proxy-Target (app recur expr))
     (Guarded-Proxy-Target expr)]
    [(Guarded-Proxy-Blames (app recur expr))
     (Guarded-Proxy-Blames expr)]
    [(Guarded-Proxy-Coercion (app recur expr))
     (Guarded-Proxy-Coercion expr)]
    [(Unguarded-Vect-length e) (Unguarded-Vect-length (recur e))]
    [(Mbox (app recur e) t) (Mbox e t)]
    [(Mbox-val-set! (app recur e1) (app recur e2)) (Mbox-val-set! e1 e2)]
    [(Mbox-val-ref (app recur e)) (Mbox-val-ref e)]
    [(Mbox-rtti-set! (app recur addr) (app recur e))
     (Mbox-rtti-set! addr e)]
    [(Mbox-rtti-ref (app recur addr)) (Mbox-rtti-ref addr)]
    [(Make-GLB-Two-Fn-Types e1 (app recur e2) (app recur e3))
     (Make-GLB-Two-Fn-Types e1 e2 e3)]
    [(Make-GLB-Two-Tuple-Types e1 (app recur e2) (app recur e3))
     (Make-GLB-Two-Tuple-Types e1 e2 e3)]
    [(MRef-Coercion-Huh (app recur e)) (MRef-Coercion-Huh e)]
    [(MRef-Coercion-Type (app recur e)) (MRef-Coercion-Type e)]
    [(MRef-Coercion (app recur e)) (MRef-Coercion e)]
    [(Type-GRef (app recur e)) (Type-GRef e)]
    [(Type-GVect (app recur e)) (Type-GVect e)]
    [(Type-MRef (app recur e)) (Type-MRef e)]
    [(Type-MRef-Huh (app recur e)) (Type-MRef-Huh e)]
    [(Type-MRef-Of (app recur e)) (Type-MRef-Of e)]
    [(Mvector (app recur e1) (app recur e2) t) (Mvector e1 e2 t)]
    [(Mvector-length (app recur e)) (Mvector-length e)]
    [(Mvector-val-set! (app recur e1) (app recur e2) (app recur e3)) (Mvector-val-set! e1 e2 e3)]
    [(Mvector-val-ref (app recur e1) (app recur e2)) (Mvector-val-ref e1 e2)]
    [(Mvector-rtti-set! (app recur addr) (app recur e))
     (Mvector-rtti-set! addr e)]
    [(Mvector-rtti-ref (app recur addr)) (Mvector-rtti-ref addr)]
    [(Type-MVect e) (Type-MVect (recur e))]
    [(Type-MVect-Huh e) (Type-MVect-Huh (recur e))]
    [(Type-MVect-Of e) (Type-MVect-Of (recur e))]
    [(MVect-Coercion-Huh e) (MVect-Coercion-Huh (recur e))]
    [(MVect-Coercion-Type e) (MVect-Coercion-Type (recur e))]
    [(MVect-Coercion e) (MVect-Coercion (recur e))]
    [(Error (app recur e)) (Error e)]
    [(Create-tuple e*) (Create-tuple (recur* e*))]
    [(Tuple-proj e i) (Tuple-proj (recur e) (recur i))]
    [(Type-Tuple-Huh e) (Type-Tuple-Huh (recur e))]
    [(Type-Tuple-num e) (Type-Tuple-num (recur e))]
    [(Type-Tuple-item e i) (Type-Tuple-item (recur e) (recur i))]
    [(Make-Tuple-Coercion uid t1 t2 lbl)
     (Make-Tuple-Coercion uid (recur t1) (recur t2) (recur lbl))]
    [(Mediating-Coercion-Huh e) (Mediating-Coercion-Huh (recur e))]
    [(Tuple-Coercion-Huh e) (Tuple-Coercion-Huh (recur e))]
    [(Cast-Tuple uid e1 e2 e3 e4) (Cast-Tuple uid (recur e1) (recur e2) (recur e3) (recur e4))]
    [(Cast-Tuple-In-Place uid e1 e2 e3 e4 e5 e6 e7)
     (Cast-Tuple-In-Place uid (recur e1) (recur e2) (recur e3) (recur e4)
                          (recur e5) (recur e6) (recur e7))]
    [(Coerce-Tuple uid e1 e2) (Coerce-Tuple uid (recur e1) (recur e2))]
    [(Coerce-Tuple-In-Place uid e1 e2 e3 e4 e5)
     (Coerce-Tuple-In-Place uid (recur e1) (recur e2) (recur e3) (recur e4)
                            (recur e5))]
    [other (error 'purify-letrec/replace-ref "unmatched ~a" other)]))


(: pl-expr (L0-Expr -> L1-Expr))
(define (pl-expr expr)
  (: pl-expr* (-> L0-Expr* L1-Expr*))
  (define (pl-expr* e*) (map pl-expr e*))
  (: pl-expr-bnd* (-> L0-Bnd* L1-Bnd*))
  (define (pl-expr-bnd* b*)
    (map (lambda ([b : L0-Bnd])
           (match-let ([(cons i e) b])
             (cons i (pl-expr e))))
         b*))
  (: pl-expr-bnd-code* (-> L0-Bnd-Code* L1-Bnd-Code*))
  (define (pl-expr-bnd-code* b*)
    (map (lambda ([b : L0-Bnd-Code])
           (match-let ([(cons i (Code i* e)) b])
             (cons i (Code i* (pl-expr e)))))
         b*))
  (match expr
    ;; The only-interesting case
    [(Letrec bnd* (app pl-expr expr))
     (define who 'purify-letrec/pl-expr/letrec-matched)
     (define who-return 'purify-letrec/pl-expr/letrec-match-returns)
     (debug who (Letrec bnd* expr))
     (define bound-uid* (list->set (map (inst car Uid L0-Expr) bnd*)))
     ;; TODO casted lambdas are never invoked by there cast
     ;; if we could determine which lambdas are the result of calling
     ;; the cast runtime procedure then we wouldn't have to take a
     ;; performance hit on these.
     ;; One simple way of doing this would be to purify-letrec before
     ;; lowering casts then implement the "fixing letrec" algorithm.
     (: lambda* L1-Bnd-Lambda*)
     (define-values (simple* complex* lambda*)
       (for/fold ([s* : L1-Bnd* '()]
                  [c* : L1-Bnd* '()]
                  [l* : L1-Bnd-Lambda* '()])
                 ([bnd : L0-Bnd bnd*])
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
     (define expr^ : L1-Expr
       (if (null? complex*)
           expr 
           (replace-ref expr setofc* setofl* 'ok)))
     (define simple-bnd* : L1-Bnd* simple*)

     (define (bnd-unitialized-box [c : Uid]) : L1-Bnd
       (cons c (Unguarded-Box (Quote 0))))
     
     (define complex-bnd* : L1-Bnd* 
       (if (null? complex*)
           '()
           (cons (ann (cons i (Unguarded-Box (Quote #f))) L1-Bnd)
                 (debug (map bnd-unitialized-box c*)))))
     ;; Don't traverse lambdas unless there is work to be done
     (define lambda-bnd* : L1-Bnd-Lambda*
       (cond
         [(null? complex*) lambda*]
         [else
          (define (make-lambda-bnd [b : L1-Bnd-Lambda])
            (match-define (cons i e) b)
            ;; references to complex values in lambda bindings is unchecked because
            ;; by the time a function in applied the letrec intialization
            ;; must have completed.
            (cons i (replace-ref-lam e setofc* setofl*)))
          (map make-lambda-bnd lambda*)]))
     
     (define temp-bnd* : L1-Bnd*
       (map (lambda ([t : Uid] [c : L1-Bnd])
              : L1-Bnd
              (match-define (cons _ e) c)
              (cons t (replace-ref e setofc* setofl* i)))
            (debug t*) complex*))
     (define (make-move [c : Uid] [t : Uid]) : L1-Expr
       (Unguarded-Box-Set! (Var c) (Var t)))
     (define set-complex* : L1-Expr*
       (let ([move* (map make-move c* t*)])
         (if (null? complex*)
             '()
             (append move* (list (Unguarded-Box-Set! (Var i) (Quote #t)))))))
     (define return
       (let* ([let-tmps : L1-Expr
                        (debug
                         (cond
                           [(null? temp-bnd*) expr^]
                           [else (Let temp-bnd* (Begin set-complex* expr^))]))]
              
              [let-lambdas : L1-Expr
                           (debug
                            (cond
                              [(null? lambda-bnd*) let-tmps]
                              [else (Letrec lambda-bnd* let-tmps)]))]
              [let-complex : L1-Expr
                           (debug
                            (cond
                              [(null? complex-bnd*) let-lambdas]
                              [else (Let complex-bnd* let-lambdas)]))])
         (debug
          (cond
            [(null? simple*) let-complex]
            [else (Let simple* let-complex)]))))
     (debug who-return return)]
    [(or (Quote _) (Quote-Coercion _) (Type _)) expr]
    [(or  (Code-Label _) (Tag _) (No-Op)) expr]
    [(Code-Label u)
     (Code-Label u)]
    [(Labels (app pl-expr-bnd-code* b*) (app pl-expr e))
     (Labels b* e)]
    [(App-Code (app pl-expr e) (app pl-expr* e*))
     (App-Code e e*)]
    [(Lambda f* (Castable c (app pl-expr e)))
     (Lambda f* (Castable c e))]
    [(Fn-Caster (app pl-expr e))
     (Fn-Caster e)]
    [(App-Fn (app pl-expr e) (app pl-expr* e*))
     (App-Fn e e*)]
    [(App-Fn-or-Proxy u (app pl-expr e) (app pl-expr* e*))
     (App-Fn-or-Proxy u e e*)]
    [(Fn-Proxy i (app pl-expr e1) (app pl-expr e2))
     (Fn-Proxy i e1 e2)]
    [(Fn-Proxy-Huh (app pl-expr e))
     (Fn-Proxy-Huh e)]
    [(Fn-Proxy-Closure (app pl-expr e))
     (Fn-Proxy-Closure e)]
    [(Fn-Proxy-Coercion (app pl-expr e))
     (Fn-Proxy-Coercion e)]
    [(Compose-Coercions (app pl-expr e1) (app pl-expr e2))
     (Compose-Coercions e1 e2)]
    [(HC (app pl-expr p?) (app pl-expr t1) (app pl-expr lbl)
         (app pl-expr i?) (app pl-expr t2)
         (app pl-expr m))
     (HC p? t1 lbl i? t2 m)]
    [(HC-Inject-Huh (app pl-expr h)) (HC-Inject-Huh h)]
    [(HC-Project-Huh (app pl-expr h)) (HC-Project-Huh h)]
    [(HC-Identity-Huh (app pl-expr h)) (HC-Identity-Huh h)]
    [(HC-Label (app pl-expr h)) (HC-Label h)]
    [(HC-T1 (app pl-expr h)) (HC-T1 h)]
    [(HC-T2 (app pl-expr h)) (HC-T2 h)]
    [(HC-Med (app pl-expr h)) (HC-Med h)]
    [(Id-Coercion-Huh (app pl-expr e))
     (Id-Coercion-Huh e)]
    [(Fn-Coercion-Huh (app pl-expr e))
     (Fn-Coercion-Huh e)]
    [(Make-Fn-Coercion u (app pl-expr e1)(app pl-expr e2)(app pl-expr e3))
     (Make-Fn-Coercion u e1 e2 e3)]
    [(Fn-Coercion (app pl-expr* e*)(app pl-expr e))
     (Fn-Coercion e* e)]
    [(Fn-Coercion-Arity (app pl-expr e))
     (Fn-Coercion-Arity e)]
    [(Fn-Coercion-Arg (app pl-expr e1)(app pl-expr e2))
     (Fn-Coercion-Arg e1 e2)]
    [(Fn-Coercion-Return (app pl-expr e))
     (Fn-Coercion-Return e)]
    [(Id-Fn-Coercion (app pl-expr a)) (Id-Fn-Coercion a)]
    [(Fn-Coercion-Arg-Set! (app pl-expr f) (app pl-expr i) (app pl-expr a))
     (Fn-Coercion-Arg-Set! f i a)]
    [(Fn-Coercion-Return-Set! (app pl-expr f) (app pl-expr r))
     (Fn-Coercion-Return-Set! f r)]
    [(Tuple-Coercion-Item-Set! (app pl-expr t) (app pl-expr i) (app pl-expr e))
     (Tuple-Coercion-Item-Set! t i e)]
    [(Id-Tuple-Coercion (app pl-expr a))
     (Id-Tuple-Coercion a)]
    [(Ref-Coercion (app pl-expr e1) (app pl-expr e2) (app pl-expr flag))
     (Ref-Coercion e1 e2 flag)]
    [(Ref-Coercion-Huh (app pl-expr e))
     (Ref-Coercion-Huh e)]
    [(Ref-Coercion-Read (app pl-expr e))
     (Ref-Coercion-Read e)]
    [(Ref-Coercion-Write (app pl-expr e))
     (Ref-Coercion-Write e)]
    [(Ref-Coercion-Ref-Huh (app pl-expr e))
     (Ref-Coercion-Ref-Huh e)]
    [(Sequence-Coercion (app pl-expr e1) (app pl-expr e2))
     (Sequence-Coercion e1 e2)]
    [(Sequence-Coercion-Huh (app pl-expr e))
     (Sequence-Coercion-Huh e)]
    [(Sequence-Coercion-Fst (app pl-expr e))
     (Sequence-Coercion-Fst e)]
    [(Sequence-Coercion-Snd (app pl-expr e))
     (Sequence-Coercion-Snd e)]
    [(Project-Coercion (app pl-expr e1) (app pl-expr e2))
     (Project-Coercion e1 e2)]
    [(Project-Coercion-Huh (app pl-expr e))
     (Project-Coercion-Huh e)]
    [(Project-Coercion-Type (app pl-expr e))
     (Project-Coercion-Type e)]
    [(Project-Coercion-Label (app pl-expr e))
     (Project-Coercion-Label e)]
    [(Inject-Coercion (app pl-expr e))
     (Inject-Coercion e)]
    [(Inject-Coercion-Type (app pl-expr e))
     (Inject-Coercion-Type e)]
    [(Inject-Coercion-Huh (app pl-expr e))
     (Inject-Coercion-Huh e)]
    [(Failed-Coercion (app pl-expr e))
     (Failed-Coercion e)]
    [(Failed-Coercion-Huh (app pl-expr e))
     (Failed-Coercion-Huh e)]
    [(Failed-Coercion-Label (app pl-expr e))
     (Failed-Coercion-Label e)]
    [(Type-Dyn-Huh (app pl-expr e))
     (Type-Dyn-Huh e)] 
    [(Type-Fn-arg (app pl-expr e) (app pl-expr i))
     (Type-Fn-arg e i)]
    [(Type-Fn-return (app pl-expr e))
     (Type-Fn-return e)]
    [(Type-Fn-arity (app pl-expr e))
     (Type-Fn-arity e)]
    [(Type-Fn-Huh (app pl-expr e))
     (Type-Fn-Huh e)]
    [(Type-GRef-Of (app pl-expr e))
     (Type-GRef-Of e)]
    [(Type-GVect-Of (app pl-expr e))
     (Type-GVect-Of e)]
    [(Type-GRef-Huh (app pl-expr e))
     (Type-GRef-Huh e)]
    [(Type-GVect-Huh (app pl-expr e))
     (Type-GVect-Huh e)]
    [(Type-Tag (app pl-expr e))
     (Type-Tag e)]
    [(Tag s)
     (Tag s)]
    [(Construct t v (app pl-expr* e*))
     (Construct t v e*)]
    [(Access t f (app pl-expr e) i?)
     (Access t f e (if i? (pl-expr i?) i?))]
    [(Check t p (app pl-expr e) (app pl-expr* e*))
     (Check t p e e*)]
    [(Let (app pl-expr-bnd* b*) (app pl-expr e))
     (Let b* e)]
    [(Var i)
     (Var i)]
    [(Global s)
     (Global s)]
    [(Assign u/s (app pl-expr e))
     (Assign u/s e)]
    [(If (app pl-expr t) (app pl-expr c) (app pl-expr a))
     (If t c a)]
    [(Switch e c* d)
     (: pl-expr-case : (Switch-Case L0-Expr) -> (Switch-Case L1-Expr))
     (define/match (pl-expr-case c)
       [((cons l r)) (cons l (pl-expr r))])
     (Switch (pl-expr e) (map pl-expr-case c*) (pl-expr d))]
    [(Begin (app pl-expr* e*) (app pl-expr e))
     (Begin e* e)]
    [(Repeat i (app pl-expr e1) (app pl-expr e2) a (app pl-expr e3) (app pl-expr e4))
     (Repeat i e1 e2 a e3 e4)]
    [(Break-Repeat) (Break-Repeat)]
    [(Op p (app pl-expr* e*))
     (Op p e*)]
    [(and nop (No-Op)) nop]
    [(Quote k)
     (Quote k)]
    [(Blame (app pl-expr e))
     (Blame e)]
    [(Observe (app pl-expr e) t)
     (Observe e t)]
    [(Unguarded-Box (app pl-expr expr))
     (Unguarded-Box expr)]
    [(Unguarded-Box-Ref (app pl-expr expr))
     (Unguarded-Box-Ref expr)]
    [(Unguarded-Box-Set! (app pl-expr expr1) (app pl-expr expr2))
     (Unguarded-Box-Set! expr1 expr2)]
    [(Unguarded-Vect (app pl-expr expr1) (app pl-expr expr2))
     (Unguarded-Vect expr1 expr2)]
    [(Unguarded-Vect-Ref (app pl-expr expr1) (app pl-expr expr2))
     (Unguarded-Vect-Ref expr1 expr2)]
    [(Unguarded-Vect-Set! (app pl-expr expr1) (app pl-expr expr2) (app pl-expr expr3))
     (Unguarded-Vect-Set! expr1 expr2 expr3)]
    [(Guarded-Proxy-Huh (app pl-expr expr))
     (Guarded-Proxy-Huh expr)]
    [(Guarded-Proxy (app pl-expr e1) r)
     (match r
       [(Twosome (app pl-expr e2) (app pl-expr e3) (app pl-expr e4))
        (Guarded-Proxy e1 (Twosome e2 e3 e4))]
       [(Coercion (app pl-expr e2))
        (Guarded-Proxy e1 (Coercion e2))])]
    [(Guarded-Proxy-Ref (app pl-expr expr))
     (Guarded-Proxy-Ref expr)]
    [(Guarded-Proxy-Source (app pl-expr expr))
     (Guarded-Proxy-Source expr)]
    [(Guarded-Proxy-Target (app pl-expr expr))
     (Guarded-Proxy-Target expr)]
    [(Guarded-Proxy-Blames (app pl-expr expr))
     (Guarded-Proxy-Blames expr)]
    [(Guarded-Proxy-Coercion (app pl-expr expr))
     (Guarded-Proxy-Coercion expr)]
    [(Unguarded-Vect-length e) (Unguarded-Vect-length (pl-expr e))]
    [(Mbox (app pl-expr e) t) (Mbox e t)]
    [(Mbox-val-set! (app pl-expr e1) (app pl-expr e2)) (Mbox-val-set! e1 e2)]
    [(Mbox-val-ref (app pl-expr e)) (Mbox-val-ref e)]
    [(Mbox-rtti-set! (app pl-expr addr) (app pl-expr e))
     (Mbox-rtti-set! addr e)]
    [(Mbox-rtti-ref (app pl-expr addr)) (Mbox-rtti-ref addr)]
    [(Make-GLB-Two-Fn-Types e1 (app pl-expr e2) (app pl-expr e3))
     (Make-GLB-Two-Fn-Types e1 e2 e3)]
    [(Make-GLB-Two-Tuple-Types e1 (app pl-expr e2) (app pl-expr e3))
     (Make-GLB-Two-Tuple-Types e1 e2 e3)]
    [(MRef-Coercion-Huh (app pl-expr e)) (MRef-Coercion-Huh e)]
    [(MRef-Coercion-Type (app pl-expr e)) (MRef-Coercion-Type e)]
    [(MRef-Coercion (app pl-expr e)) (MRef-Coercion e)]
    [(Type-GRef (app pl-expr e)) (Type-GRef e)]
    [(Type-GVect (app pl-expr e)) (Type-GVect e)]
    [(Type-MRef (app pl-expr e)) (Type-MRef e)]
    [(Type-MRef-Huh (app pl-expr e)) (Type-MRef-Huh e)]
    [(Type-MRef-Of (app pl-expr e)) (Type-MRef-Of e)]
    [(Mvector (app pl-expr e1) (app pl-expr e2) t) (Mvector e1 e2 t)]
    [(Mvector-length (app pl-expr e)) (Mvector-length e)]
    [(Mvector-val-set! (app pl-expr e1) (app pl-expr e2) (app pl-expr e3)) (Mvector-val-set! e1 e2 e3)]
    [(Mvector-val-ref (app pl-expr e1) (app pl-expr e2)) (Mvector-val-ref e1 e2)]
    [(Mvector-rtti-set! (app pl-expr addr) (app pl-expr e))
     (Mvector-rtti-set! addr e)]
    [(Mvector-rtti-ref (app pl-expr addr)) (Mvector-rtti-ref addr)]
    [(Type-MVect e) (Type-MVect (pl-expr e))]
    [(Type-MVect-Huh e) (Type-MVect-Huh (pl-expr e))]
    [(Type-MVect-Of e) (Type-MVect-Of (pl-expr e))]
    [(MVect-Coercion-Huh e) (MVect-Coercion-Huh (pl-expr e))]
    [(MVect-Coercion-Type e) (MVect-Coercion-Type (pl-expr e))]
    [(MVect-Coercion e) (MVect-Coercion (pl-expr e))]
    [(Error (app pl-expr e)) (Error e)]
    [(Create-tuple e*) (Create-tuple (pl-expr* e*))]
    [(Tuple-proj e i) (Tuple-proj (pl-expr e) (pl-expr i))]
    [(Tuple-Coercion-Huh e) (Tuple-Coercion-Huh (pl-expr e))]
    [(Tuple-Coercion-Num e) (Tuple-Coercion-Num (pl-expr e))]
    [(Tuple-Coercion-Item e i) (Tuple-Coercion-Item (pl-expr e) (pl-expr i))]
    [(Coerce-Tuple uid e1 e2) (Coerce-Tuple uid (pl-expr e1) (pl-expr e2))]
    [(Coerce-Tuple-In-Place uid e1 e2 e3 e4 e5)
     (Coerce-Tuple-In-Place uid (pl-expr e1) (pl-expr e2) (pl-expr e3)
                            (pl-expr e4) (pl-expr e5))]
    [(Cast-Tuple uid e1 e2 e3 e4) (Cast-Tuple uid (pl-expr e1) (pl-expr e2) (pl-expr e3) (pl-expr e4))]
    [(Cast-Tuple-In-Place uid e1 e2 e3 e4 e5 e6 e7)
     (Cast-Tuple-In-Place uid (pl-expr e1) (pl-expr e2) (pl-expr e3) (pl-expr e4)
                          (pl-expr e5) (pl-expr e6) (pl-expr e7))]
    [(Type-Tuple-Huh e) (Type-Tuple-Huh (pl-expr e))]
    [(Type-Tuple-num e) (Type-Tuple-num (pl-expr e))]
    [(Type-Tuple-item e i) (Type-Tuple-item (pl-expr e) (pl-expr i))]
    [(Make-Tuple-Coercion uid t1 t2 lbl) (Make-Tuple-Coercion uid (pl-expr t1) (pl-expr t2) (pl-expr lbl))]
    [(Mediating-Coercion-Huh e) (Mediating-Coercion-Huh (pl-expr e))]
    [other (error 'purify-letrec/expr "unmatched ~a" other)]))

#|
[(and (null? complex*) (null? lambda*) (null? simple*)) (return-state exp)]
[(and (null? complex*) (null? simple*)) (return-state (Letrec lambda* exp))]
[(and (null? complex*) (null? lambda*)) (return-state (Let simple* exp))]
[(and (null? simple*) (null? lambda*))
 (return-state
  (Let let-cbnd*
    (Let let-tbnd* (Begin (map f c* t*) exp))))]
[(null? lambda*)
 (return-state
  (Let (append let-cbnd* let-tbnd*) (Begin (map f c* t*) exp)))]
[(null? complex*)
 (return-state
  (Let simple* (Letrec lambda* exp)))]
[(null? simple*)
 (return-state
  (Let let-cbnd*
    (Letrec lambda*
      (Let let-tbnd* (Begin (map f c* t*) exp)))))]
[else
 (return-state
  (Let simple*
    (Let let-cbnd*
      (Letrec lambda*
        (Let let-tbnd* (Begin (map f c* t*) exp))))))]

(let* (
       
       [let-cbnd* (map (inst cons Uid L1-Expr)
                       c*
                       (make-list (length c*) (Gbox (Quote 0))))]
       [let-tbnd* (map (inst cons Uid L1-Expr) t* (map (inst cdr Uid L1-Expr) complex*))])

  [(Lambda f* e) (Lambda f* (recur e))]
  [(Cast exp r)
   (exp : L1-Expr <- (pl-expr exp))
   (match r
     [(Twosome t1 t2 lbl)
      (return-state (Cast exp (Twosome t1 t2 lbl)))]
     [(Coercion c)
      (return-state (Cast exp (Coercion c)))])]

  [(Let bnd* exp)
   (bnd* : L1-Bnd* <- (pl-bnd* bnd*))
   (exp : L1-Expr  <- (pl-expr exp))
   (return-state (Let bnd* exp))]
  [(App exp exp*)
   (exp  : L1-Expr  <- (pl-expr  exp))
   (exp* : L1-Expr* <- (map-state pl-expr exp*))
   (return-state (App exp exp*))]
  [(Op p exp*)
   (exp* : L1-Expr* <- (map-state pl-expr exp*))
   (return-state (Op p exp*))]
  [(If tst csq alt)
   (tst : L1-Expr <- (pl-expr tst))
   (csq : L1-Expr <- (pl-expr csq))
   (alt : L1-Expr <- (pl-expr alt))
   (return-state (If tst csq alt))]
  [(Begin e* e)
   (e* : L1-Expr* <- (map-state pl-expr e*))
   (e  : L1-Expr  <- (pl-expr  e))
   (return-state (Begin e* e))]
  [(Repeat i e1 e2 e3)
   (e1 : L1-Expr <- (pl-expr e1))
   (e2 : L1-Expr <- (pl-expr e2))
   (e3 : L1-Expr <- (pl-expr e3))
   (return-state (Repeat i e1 e2 e3))]
  [(Gbox e) (lift-state (inst Gbox L1-Expr) (pl-expr e))]
  [(Gunbox e) (lift-state (inst Gunbox L1-Expr) (pl-expr e))]
  [(Gbox-set! e1 e2) (lift-state (inst Gbox-set! L1-Expr L1-Expr)
                                 (pl-expr e1) (pl-expr e2))]
  [(Gvector n e) (lift-state (inst Gvector L1-Expr L1-Expr) (pl-expr n) (pl-expr e))]
  [(Gvector-ref e index) (lift-state (inst Gvector-ref L1-Expr L1-Expr) (pl-expr e) (pl-expr index))]
  [(Gvector-set! e1 index e2) (lift-state (inst Gvector-set! L1-Expr L1-Expr L1-Expr)
                                          (pl-expr e1) (pl-expr index) (pl-expr e2))]
  [(Var id)    (return-state (Var id))]
  [(Quote lit) (return-state (Quote lit))]
  
  
  |#
