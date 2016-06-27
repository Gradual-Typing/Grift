#lang typed/racket/base

#|------------------------------------------------------------------------------+
|Pass: src/insert-casts                                                         |
+-------------------------------------------------------------------------------+
|Author: Deyaaeldeen Almahallawi (dalmahal@indiana.edu)                         |
+-------------------------------------------------------------------------------+
|Description: This pass rewrites letrec expressions so that, in the output of   |
| the pass, all letrec expressions are "pure," i.e., bind only variables to     |
| lambda expressions.                                                           |
+-------------------------------------------------------------------------------+
|Input Grammar: Cast0-Lang                                                      |
+------------------------------------------------------------------------------|#
(require "../helpers.rkt"
         "../errors.rkt"
         "../configuration.rkt"
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

(: purify-letrec (Lambda0-Lang Config -> Lambda1-Lang))
(define (purify-letrec prgm comp-config)
  (match-define (Prog (list prgm-name prgm-next prgm-type)
                  (Let-Static* prgm-type-bnd* prgm-crcn-bnd* prgm-expr))
    prgm)

  (define unique (make-unique-counter prgm-next))
  
  (define expr/pure-letrec (pl-expr (make-next-uid! unique) prgm-expr))

  (Prog (list prgm-name (unique-counter-next! unique) prgm-type)
    (Let-Static* prgm-type-bnd* prgm-crcn-bnd* expr/pure-letrec)))



#;(: gen-uid* (Nat -> (State Nat (Listof Uid))))
#;
(define ((gen-uid* n) next)
  (let ([f : (Nat -> Uid) (lambda (n) (Uid "purifiedbinds" n))])
    (values (map f (range next (+ next n))) (+ next n))))

#;(: simple?* ((Listof L0-Expr) Uid* Integer Boolean -> Boolean))
#;
(define (simple?* exp uid* depth outer-lambda?)
  (if (simple? (car exp) uid* depth outer-lambda?)
      (simple?* (cdr exp) uid* depth outer-lambda?)
      #f))

;; TODO: is it possible to merge simple? and pl-expr to make only one
;; pass over the AST?
;; TODO Uid* should be a hash-set
(: simple? (L0-Expr (Setof Uid) Integer Boolean -> Boolean))
(define (simple? expr uid* depth outer-lambda?)
  
  (define (recur [x : L0-Expr])
    (simple? x uid* (add1 depth) outer-lambda?))

  (define (recur* [x* : L0-Expr*])
    (andmap recur x*))
  
  (: recur-all : (->* () #:rest L0-Expr Boolean))
  (define (recur-all . x*)
    (recur* x*))
  
  (match expr
    ;; The really interesting choices
    [(Var x) (set-member? uid* x)]
    [(Lambda _ (Castable ctr e))
     (if (> depth 0)
         (simple? e uid* (+ 1 depth) #t)
         #f)]
    [(App-Fn e e*) 
     (if outer-lambda?
         (and (recur e) (recur* e*))
         #f)]
    [(App-Fn-or-Proxy u e e*)
     (if outer-lambda?
         (and (recur e) (recur* e*))
         #f)]
    ;; Constant data is simple
    [(or (Quote _) (Quote-Coercion _) (Type _) (Code-Label _) (Tag _)) #t]
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
    [(Begin e* e)  (and (recur e) (recur* e*))]
    [(Repeat i e1 e2 e3) (recur-all e1 e2 e3)]
    [(App-Code e e*) (and (recur e) (recur* e*))]
    [(Lambda f* (Castable c e)) (recur e)]
    [(Fn-Caster e) (recur e)]
    [(Fn-Proxy i e1 e2) (recur-all e1 e2)]
    [(Fn-Proxy-Huh e) (recur e)]
    [(Fn-Proxy-Closure e) (recur e)]
    [(Fn-Proxy-Coercion e) (recur e)]
    [(Compose-Coercions e1 e2) (recur-all e1 e2)]
    [(Id-Coercion-Huh e) (recur e)]
    [(Fn-Coercion-Huh e) (recur e)]
    [(Make-Fn-Coercion u e1 e2 e3) (recur-all e1 e2 e3)]
    [(Compose-Fn-Coercion u e1 e2) (recur-all e1 e2)]
    [(Fn-Coercion e* e) (and (recur e) (recur* e*))]
    [(Fn-Coercion-Arg e1 e2) (recur-all e1 e2)]
    [(Fn-Coercion-Return e) (recur e)]
    [(Ref-Coercion e1 e2) (recur-all e1 e2)]
    [(Ref-Coercion-Huh e) (recur e)]
    [(Ref-Coercion-Read e) (recur e)]
    [(Ref-Coercion-Write e) (recur e)]
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
    [(Dyn-tag e) (recur e)]
    [(Dyn-immediate e) (recur e)]
    [(Dyn-type e) (recur e)]
    [(Dyn-value e) (recur e)]
    [(Dyn-make e1 e2) (recur-all e1 e2)]
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
    [other (error 'purify-letrec/simple? "unmatched ~a" other)]))

;; Computes the intersection between two lists and the list of elements
;; that is in the first list but not in the second.
(: diff1-intersect (L1-Bnd-Lambda* Uid* -> (Values L1-Bnd-Lambda* L1-Bnd-Lambda*)))
(define (diff1-intersect l1 l2)
  (for/fold ([no* : L1-Bnd-Lambda* '()]
             [yes* : L1-Bnd-Lambda* '()])
            ([a : L1-Bnd-Lambda l1])
    (match-let ([(cons i e) a])
      (if (memq i l2)
          (values no* (cons a yes*))
          (values (cons a no*) yes*)))))

;; Computes the list of elements in the second list but not in the first.
(: diff2 (Uid* Uid* -> Uid*))
(define (diff2 l1 l2)
  (let-values ([(l) (for/fold ([no* : Uid* '()])
                              ([a : Uid l2])
                      (if (memq a l1)
                          (values no*)
                          (values (cons a no*))))])
    l))

(: replace-ref-lam (L1-Lambda (Setof Uid) -> L1-Lambda))
(define (replace-ref-lam expr v*)
  (match-let ([(Lambda f* (Castable ctr? e)) expr])
    (Lambda f* (Castable ctr? (replace-ref e v*)))))

(: replace-ref (L1-Expr (Setof Uid) -> L1-Expr))
(define (replace-ref expr v*)
  (define (recur [e : L1-Expr])
    (replace-ref e v*))
  (define (recur* [e* : L1-Expr*])
    (map recur e*))
  (match expr
    ;;
    [(Var x)
     ;; TODO this is broken place checks to make sure it is not
     ;; possible to get ahold of uninitialized references.
     (if (set-member? v* x)
         (Unguarded-Box-Ref (Var x))
         (Var x))]
    ;; Every other case is just a boring flow agnostic tree traversal
    [(or (Quote _) (Quote-Coercion _) (Type _) (Code-Label _) (Tag _)) expr]
    [(Lambda f* (Castable ctr e)) (Lambda f* (Castable ctr (recur e)))]
    ;; TODO examine this code and ensure it isn't just doing
    ;; wasted computation we are using unique identifiers
    ;; their shouldn't be binding overlaps
    [(Letrec b* e)
     (define (recur-bnd-lambda [b : L1-Bnd-Lambda])
       (match-let ([(cons i (Lambda i* (Castable c? e))) e])
         (cons i (Lambda i* (Castable c? (recur e))))))
     (Letrec (map recur-bnd-lambda b*) (recur e))
     #;;; Get rid of this once we know it works
     (let-values ([(no* yes*) (diff1-intersect bnd* v*)])
       (let* ([nob* (map (inst car Uid L1-Lambda) no*)]
              [no* (map (lambda ([e : L1-Lambda]) : L1-Lambda
                                (replace-ref-lam e v*))
                        (map (inst cdr Uid L1-Lambda) no*))]
              [no* (map (inst cons Uid L1-Lambda) nob* no*)]
              [v* (diff2 (map (inst car Uid L1-Lambda) bnd*) v*)]
              [e (if (> (length v*) 0) (replace-ref e v*) e)])
         (Letrec (append no* yes*) e)))]
    [(Let b* e)
     (define (recur-bnd [b : L1-Bnd])
       (match-let ([(cons i e) b])
         (cons i (recur e))))
     (Let (map recur-bnd b*) (recur e))
     #;;; Get rid of this once we know it works
     (let* ([b* (map (inst car Uid L1-Expr) bnd*)]
            [bnd* (map (inst cons Uid L1-Expr) b* (map recur/env (map (inst cdr Uid L1-Expr) bnd*)))]
            [v* (diff2 b* v*)]
            [e (if (> (length v*) 0) (replace-ref e v*) e)])
       (Let bnd* e))]
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
    [(Id-Coercion-Huh (app recur e))
     (Id-Coercion-Huh e)]
    [(Fn-Coercion-Huh (app recur e))
     (Fn-Coercion-Huh e)]
    [(Make-Fn-Coercion u (app recur e1)(app recur e2)(app recur e3))
     (Make-Fn-Coercion u e1 e2 e3)]
    [(Compose-Fn-Coercion u (app recur e1) (app recur e2))
     (Compose-Fn-Coercion u e1 e2)]
    [(Fn-Coercion (app recur* e*)(app recur e))
     (Fn-Coercion e* e)]
    [(Fn-Coercion-Arg (app recur e1)(app recur e2))
     (Fn-Coercion-Arg e1 e2)]
    [(Fn-Coercion-Return (app recur e))
     (Fn-Coercion-Return e)]
    [(Ref-Coercion (app recur e1) (app recur e2))
     (Ref-Coercion e1 e2)]
    [(Ref-Coercion-Huh (app recur e))
     (Ref-Coercion-Huh e)]
    [(Ref-Coercion-Read (app recur e))
     (Ref-Coercion-Read e)]
    [(Ref-Coercion-Write (app recur e))
     (Ref-Coercion-Write e)]
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
    [(Dyn-tag (app recur e))
     (Dyn-tag e)]
    [(Dyn-immediate (app recur e))
     (Dyn-immediate e)]
    [(Dyn-type (app recur e))
     (Dyn-type e)]
    [(Dyn-value (app recur e))
     (Dyn-value e)]
    [(Dyn-make (app recur e1) (app recur e2))
     (Dyn-make e1 e2)]
    [(If (app recur t) (app recur c) (app recur a))
     (If t c a)]
    [(Begin (app recur* e*) (app recur e))
     (Begin e* e)]
    [(Repeat i (app recur e1) (app recur e2) (app recur e3))
     (Repeat i e1 e2 e3)]
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
    [other (error 'purify-letrec/replace-ref "unmatched ~a" other)]))




(: pl-expr ((String -> Uid) L0-Expr -> L1-Expr))
(define (pl-expr next-uid! expr)
  
  (: recur : L0-Expr -> L1-Expr)
  (define (recur expr)
    (match expr
      ;; The only-interesting case
      [(Letrec bnd* (app recur expr))
       (define bound-uid* (list->set (map (inst car Uid L0-Expr) bnd*)))
       (define-values (simple* complex* lambda*)
         (for/fold ([s* : L1-Bnd* '()]
                    [c* : L1-Bnd* '()]
                    [l* : L1-Bnd-Lambda* '()])
                   ([bnd : L0-Bnd bnd*])
           (match bnd
             [(cons i (Lambda f* (Castable ctr (app recur expr)))) 
              (values s* c* `([,i . ,(Lambda f* (Castable ctr expr))] . ,l*))]
             [(cons i (app recur expr))
              (cond
                [(simple? expr bound-uid* 0 #f)
                 (values `([,i . ,expr] . ,s*) c* l*)]
                [else (values s* `([,i . ,expr] . ,c*) l*)])])))

       (define t* : Uid* (map (lambda (x) (next-uid! "tmp")) complex*))
       (define c* : Uid* (map (inst car Uid Any) complex*))
       (define setofc* : (Setof Uid) (list->set c*))
       (define expr^ : L1-Expr 
         (if (null? complex*) expr (replace-ref expr setofc*)))
       (define simple-bnd* : L1-Bnd* simple*)
       (define complex-bnd*
         (for/list : (Listof L1-Bnd) ([c : Uid c*])
           (cons c (Unguarded-Box (Quote 0)))))
       (define lambda-bnd*
         (cond
           [(null? complex*) lambda*]
           [else
            (for/list : (Listof L1-Bnd-Lambda)
                      ([b : L1-Bnd-Lambda lambda*])
              (match-define (cons i e) b)
              (cons i (replace-ref-lam e setofc*)))]))
       (define temp-bnd* : L1-Bnd*
         (map (lambda ([t : Uid] [c : L1-Bnd])
                : L1-Bnd
                (match-define (cons _ e) c)
                (cons t (replace-ref e setofc*)))
              t* complex*))
       (define set-complex* : L1-Expr*
         (map (lambda ([c : Uid] [t : Uid])
                (Unguarded-Box-Set! (Var c) (Var t)))
              c* t*))
       (let* ([let-tmps : L1-Expr
               (cond
                 [(null? temp-bnd*) expr^]
                 [else (Let temp-bnd* (Begin set-complex* expr^))])]
              [let-lambdas : L1-Expr
               (cond
                 [(null? lambda-bnd*) let-tmps]
                 [else (Letrec lambda-bnd* let-tmps)])]
              [let-complex : L1-Expr
               (cond
                 [(null? complex-bnd*) let-lambdas]
                 [else (Let complex-bnd* let-lambdas)])])
         (cond
           [(null? simple*) let-complex]
           [else (Let simple* let-complex)]))]
      [(or (Quote _) (Quote-Coercion _) (Type _) (Code-Label _) (Tag _)) expr]
      [(Code-Label u)
       (Code-Label u)]
      [(Labels (app recur-bnd-code* b*) (app recur e))
       (Labels b* e)]
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
      [(Id-Coercion-Huh (app recur e))
       (Id-Coercion-Huh e)]
      [(Fn-Coercion-Huh (app recur e))
       (Fn-Coercion-Huh e)]
      [(Make-Fn-Coercion u (app recur e1)(app recur e2)(app recur e3))
       (Make-Fn-Coercion u e1 e2 e3)]
      [(Compose-Fn-Coercion u (app recur e1) (app recur e2))
       (Compose-Fn-Coercion u e1 e2)]
      [(Fn-Coercion (app recur* e*)(app recur e))
       (Fn-Coercion e* e)]
      [(Fn-Coercion-Arg (app recur e1)(app recur e2))
       (Fn-Coercion-Arg e1 e2)]
      [(Fn-Coercion-Return (app recur e))
       (Fn-Coercion-Return e)]
      [(Ref-Coercion (app recur e1) (app recur e2))
       (Ref-Coercion e1 e2)]
      [(Ref-Coercion-Huh (app recur e))
       (Ref-Coercion-Huh e)]
      [(Ref-Coercion-Read (app recur e))
       (Ref-Coercion-Read e)]
      [(Ref-Coercion-Write (app recur e))
       (Ref-Coercion-Write e)]
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
      [(Tag s)
       (Tag s)]
      [(Dyn-tag (app recur e))
       (Dyn-tag e)]
      [(Dyn-immediate (app recur e))
       (Dyn-immediate e)]
      [(Dyn-type (app recur e))
       (Dyn-type e)]
      [(Dyn-value (app recur e))
       (Dyn-value e)]
      [(Dyn-make (app recur e1) (app recur e2))
       (Dyn-make e1 e2)]
      [(Let (app recur-bnd* b*) (app recur e))
       (Let b* e)]
      [(Var i)
       (Var i)]
      [(If (app recur t) (app recur c) (app recur a))
       (If t c a)]
      [(Begin (app recur* e*) (app recur e))
       (Begin e* e)]
      [(Repeat i (app recur e1) (app recur e2) (app recur e3))
       (Repeat i e1 e2 e3)]
      [(Op p (app recur* e*))
       (Op p e*)]
      [(Quote k)
       (Quote k)]
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
      [other (error 'purify-letrec/expr "unmatched ~a" other)]))

  (: recur* (-> L0-Expr* L1-Expr*))
  (define (recur* e*) (map recur e*))
  
  (: recur-bnd* (-> L0-Bnd* L1-Bnd*))
  (define (recur-bnd* b*)
    (map (lambda ([b : L0-Bnd])
           (match-let ([(cons i e) b])
             (cons i (recur e))))
         b*))

  (: recur-bnd-code* (-> L0-Bnd-Code* L1-Bnd-Code*))
  (define (recur-bnd-code* b*)
    (map (lambda ([b : L0-Bnd-Code])
           (match-let ([(cons i (Code i* e)) b])
             (cons i (Code i* (recur e)))))
         b*))
  
  (recur expr))

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

