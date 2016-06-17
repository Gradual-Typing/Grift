#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: compiler/casts/label-lambdas                                             |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
| Description: Moves every lambda into a letrec thus giving it a
| binding. With the new closure conversion steps this pass is now uneeded
| and thus needs to be removed unfortunately it is not entirely evident how to
| do so and I need to make some progress thus letrec-refinement and lambda labeling
| is occuring here. This should be changed. One thought is to perform letrec
| purification earlier. In general I don't like this because letrec is not
| fundemental to the system but is being forced into the center stage. Perhaps
| enforcing the use of letrec only on functions is enough to eliminate this pass
|
+-------------------------------------------------------------------------------+
| Grammer:
+------------------------------------------------------------------------------|#
(require "../helpers.rkt"
         "../errors.rkt"
         "../configuration.rkt"
         "../language/cast-or-coerce3.1.rkt"
         "../language/cast-or-coerce4.rkt")
(provide label-lambdas
         (all-from-out
          "../language/cast-or-coerce3.1.rkt"
          "../language/cast-or-coerce4.rkt"))

(: label-lambdas (Cast-or-Coerce3.1-Lang Config  -> Cast-or-Coerce4-Lang))
(define (label-lambdas prgm comp-config)
  (match-let ([(Prog (list name count type)
                     (Let-Static* tbnd* cbnd* exp)) prgm])
    (let* ([next : (Boxof Nat) (box count)]
           [exp  : CoC4-Expr (ll-expr! next exp)]
           [next : Nat (unbox next)])
      (Prog (list name next type) (Let-Static* tbnd* cbnd* exp)))))

(: ll-expr! ((Boxof Nat) CoC3.1-Expr -> CoC4-Expr))
(define (ll-expr! next exp)
  ;; Get a Unique Identifier
  (: next-uid! (String -> Uid))
  (define (next-uid! x)
    (let ([n (unbox next)])
      (set-box! next (add1 n))
      (Uid x n)))
  ;; ll-let takes the fields of from let and letrecs and pulls all
  ;; bound procedures out into a letrec form. Placing
  ;; the rest as bindings in a let as the body of the let-proc
  ;; every time I look at this code I realize that one of the oddities
  ;; is that in a lexical world this would break horribly but invariants
  ;; about variable renaming prevents this from occuing.
  ;; We should do some closure and letrec optimization soon!
  (: ll-let (CoC3.1-Bnd* CoC3.1-Expr -> CoC4-Expr))
  (define (ll-let b* e)
    ;; split-bound-procedures filters lambdas out of lets
    (: split-bnds (-> CoC3.1-Bnd* (values CoC4-Bnd-Lambda* CoC4-Bnd-Data*)))
    (define (split-bnds b*)
      (for/fold ([bp* : CoC4-Bnd-Lambda* '()]
                 [bd* : CoC4-Bnd-Data*   '()])
                ([b   : CoC3.1-Bnd          b*])
        (match b
          [(cons i (Lambda f* (Castable b e)))
           (let ([bp (cons i (Lambda f* (Castable b (recur e))))])
             (values (cons bp bp*) bd*))]
          [(cons i e)
           (let ([bd : CoC4-Bnd-Data (cons i (recur e))])
             (values bp* (cons bd bd*)))])))
    (let-values ([(bp* bd*) (split-bnds b*)]
                 [(e) (recur e)])
        (cond
          [(and (null? bp*) (null? bd*)) e]
          [(null? bp*) (Let bd* e)]
          [(null? bd*) (Letrec bp* e)]
          [else (Let bd* (Letrec bp* e))])))
  (: recur (CoC3.1-Expr -> CoC4-Expr))
  (define (recur exp)
    (match exp
      ;; The tiny core
      ;; This line should only be reached if the lambda
      ;; is not being bound by a let or a letrec
      ;; There are three interesting cases

      ;; Anonymous Lambdas are given a binding
      [(Lambda f* (Castable ctr e))
       (let ([id (next-uid! "annon")])
         (Letrec `((,id . ,(Lambda f* (Castable ctr (recur e)))))
                 (Var id)))]
      
      ;; Both lets and letrecs may now be treated the same
      [(Letrec b* e) (ll-let b* e)]
      [(Let b* e)    (ll-let b* e)]
      
      ;; And all the boring cases
      [(Op p exp*) (Op p (map recur exp*))]
      [(Type-Tag e) (Type-Tag (recur e))]
      [(Type-Fn-arg e i) (Type-Fn-arg (recur e) (recur i))]
      [(Type-Fn-return e) (Type-Fn-return (recur e))]
      [(Type-Fn-arity e) (Type-Fn-arity (recur e))]
      [(Type-GRef-Of e) (Type-GRef-Of (recur e))]
      [(Type-GVect-Of e) (Type-GVect-Of (recur e))]
      [(Blame e) (Blame (recur e))]
      [(If t c a) (If (recur t) (recur c) (recur a))]
      [(Var i) (Var i)]
      [(Type t) (Type t)]
      [(Quote k) (Quote k)]
      [(Begin exp* exp) (Begin (map recur exp*) (recur exp))]
      [(Repeat i e1 e2 e3) (Repeat i (recur e1) (recur e2) (recur e3))]
      [(Tag s) (Tag s)]
      [(Dyn-tag e) (Dyn-tag (recur e))]
      [(Dyn-immediate e) (Dyn-immediate (recur e))]
      [(Dyn-type e) (Dyn-type (recur e))]
      [(Dyn-value e) (Dyn-value (recur e))]
      [(Dyn-make e1 e2) (Dyn-make (recur e1) (recur e2))]
      ;; newer stuff
      [(Observe e t) (Observe (recur e) t)]
      [(Code-Label u) (Code-Label u)]
      [(Labels c* b) (Labels (map ll-bndc c*) (recur b))]
      [(App-Code e e*) (App-Code (recur e) (map recur e*))]
      [(App-Fn e e*) (App-Fn (recur e) (map recur e*))]
      [(App-Fn-or-Proxy u e e*)
       (App-Fn-or-Proxy u (recur e) (map recur e*))]
      [(Fn-Proxy i e1 e2) (Fn-Proxy i (recur e1) (recur e2))]
      [(Fn-Proxy-Huh e) (Fn-Proxy-Huh (recur e))]
      [(Fn-Proxy-Closure e) (Fn-Proxy-Closure (recur e))]
      [(Fn-Proxy-Coercion e) (Fn-Proxy-Coercion (recur e))]
      [(Fn-Caster e) (Fn-Caster (recur e))]
      ;; Coercions manipulation
      [(Type-Dyn-Huh e) (Type-Dyn-Huh (recur e))]
      [(Type-Fn-Huh e) (Type-Fn-Huh (recur e))]
      [(Type-GRef-Huh e) (Type-GRef-Huh (recur e))]
      [(Type-GVect-Huh e) (Type-GVect-Huh (recur e))]
      [(Quote-Coercion c) (Quote-Coercion c)]
      [(Unguarded-Box exp) (Unguarded-Box (recur exp))]
      [(Unguarded-Box-Ref exp) (Unguarded-Box-Ref (recur exp))]
      [(Unguarded-Box-Set! exp1 exp2)
       (Unguarded-Box-Set! (recur exp1) (recur exp2))]
      [(Unguarded-Vect exp1 exp2)
       (Unguarded-Vect (recur exp1) (recur exp2))]
      [(Unguarded-Vect-Ref exp1 exp2)
       (Unguarded-Vect-Ref (recur exp1) (recur exp2))]
      [(Unguarded-Vect-Set! exp1 exp2 exp3)
       (Unguarded-Vect-Set! (recur exp1) (recur exp2) (recur exp3))]
      [(Guarded-Proxy-Huh exp)
       (Guarded-Proxy-Huh (recur exp))]
      [(Guarded-Proxy (app recur e) r)
       (match r
         [(Twosome t1 t2 l)
          (Guarded-Proxy e (Twosome (recur t1) (recur t2) (recur l)))]
         [(Coercion c)
          (Guarded-Proxy e (Coercion (recur c)))])]
      [(Guarded-Proxy-Ref exp)
       (Guarded-Proxy-Ref (recur exp))]
      [(Guarded-Proxy-Source exp)
       (Guarded-Proxy-Source (recur exp))]
      [(Guarded-Proxy-Target exp)
       (Guarded-Proxy-Target (recur exp))]
      [(Guarded-Proxy-Blames exp)
       (Guarded-Proxy-Blames (recur exp))]
      [(Guarded-Proxy-Coercion exp)
       (Guarded-Proxy-Coercion (recur exp))]
      [(Sequence-Coercion f s)
       (Sequence-Coercion (recur f) (recur s))]
      [(Sequence-Coercion-Fst e)
       (Sequence-Coercion-Fst (recur e))]
      [(Sequence-Coercion-Snd e)
       (Sequence-Coercion-Snd (recur e))]
      [(Sequence-Coercion-Huh e)
       (Sequence-Coercion-Huh (recur e))]
      [(Project-Coercion t l)
       (Project-Coercion (recur t) (recur l))]
      [(Project-Coercion-Huh e)
       (Project-Coercion-Huh (recur e))]
      [(Project-Coercion-Type e)
       (Project-Coercion-Type (recur e))]
      [(Project-Coercion-Label e)
       (Project-Coercion-Label (recur e))]
      [(Inject-Coercion t)
       (Inject-Coercion (recur t))]
      [(Inject-Coercion-Huh e)
       (Inject-Coercion-Huh (recur e))]
      [(Inject-Coercion-Type e)
       (Inject-Coercion-Type (recur e))]
      [(Failed-Coercion l)
       (Failed-Coercion (recur l))]
      [(Failed-Coercion-Huh e)
       (Failed-Coercion-Huh (recur e))]
      [(Failed-Coercion-Label e)
       (Failed-Coercion-Label (recur e))]
      [(Id-Coercion-Huh e)
       (Id-Coercion-Huh (recur e))]
      [(Fn-Coercion e* e)
       (Fn-Coercion (map recur e*) (recur e))]
      [(Make-Fn-Coercion u i t1 t2)
       (Make-Fn-Coercion u (recur i) (recur t1) (recur t2))]
      [(Compose-Fn-Coercion u c1 c2)
       (Compose-Fn-Coercion u (recur c1) (recur c2))]
      [(Fn-Coercion-Huh e)
       (Fn-Coercion-Huh (recur e))]
      [(Fn-Coercion-Arg e1 e2)
       (Fn-Coercion-Arg (recur e1) (recur e2))]
      [(Fn-Coercion-Return e)
       (Fn-Coercion-Return (recur e))]
      [(Ref-Coercion-Huh e)
       (Ref-Coercion-Huh (recur e))]
      [(Ref-Coercion e1 e2)
       (Ref-Coercion (recur e1) (recur e2))]
      [(Ref-Coercion-Read e)
       (Ref-Coercion-Read (recur e))]
      [(Ref-Coercion-Write e)
       (Ref-Coercion-Write (recur e))]  
      [other (error 'll-expr "~a" other)]))
  ;; recur through a code binding
  (: ll-bndc  (CoC3.1-Bnd-Code -> CoC4-Bnd-Code))
  (define (ll-bndc u.c)
    (match-let ([(cons u (Code u* e)) u.c])
      (cons u (Code u* (recur e)))))
  ;; Kickoff the expression mapping
  (recur exp))

