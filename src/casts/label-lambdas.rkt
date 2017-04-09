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
         "../unique-counter.rkt"
         "../unique-identifiers.rkt"
         "../language/forms.rkt"
         "../language/cast-or-coerce3.1.rkt"
         "../language/cast-or-coerce4.rkt")
(provide label-lambdas
         (all-from-out
          "../language/cast-or-coerce3.1.rkt"
          "../language/cast-or-coerce4.rkt"))

(: label-lambdas (Cast-or-Coerce3.1-Lang  -> Cast-or-Coerce4-Lang))
(define (label-lambdas prgm)
  (match-define (Prog (list n c t) (Let-Static* tb* cb* e)) prgm)
  (define uc (make-unique-counter c))
  (define new-exp
    (parameterize ([current-unique-counter uc])
      (ll-expr e)))
  (Prog (list n (unique-counter-next! uc) t)
    (Let-Static* tb* cb* new-exp)))

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
         (let ([bp (cons i (Lambda f* (Castable b (ll-expr e))))])
           (values (cons bp bp*) bd*))]
        [(cons i e)
         (let ([bd : CoC4-Bnd-Data (cons i (ll-expr e))])
           (values bp* (cons bd bd*)))])))
  (let-values ([(bp* bd*) (split-bnds b*)]
               [(e) (ll-expr e)])
    (cond
      [(and (null? bp*) (null? bd*)) e]
      [(null? bp*) (Let bd* e)]
      [(null? bd*) (Letrec bp* e)]
      [else (Let bd* (Letrec bp* e))])))


(: ll-expr (CoC3.1-Expr -> CoC4-Expr))
(define (ll-expr exp)
   (match exp
      ;; The tiny core
      ;; This line should only be reached if the lambda
      ;; is not being bound by a let or a letrec
      ;; There are three interesting cases

      ;; Anonymous Lambdas are given a binding
      [(Lambda f* (Castable ctr e))
       (let ([id (next-uid! "annon")])
         (Letrec `((,id . ,(Lambda f* (Castable ctr (ll-expr e)))))
                 (Var id)))]
      
      ;; Both lets and letrecs may now be treated the same
      [(Letrec b* e) (ll-let b* e)]
      [(Let b* e)    (ll-let b* e)]
      
      ;; And all the boring cases
      [(and nop (No-Op)) nop]
      [(Op p exp*) (Op p (map ll-expr exp*))]
      [(Type-Tag e) (Type-Tag (ll-expr e))]
      [(Type-Fn-arg e i) (Type-Fn-arg (ll-expr e) (ll-expr i))]
      [(Type-Fn-return e) (Type-Fn-return (ll-expr e))]
      [(Type-Fn-arity e) (Type-Fn-arity (ll-expr e))]
      [(Type-GRef-Of e) (Type-GRef-Of (ll-expr e))]
      [(Type-GVect-Of e) (Type-GVect-Of (ll-expr e))]
      [(Blame e) (Blame (ll-expr e))]
      [(If t c a) (If (ll-expr t) (ll-expr c) (ll-expr a))]
      [(Switch e c* d)
       (Switch (ll-expr e) (map-switch-case* ll-expr c*) (ll-expr d))]
      [(Var i) (Var i)]
      [(Type t) (Type t)]
      [(Quote k) (Quote k)]
      [(Begin exp* exp) (Begin (map ll-expr exp*) (ll-expr exp))]
      [(Repeat i e1 e2 a e3 e4) (Repeat i (ll-expr e1) (ll-expr e2) a (ll-expr e3) (ll-expr e4))]
      [(Break-Repeat) (Break-Repeat)]
      [(Tag s) (Tag s)]
      [(Dyn-tag e) (Dyn-tag (ll-expr e))]
      [(Dyn-immediate e) (Dyn-immediate (ll-expr e))]
      [(Dyn-type e) (Dyn-type (ll-expr e))]
      [(Dyn-value e) (Dyn-value (ll-expr e))]
      [(Dyn-make e1 e2) (Dyn-make (ll-expr e1) (ll-expr e2))]
      ;; newer stuff
      [(Observe e t) (Observe (ll-expr e) t)]
      [(Code-Label u) (Code-Label u)]
      [(Labels c* b) (Labels (map ll-bndc c*) (ll-expr b))]
      [(App-Code e e*) (App-Code (ll-expr e) (map ll-expr e*))]
      [(App-Fn e e*) (App-Fn (ll-expr e) (map ll-expr e*))]
      [(App-Fn-or-Proxy u e e*)
       (App-Fn-or-Proxy u (ll-expr e) (map ll-expr e*))]
      [(Fn-Proxy i e1 e2) (Fn-Proxy i (ll-expr e1) (ll-expr e2))]
      [(Fn-Proxy-Huh e) (Fn-Proxy-Huh (ll-expr e))]
      [(Fn-Proxy-Closure e) (Fn-Proxy-Closure (ll-expr e))]
      [(Fn-Proxy-Coercion e) (Fn-Proxy-Coercion (ll-expr e))]
      [(Fn-Caster e) (Fn-Caster (ll-expr e))]
      ;; Coercions manipulation
      [(HC (app ll-expr p?) (app ll-expr t1) (app ll-expr lbl)
           (app ll-expr i?) (app ll-expr t2)
           (app ll-expr m))
       (HC p? t1 lbl i? t2 m)]
      [(HC-Inject-Huh (app ll-expr h)) (HC-Inject-Huh h)]
      [(HC-Project-Huh (app ll-expr h)) (HC-Project-Huh h)]
      [(HC-Identity-Huh (app ll-expr h)) (HC-Identity-Huh h)]
      [(HC-Label (app ll-expr h)) (HC-Label h)]
      [(HC-T1 (app ll-expr h)) (HC-T1 h)]
      [(HC-T2 (app ll-expr h)) (HC-T2 h)]
      [(HC-Med (app ll-expr h)) (HC-Med h)]
      [(Type-Dyn-Huh e) (Type-Dyn-Huh (ll-expr e))]
      [(Type-Fn-Huh e) (Type-Fn-Huh (ll-expr e))]
      [(Type-GRef-Huh e) (Type-GRef-Huh (ll-expr e))]
      [(Type-GVect-Huh e) (Type-GVect-Huh (ll-expr e))]
      [(Quote-Coercion c) (Quote-Coercion c)]
      [(Unguarded-Box exp) (Unguarded-Box (ll-expr exp))]
      [(Unguarded-Box-Ref exp) (Unguarded-Box-Ref (ll-expr exp))]
      [(Unguarded-Box-Set! exp1 exp2)
       (Unguarded-Box-Set! (ll-expr exp1) (ll-expr exp2))]
      [(Unguarded-Vect exp1 exp2)
       (Unguarded-Vect (ll-expr exp1) (ll-expr exp2))]
      [(Unguarded-Vect-Ref exp1 exp2)
       (Unguarded-Vect-Ref (ll-expr exp1) (ll-expr exp2))]
      [(Unguarded-Vect-Set! exp1 exp2 exp3)
       (Unguarded-Vect-Set! (ll-expr exp1) (ll-expr exp2) (ll-expr exp3))]
      [(Guarded-Proxy-Huh exp)
       (Guarded-Proxy-Huh (ll-expr exp))]
      [(Guarded-Proxy (app ll-expr e) r)
       (match r
         [(Twosome t1 t2 l)
          (Guarded-Proxy e (Twosome (ll-expr t1) (ll-expr t2) (ll-expr l)))]
         [(Coercion c)
          (Guarded-Proxy e (Coercion (ll-expr c)))])]
      [(Guarded-Proxy-Ref exp)
       (Guarded-Proxy-Ref (ll-expr exp))]
      [(Guarded-Proxy-Source exp)
       (Guarded-Proxy-Source (ll-expr exp))]
      [(Guarded-Proxy-Target exp)
       (Guarded-Proxy-Target (ll-expr exp))]
      [(Guarded-Proxy-Blames exp)
       (Guarded-Proxy-Blames (ll-expr exp))]
      [(Guarded-Proxy-Coercion exp)
       (Guarded-Proxy-Coercion (ll-expr exp))]
      [(Sequence-Coercion f s)
       (Sequence-Coercion (ll-expr f) (ll-expr s))]
      [(Sequence-Coercion-Fst e)
       (Sequence-Coercion-Fst (ll-expr e))]
      [(Sequence-Coercion-Snd e)
       (Sequence-Coercion-Snd (ll-expr e))]
      [(Sequence-Coercion-Huh e)
       (Sequence-Coercion-Huh (ll-expr e))]
      [(Project-Coercion t l)
       (Project-Coercion (ll-expr t) (ll-expr l))]
      [(Project-Coercion-Huh e)
       (Project-Coercion-Huh (ll-expr e))]
      [(Project-Coercion-Type e)
       (Project-Coercion-Type (ll-expr e))]
      [(Project-Coercion-Label e)
       (Project-Coercion-Label (ll-expr e))]
      [(Inject-Coercion t)
       (Inject-Coercion (ll-expr t))]
      [(Inject-Coercion-Huh e)
       (Inject-Coercion-Huh (ll-expr e))]
      [(Inject-Coercion-Type e)
       (Inject-Coercion-Type (ll-expr e))]
      [(Failed-Coercion l)
       (Failed-Coercion (ll-expr l))]
      [(Failed-Coercion-Huh e)
       (Failed-Coercion-Huh (ll-expr e))]
      [(Failed-Coercion-Label e)
       (Failed-Coercion-Label (ll-expr e))]
      [(Id-Coercion-Huh e)
       (Id-Coercion-Huh (ll-expr e))]
      [(Fn-Coercion e* e)
       (Fn-Coercion (map ll-expr e*) (ll-expr e))]
      [(Fn-Coercion-Arity (app ll-expr e))
       (Fn-Coercion-Arity e)]
      [(Make-Fn-Coercion u i t1 t2)
       (Make-Fn-Coercion u (ll-expr i) (ll-expr t1) (ll-expr t2))]
      [(Compose-Fn-Coercion u c1 c2)
       (Compose-Fn-Coercion u (ll-expr c1) (ll-expr c2))]
      [(Fn-Coercion-Huh e)
       (Fn-Coercion-Huh (ll-expr e))]
      [(Fn-Coercion-Arg e1 e2)
       (Fn-Coercion-Arg (ll-expr e1) (ll-expr e2))]
      [(Fn-Coercion-Return e)
       (Fn-Coercion-Return (ll-expr e))]
      [(Id-Fn-Coercion (app ll-expr a)) (Id-Fn-Coercion a)]
      [(Fn-Coercion-Arg-Set! (app ll-expr f) (app ll-expr i) (app ll-expr a))
       (Fn-Coercion-Arg-Set! f i a)]
      [(Fn-Coercion-Return-Set! (app ll-expr f) (app ll-expr r))
       (Fn-Coercion-Return-Set! f r)]
      [(Tuple-Coercion-Item-Set! (app ll-expr t) (app ll-expr i) (app ll-expr e))
       (Tuple-Coercion-Item-Set! t i e)]
      [(Id-Tuple-Coercion (app ll-expr a))
       (Id-Tuple-Coercion a)]
      [(Ref-Coercion-Huh e)
       (Ref-Coercion-Huh (ll-expr e))]
      [(Ref-Coercion e1 e2)
       (Ref-Coercion (ll-expr e1) (ll-expr e2))]
      [(Ref-Coercion-Read e)
       (Ref-Coercion-Read (ll-expr e))]
      [(Ref-Coercion-Write e)
       (Ref-Coercion-Write (ll-expr e))]
      [(Mbox (app ll-expr e) t) (Mbox e t)]
      [(Mbox-val-set! (app ll-expr e1) (app ll-expr e2)) (Mbox-val-set! e1 e2)]
      [(Mbox-val-ref (app ll-expr e)) (Mbox-val-ref e)]
      [(Mbox-rtti-set! (app ll-expr addr) (app ll-expr e))
       (Mbox-rtti-set! addr e)]
      [(Mbox-rtti-ref (app ll-expr addr)) (Mbox-rtti-ref addr)]
      [(Make-Fn-Type e1 (app ll-expr e2) (app ll-expr e3))
       (Make-Fn-Type e1 e2 e3)]
      [(Make-Tuple-Type e1 (app ll-expr e2) (app ll-expr e3))
       (Make-Tuple-Type e1 e2 e3)]
      [(MRef-Coercion-Huh (app ll-expr e)) (MRef-Coercion-Huh e)]
      [(MRef-Coercion-Type (app ll-expr e)) (MRef-Coercion-Type e)]
      [(MRef-Coercion (app ll-expr e)) (MRef-Coercion e)]
      [(Type-GRef (app ll-expr e)) (Type-GRef e)]
      [(Type-GVect (app ll-expr e)) (Type-GVect e)]
      [(Type-MRef (app ll-expr e)) (Type-MRef e)]
      [(Type-MRef-Huh (app ll-expr e)) (Type-MRef-Huh e)]
      [(Type-MRef-Of (app ll-expr e)) (Type-MRef-Of e)]
      [(Mvector (app ll-expr e1) (app ll-expr e2) t) (Mvector e1 e2 t)]
      [(Mvector-length (app ll-expr e)) (Mvector-length e)]
      [(Unguarded-Vect-length e) (Unguarded-Vect-length (ll-expr e))]
      [(Mvector-val-set! (app ll-expr e1) (app ll-expr e2) (app ll-expr e3)) (Mvector-val-set! e1 e2 e3)]
      [(Mvector-val-ref (app ll-expr e1) (app ll-expr e2)) (Mvector-val-ref e1 e2)]
      [(Mvector-rtti-set! (app ll-expr addr) (app ll-expr e))
       (Mvector-rtti-set! addr e)]
      [(Mvector-rtti-ref (app ll-expr addr)) (Mvector-rtti-ref addr)]
      [(Type-MVect e) (Type-MVect (ll-expr e))]
      [(Type-MVect-Huh e) (Type-MVect-Huh (ll-expr e))]
      [(Type-MVect-Of e) (Type-MVect-Of (ll-expr e))]
      [(MVect-Coercion-Huh e) (MVect-Coercion-Huh (ll-expr e))]
      [(MVect-Coercion-Type e) (MVect-Coercion-Type (ll-expr e))]
      [(MVect-Coercion e) (MVect-Coercion (ll-expr e))]
      [(Error (app ll-expr e)) (Error e)]
      [(Create-tuple e*) (Create-tuple (map ll-expr e*))]
      [(Copy-Tuple n v)
       (Copy-Tuple (ll-expr n) (ll-expr v))]
      [(Tuple-proj e i) (Tuple-proj (ll-expr e) (ll-expr i))]
      [(Tuple-Coercion-Huh e) (Tuple-Coercion-Huh (ll-expr e))]
      [(Tuple-Coercion-Num e) (Tuple-Coercion-Num (ll-expr e))]
      [(Tuple-Coercion-Item e i) (Tuple-Coercion-Item (ll-expr e) (ll-expr i))]
      [(Coerce-Tuple uid e1 e2) (Coerce-Tuple uid (ll-expr e1) (ll-expr e2))]
      [(Coerce-Tuple-In-Place uid e1 e2 e3)
       (Coerce-Tuple-In-Place uid (ll-expr e1) (ll-expr e2) (ll-expr e3))]
      [(Cast-Tuple uid e1 e2 e3 e4) (Cast-Tuple uid (ll-expr e1) (ll-expr e2) (ll-expr e3) (ll-expr e4))]
      [(Cast-Tuple-In-Place uid e1 e2 e3 e4 e5)
       (Cast-Tuple-In-Place uid (ll-expr e1) (ll-expr e2) (ll-expr e3) (ll-expr e4) (ll-expr e5))]
      [(Type-Tuple-Huh e) (Type-Tuple-Huh (ll-expr e))]
      [(Type-Tuple-num e) (Type-Tuple-num (ll-expr e))]
      [(Type-Tuple-item e i) (Type-Tuple-item (ll-expr e) (ll-expr i))]
      [(Make-Tuple-Coercion uid t1 t2 lbl) (Make-Tuple-Coercion uid (ll-expr t1) (ll-expr t2) (ll-expr lbl))]
      [(Compose-Tuple-Coercion uid e1 e2) (Compose-Tuple-Coercion uid (ll-expr e1) (ll-expr e2))]
      [(Mediating-Coercion-Huh e) (Mediating-Coercion-Huh (ll-expr e))]
      [other (error 'll-expr "~a" other)]))

;; recur through a code binding
(: ll-bndc  (CoC3.1-Bnd-Code -> CoC4-Bnd-Code))
(define (ll-bndc u.c)
  (match-let ([(cons u (Code u* e)) u.c])
    (cons u (Code u* (ll-expr e)))))

 

