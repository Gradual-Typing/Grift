#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: src/casts/lower-function-casts                                           |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
 Discription:
 This pass adds the castable form which extends all lambdas that
 are castable with a free variable which represents the function that is
 necissary in order to cast the closure created by this lambda. It can now be
 extracted from a closure value using the fn-cast form. Note that lambdas
 created in the process of creating these casting functions are not castable.
 Though their return values are.

 Casts of the form (runtime-cast value-exp type1-exp type2-exp label-exp) are
 used in each of the higher order casting functions because the values needed
 to perform the cast are not available until runtime.

 This pass also substitutes all casts of the for (cast v Fn1 Fn2 l) with the
 equivalent form (fn-cast v Fn1 Fn2 l), in order to show that the cast specific
 to the closure value is being used.

 Furthermore all types not found in casts are removed.

Notes on the move to supporting coercions

(Coerce coercion E)  ==  E <coercion> in the literature

What are function coercions and how do we make them hybrid?

(Fun-Proxy? E)

(Let ([a (Coerce (Proxy-Fn (Inject Int) (Project Int "foo")) (Lambda (x) x))])
 (a 1))

== (Fn (Inject Int) (Project Int "foo")) ;; (Fn (Project Int "foo2") (Inject Int))
--> apply_coercion (Fn (Identity Dyn) (Indentity Dyn)) (lambda (x) x)

(Let ([tmp (lambda (x) x)]
      [a (lambda (x) (Coerce (Project Int "foo") (tmp (Inject Int))))]
  (a 1)))


But this isn't composable in a space efficent manner for instance
(Let ([tmp (lambda (x) x)]
      [olda   (lambda (x) (Coerce (Project Int "foo") (tmp (Inject Int))))]
      [a    (Coerce (Proxy-Fn (Project Int "foo2") (Inject Int)) olda)]
  (a 1)))

==> should step to
(Let ([tmp (lambda (x) x)]
      [olda ?????]
      [a   tmp])
  (a 1))

;; So in this example every call to dynamic gets the original closure
;; and every call to static gets a closure that is proxying with only
;; a single layer of indirection.

;; source
(letrec ((dynamic : ((Dyn -> Dyn) -> Unit)
          (lambda (f) (static f)))
         (static : ((Int -> Int) -> Unit)
          (lambda (f) (dynamic f))))
  (dynamic (lambda (x) x)))

==> Insert coercions

(letrec ((dynamic : ((Dyn -> Dyn) -> Unit)
          (lambda (f)
            (dynamic
             (coerce (Fn (Inject Int) (Project Int "l1")) f))))
         (static : ((Int -> Int) -> Unit)
          (lambda (f)
            (dynamic
             (coerce (Fn (Project Int "l2") (Inject Int)) f)))))
  (dynamic (lambda (x) x)))

==> w/hybrid proxies

(label ([coerce-fun1 ;; (Coercion Closure -> Closure)
         (code (crc fun)
          (if (not (hybrid-proxy? fun)) 
              (proxy-closure crcn fun)
              (let ([crcn^ (rt-compose-coercion crcn (hybrid-proxy-coercion fun))]
                    [fun^  (hybrid-proxy-closure fun)])
                    (if (identity-coercion? crcn)
                        fun^
                        ;; might need to handle failed coercion here also ;
                        (hybrid-proxy crcn fun^))) ))])
 (letrec ((dynamic : ((Dyn -> Dyn) -> Unit)
           (lambda (f)
             (static
              (coerce-fun1 (Fn (Inject Int) (Project Int "l1")) f))))
          (static : ((Int -> Int) -> Unit)
           (lambda (f)
             (dynamic
              (coerce-fun1 (Fn (Project Int "l2") (Inject Int)) f)))))
  (dynamic (lambda (x) x))))

;;;;;;; extra code needed for hybrid-proxy

(labels ([;; the code pointer for a hybrid-proxy-closure
          coerce-and-apply1 ;; ((Ptr -> Ptr) arg -> Ptr)
          (code (self arg0)
           (let ([crc (hybrid-proxy-coercion self)]
                 [fun (hybrid-proxy-closure  self)])
             (interp-coerce (Proxy-Fn-return crc)
                            (fun (interp-coerce (Proxy-Fn-argument 0 crc)
                                                arg0)))))]
         [interp-coerce    ;; (Coecion Ptr -> Ptr)
          (code (crc val)
           (cond
             [(Identity? crc) val]
             [(Inject? crc) #| make dynamic value |#]
             [(Project? crc)
              #|-- extract from dynamic
                -- make new coercion between extracted type and projected type|#]
             [(Proxy-Fn? crc) #|dispatch to correct arrity of coerce-fun |#]))]))

;;;;;;;; representation of hybrid proxy closures
(hybrid-proxy coercion fun) ==> (tag #b001 #(coerce-apply-coerce coercion fun))
(hybrid-proxy? closure) ==> (= #b001 (get-tag closure))
(hybrid-proxy-closure  proxy) ==> (array-ref (untag proxy) 2)
(hybrid-proxy-coercion proxy) ==> (array-ref (untag proxy) 1)
(closure-code closure)        ==> (array-ref closure 0)
(general-apply f a ...) ==> (closure-apply (untag c) a ...)
(closure-apply f a ...) ==> (let ([c f])
                              (label-apply (closure-code c) c a ...))

;; Invarients accessors must only be called on hybrid proxy
;; Hybrid-proxies and only have root level failed or function coercions

;; T! & Id_?

;; Space efficient coercions in lazy ud
s, t ::= Id_? | (G? ; i) | i
i    ::= (g ; G!) | g | Fail
g    ::= Id_i | (s -> t)


;; Values
u :: k | λx. e
v :: (u : c)

;; Expressions
e ::= (s e) | (e e) | v |  

;; (Space efficient ?) coercions in lazy-d
c    ::= T?  | g
g    ::= T!  | (s -> t)   ;; Coercions that will appear in the source code
s, t ::= T?ˡ | (T? ; i) | i | T! ;; Coecions that may result from composition
i    ::= (r ; T!) | r | ⊥ˡ 
r    ::= Id_t | (s -> t)

;; Evaluation
E[c u] -> (u : c)
E[c (u : g)] -> E[(u : g & c)]
E[((λx.e : (s -> t)) v)] -> t (λx.e (s v))


<v c> -> v
<u c> -> (u : c)
<u ⊥ˡ> -> (raise l)
<u : T!> -> (u : T!)
<u : (s -> t)> -> (u : (s -> t))
<(u : g) c> -> <u (c & g)>

compose source level coercion
(&) :: g -> c -> s 
T! & T?  -> Id_T
T! & G?ˡ -> <<T =>ˡ G>>
(s -> t) & (s' -> t') -> [s' $ s -> t $ t']

compose intermediate coercion
($) :: s -> t -> s (must be smaller or equal size)
T?l $ Id_? = T?ˡ
T?l $ T!   = Id_?
T?l $ (T? ; i) = Can't happen?
T?l $ (_  ; )  = what here
(T? ; i) $ 


;; Smart rebuild
[s -> t] -> s
[id_T -> id_G] -> Id_(T -> G)
[s -> t] -> (s -> t)
 

+-------------------------------------------------------------------------------+
|Input Grammar Cast-Or-Coercion-Language                                        |
|Output Grammar Cast2-Language                                                  |
+------------------------------------------------------------------------------|#
(require "../helpers.rkt"
         "../errors.rkt"
         "../unique-counter.rkt"
         "../configuration.rkt"
         "../language/cast-or-coerce0.rkt"
         "../language/cast-or-coerce1.rkt")

(provide lower-function-casts
         (all-from-out
          "../language/cast-or-coerce0.rkt"
          "../language/cast-or-coerce1.rkt"))

(define direct-fn-cast-optimization? (make-parameter #t))

;; The entry point for this pass it is called by impose-casting semantics
(: lower-function-casts (Cast-or-Coerce0-Lang -> Cast-or-Coerce1-Lang))
(define (lower-function-casts prgm)
  (match-define (Prog (list name next type) exp) prgm)

  (define ucount (make-unique-counter next))
  (define fn-casts : (HashTable Nat CoC1-Bnd-Code)
    (make-hasheq)) 
  
  (define new-expression
    (case (cast-representation)
      [(Type-Based)
       (define get-fn-cast!/type-based-cast
         (get-fn-cast! ucount fn-casts "fn_cast_" build-fn-cast/type-based-cast))
       (lfc-expr get-fn-cast!/type-based-cast build-apply/type-based-cast exp)]
      [(Coercions)
       (define get-fn-cast!/coercion
         (get-fn-cast! ucount fn-casts "fn_coerce_" build-fn-cast/coercion))
       (lfc-expr get-fn-cast!/coercion build-apply/coercion exp)]))

  (Prog (list name (unique-counter-next! ucount) type)
        (Labels (hash-values fn-casts) new-expression)))
  
  ;; (match 
  ;;   ['Type-Based
  ;;    (match-define-values (e (naive-fn-state n c*))
  ;;      (run-state (lfc-expr exp) (naive-fn-state next (hasheq))))
  ;;    (Prog (list name n type) (Labels (hash-values c*) e))]
  ;;   ['Coercions
  ;;    (match-define-values (e (hybrid-fn-state n a* c*))
  ;;      (run-state (lfc-expr exp) (hybrid-fn-state next '() (hasheq))))
  ;;    (Prog (list name n type) (Labels (append (hash-values c*) a*) e))]))





#;(: put-bnd (Integer CoC1-Bnd-Code -> (State LFC-State Null)))
#;(define ((put-apply i b) s)
  (match
    [(hybrid-fn-state n a c) (hybrid-fn
  (match-let ([(cons n h) s])
    (values '() (cons n (hash-set h i b)))))]))

#;(: get-bnd (Integer -> (State LFC-State (Option CoC1-Bnd-Code))))
#;(define ((get-bnd i) s)
  (match-let ([(cons n h) s])
    (values (hash-ref h i #f) s)))
(: lfc-expr :
   (Nat -> Uid)
   (CoC1-Expr CoC1-Expr* -> CoC1-Expr)
   CoC0-Expr
   -> CoC1-Expr)
(define (lfc-expr get-fn-cast! build-application exp)
  (: recur : CoC0-Expr -> CoC1-Expr)
  (define (recur exp)
    (match exp
      [(Cast (app recur exp) (Twosome t1 t2 blame-info))
       (cond
         [(and (Fn? t1) (Fn? t2) (direct-fn-cast-optimization?))
          (let ([caster (get-fn-cast! (Fn-arity t1))])
            (App-Code (Code-Label caster)
                      (list exp (Type t1) (Type t2) (Quote blame-info))))]
         [else (Cast exp (Twosome t1 t2 blame-info))])]
      [(Cast (app recur exp) (Coercion crcn))
       (cond
         [(and (Fn? crcn) (direct-fn-cast-optimization?))
          (let ([caster (get-fn-cast! (Fn-arity crcn))])
            (App-Code (Code-Label caster) (list exp (Quote-Coercion crcn))))]
         [else (Cast exp (Coercion crcn))])]
      [(Lambda f* (app recur exp))
       (let ([caster (get-fn-cast! (length f*))])
         (Lambda f* (Castable caster exp)))]
      [(App (app recur exp) (app recur* exp*))
       (build-application exp exp*)]
      ;; Just Recursion Cases Follow
      ;; Should be pretty boring
      [(Letrec (app recur-in-bnd* bnd*) (app recur exp))
        (Letrec bnd* exp)]
      [(Let (app recur-in-bnd* bnd*) (app recur exp))
       (Let bnd* exp)]
      [(Op p (app recur* exp*))
       (Op p exp*)]
      [(If (app recur t) (app recur c) (app recur a))
       (If t c a)]
      [(Begin (app recur* e*) (app recur e))
       (Begin e* e)]
      [(Repeat i (app recur e1) (app recur e2) a (app recur e3) (app recur e4))
       (Repeat i e1 e2 a e3 e4)]
      [(Gbox (app recur e))
       (Gbox e)]
      [(Gunbox (app recur e))
       (Gunbox e)]
      [(Gbox-set! (app recur e1) (app recur e2))
       (Gbox-set! e1 e2)]
      [(Gvector (app recur n) (app recur e))
       (Gvector n e)]
      [(Gvector-ref (app recur e) (app recur i))
       (Gvector-ref e i)]
      [(Gvector-set! (app recur e1) (app recur i) (app recur e2))
       (Gvector-set! e1 i e2)]
      [(Dyn-Fn-App (app recur e) (app recur* e*) t* l)
       (Dyn-Fn-App e e* t* l)]
      [(Dyn-GRef-Ref (app recur e) l)
       (Dyn-GRef-Ref e l)]
      [(Dyn-GRef-Set! (app recur e1) (app recur e2) t l)
       (Dyn-GRef-Set! e1 e2 t l)]
      [(Dyn-GVector-Ref (app recur e) (app recur i) l)
       (Dyn-GVector-Ref e i l)]
      [(Dyn-GVector-Set! (app recur e1) (app recur i) (app recur e2) t l)
       (Dyn-GVector-Set! e1 i e2 t l)]
      [(Create-tuple (app recur* e*))
       (Create-tuple e*)]
      [(Tuple-proj e i) (Tuple-proj (recur e) i)]
      [(Tuple-Coercion-Huh e) (Tuple-Coercion-Huh (recur e))]
      [(Tuple-Coercion-Num e) (Tuple-Coercion-Num (recur e))]
      [(Tuple-Coercion-Item e i) (Tuple-Coercion-Item (recur e) i)]
      [(Coerce-Tuple uid e1 e2) (Coerce-Tuple uid (recur e1) (recur e2))]
      [(Cast-Tuple uid e1 e2 e3 e4) (Cast-Tuple uid (recur e1) (recur e2) (recur e3) (recur e4))]
      [(Type-Tuple-Huh e) (Type-Tuple-Huh (recur e))]
      [(Type-Tuple-num e) (Type-Tuple-num (recur e))]
      [(Make-Tuple-Coercion uid t1 t2 lbl) (Make-Tuple-Coercion uid (recur t1) (recur t2) (recur lbl))]
      [(Compose-Tuple-Coercion uid e1 e2) (Compose-Tuple-Coercion uid (recur e1) (recur e2))]
      [(Mediating-Coercion-Huh? e) (Mediating-Coercion-Huh? (recur e))]
      [(Var id)    (Var id)]
      [(Quote lit) (Quote lit)]
      [other (error 'lower-function-casts/expr "unmatched ~a" other)]))

  (: recur* : CoC0-Expr* -> CoC1-Expr*)
  (define (recur* e*) (map recur e*))

  (: recur-in-bnd* : CoC0-Bnd* -> CoC1-Bnd*)
  (define (recur-in-bnd* b*)
    (define (recur-in-bnd [b : CoC0-Bnd]) : CoC1-Bnd
      (cons (car b) (recur (cdr b))))
    (map recur-in-bnd b*)
    )

  (recur exp))

;; (: lfc-expr* (-> CoC0-Expr* (State LFC-State CoC1-Expr*)))
;; (define (lfc-expr* e*) (map-state lfc-expr e*))

;; ;; Recur through binding with the casting state
;; (: lfc-bnd* (-> CoC0-Bnd* (State LFC-State CoC1-Bnd*)))
;; (define (lfc-bnd* b*)
;;   (: lfc-bnd (-> CoC0-Bnd (State LFC-State CoC1-Bnd)))
;;   (define (lfc-bnd b)
;;     (match-let ([(cons i e) b])
;;       (do (bind-state : (State LFC-State CoC1-Bnd))
;;           (e : CoC1-Expr <- (lfc-expr e))
;;         (return-state (cons i e)))))
;;   (map-state lfc-bnd b*))


(: get-fn-cast! :
   Unique-Counter
   (HashTable Nat CoC1-Bnd-Code)
   String
   ((String -> Uid) -> (Nat Uid -> CoC1-Code))
   ->
   (Nat -> Uid))
(define (get-fn-cast! counter fn-casts name-base build-caster-code)
  (: next-uid! : String -> Uid)
  (define (next-uid! str)
    (Uid str (unique-counter-next! counter)))

  (: build-caster! : Nat Uid -> CoC1-Code)
  (define build-caster! (build-caster-code next-uid!))

  (lambda ([arity : Nat])
    : Uid
    (let ([bnd? (hash-ref fn-casts arity #f)])
      (cond
        [bnd? (car bnd?)]
        [else
         (define name (string-append name-base (number->string arity)))
         (define caster-uid  (next-uid! name))
         (define caster-code (build-caster! arity caster-uid))
         (define caster-bnd  (cons caster-uid caster-code))
         (hash-set! fn-casts arity caster-bnd)
         caster-uid]))))

(: build-fn-cast/type-based-cast : (String -> Uid) -> (Nat Uid -> CoC1-Code))
(define ((build-fn-cast/type-based-cast next-uid!) ary name)
  (match-define (and caster-fmls (list fn t1 t2 lbl))
    (map next-uid! '("f" "t1" "t2" "lbl")))
  (match-define (list fn-var t1-var t2-var lbl-var)
    (map #{Var @ Uid} #{caster-fmls :: Uid*}))
  (define uid* (map next-uid! (make-list ary "v")))
  (define args
    (for/list : (Listof CoC1-Expr)
              ([u uid*]
               [i (in-naturals)])
      (let* ([i  : CoC1-Expr (Quote i)]
             [t1 : CoC1-Expr (Type-Fn-arg t1-var i)]
             [t2 : CoC1-Expr (Type-Fn-arg t2-var i)])
        (Interpreted-Cast (Var u) (Twosome t2 t1 lbl-var)))))
  (define t1-ret (Type-Fn-return t1-var))
  (define t2-ret (Type-Fn-return t2-var))
  (define call (App-Fn fn-var args))
  (define cast-call (Interpreted-Cast call (Twosome t1-ret t2-ret lbl-var)))
  (define then-cast (Lambda uid* (Castable name cast-call)))
  (Code caster-fmls then-cast))

(: build-apply/type-based-cast : CoC1-Expr CoC1-Expr* -> CoC1-Expr)
(define (build-apply/type-based-cast exp exp*)
  (App-Fn exp exp*))

(: build-fn-cast/coercion : (String -> Uid) -> (Nat Uid -> CoC1-Code))
(define ((build-fn-cast/coercion next-uid!) arity name)
  (: or-help ((Listof CoC1-Expr) -> CoC1-Expr))
  (define (or-help a)
    (cond
      [(null? a) (Quote #t)]
      [(null? (cdr a)) (car a)]
      [else (If (car a) (or-help (cdr a)) (Quote #f))]))
  (: id-c*? (Uid* -> CoC1-Expr*))
  (define (id-c*? x*)
    (map (lambda ([x : Uid]) (Id-Coercion-Huh (Var x))) x*))
  (: new-fn-crcn (Uid* Uid -> CoC1-Expr))
  (define (new-fn-crcn args ret)
    (Fn-Coercion (map (inst Var Uid) args) (Var ret)))
  (: compose-return (Uid Uid Uid -> CoC1-Bnd*))
  (define (compose-return ret new old)
    (cons (cons ret (Compose-Coercions (Fn-Coercion-Return (Var old))
                                       (Fn-Coercion-Return (Var new))))
          '()))
  (: compose-arg (Uid Uid Uid Index -> CoC1-Bnd))
  (define (compose-arg arg new old i)
    (cons arg (Compose-Coercions (Fn-Coercion-Arg (Var new) (Quote i))
                                 (Fn-Coercion-Arg (Var old) (Quote i)))))

  (define u-clos (next-uid! "unknown_closure"))
  (define new-crcn (next-uid! "new_fn_coercion"))
  (define arg* (map next-uid! (make-list arity "arg_coercion")))
  (define ret  (next-uid! "ret_coercion"))
  (define old-crcn (next-uid! "old_fn_coercion"))
  (define r-clos (next-uid! "raw_closure"))

  (define destructure-old-closure : CoC1-Bnd*
    `((,old-crcn  . ,(Fn-Proxy-Coercion (Var u-clos)))
      (,r-clos    . ,(Fn-Proxy-Closure  (Var u-clos)))))
  
  (define composed-coercions-bindings : CoC1-Bnd*
    (for/fold ([b* : CoC1-Bnd* (compose-return ret new-crcn old-crcn)])
              ([a : Uid (reverse arg*)]
               [i : Integer (in-range (- arity 1) -1 -1)])
      (unless (index? i)
        (error 'lower-function-casts "bad index made"))
      (cons (compose-arg a new-crcn old-crcn i) b*)))

  (define all-result-in-id-huh : CoC1-Expr
    (or-help (id-c*? (cons ret arg*))))

  (unless (index? arity)
    (error 'lower-function-cast/build-fn-cast-with-coercion
           "arity grew too large to be a valid index"))
  
  (Code (list u-clos new-crcn)
        ;; Is the closure we are casting a hybrid proxy
        (If (Fn-Proxy-Huh (Var u-clos))
            ;; If so we have to compose the old and new fn-coercions
            ;; First get the old fn-coercion
            (Let destructure-old-closure
                 ;; Then compose each sub coercion
                 ;; this loop reverses the list arguments hence the
                 ;; reverse that is used in the argument list
                 (Let composed-coercions-bindings
                      ;; Check if all Composes resulted in Id Coercions
                      (If all-result-in-id-huh
                          ;; If so the original closure is the correct type
                          (Var r-clos)
                          ;; Otherwise a new proxy is needed
                          (Fn-Proxy #{arity :: Index}
                                    (Var r-clos)
                                    (new-fn-crcn arg* ret)))))
            ;; Closure is a regular on --> just make a new proxy
            (Fn-Proxy #{arity :: Index} (Var u-clos) (Var new-crcn)))))
  
(: build-apply/coercion : CoC1-Expr CoC1-Expr* -> CoC1-Expr)
(define (build-apply/coercion exp exp*)
  (App/Fn-Proxy-Huh exp exp*))


  
  
