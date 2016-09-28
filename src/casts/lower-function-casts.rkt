#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: src/casts/lower-function-casts                                           |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
 Description:
 This pass adds the castable form which extends all lambdas that
 are castable with a free variable which represents the function that is
 necessary in order to cast the closure created by this lambda. It can now be
 extracted from a closure value using the fn-cast form. Note that lambdas
 created in the process of creating these casting functions are not castable.
 Though their return values are.

 Casts of the form (runtime-cast value-exp type1-exp type2-exp label-exp) are
 used in each of the higher order casting functions because the values needed
 to perform the cast are not available until runtime.

 This pass also substitutes all casts of the form (cast v Fn1 Fn2 l) with the
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
         "../configuration.rkt"
         "../language/cast-or-coerce0.rkt"
         "../language/cast-or-coerce1.rkt")

(provide lower-function-casts
         (all-from-out
          "../language/cast-or-coerce0.rkt"
          "../language/cast-or-coerce1.rkt"))


;; The LFC-State in this code is the either the state for the hybrid function pass
;; or the state for the nieve function cast representation either way any code
;; that need to touch the code must be able to handle both states
;; usually by erroring if they get the wrong state though the state can be used
;; as a switch for behaving differently the simplest example of this is the code
;; in lfc-expr cast case that errors when the state indicates that there shouldn't
;; be any casts.
(define-type LFC-State  (U hybrid-fn-state naive-fn-state))
(define-type Arrity-Map (HashTable Integer CoC1-Bnd-Code)) 

(struct hybrid-fn-state ([next : Nat]
                         [apply : (Listof CoC1-Bnd-Code)]
                         [coerce : Arrity-Map]))
(struct naive-fn-state  ([next : Nat][cast  : Arrity-Map]))  



;; The entry point for this pass it is called by impose-casting semantics
(: lower-function-casts (Cast-or-Coerce0-Lang Config -> Cast-or-Coerce1-Lang))
(define (lower-function-casts prgm config)
  (match-define (Prog (list name next type) exp) prgm)
  (match (Config-cast-rep config)
    ['Twosomes
     (match-define-values (e (naive-fn-state n c*))
       (run-state (lfc-expr exp) (naive-fn-state next (hasheq))))
     (Prog (list name n type) (Labels (hash-values c*) e))]
    ['Coercions
     (match-define-values (e (hybrid-fn-state n a* c*))
       (run-state (lfc-expr exp) (hybrid-fn-state next '() (hasheq))))
     (Prog (list name n type) (Labels (append (hash-values c*) a*) e))]))





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

(: lfc-expr (CoC0-Expr -> (State LFC-State CoC1-Expr)))
(define (lfc-expr exp)
  (do (bind-state : (State LFC-State CoC1-Expr))
      (match exp
        ;; Here we must be representing casts as twosomes because the ;
        ;; cast nodes are still in the tree
        [(Cast exp rep)
         (exp : CoC1-Expr  <- (lfc-expr exp))
         (match rep
           [(Twosome t1 t2 b-lbl)
            (s : LFC-State <- get-state)
            (begin ;; Escape to the world of effects for a hot second
              (unless (naive-fn-state? s)
                (error 'lfc-expr "Cast line with hybrid-fn-state")))
            (uid : Uid <- (uid-lfc "tmp_for_function_cast"))
            (if (and (Fn? t1) (Fn? t2))
                (doing;; continue with do notation
                 (c-lbl : Uid <- (get-fn-cast/coerce (Fn-arity t1)))
                 (return-state
                  (App-Code (Code-Label c-lbl)
                            (list exp (Type t1) (Type t2) (Quote b-lbl)))))
                (return-state (Cast exp (Twosome t1 t2 b-lbl))))]
           #|TODO Get rid of this code once the above is known
           to work
           (let ([tmp (Var uid)])
           (return-state
           (Let (list (cons uid exp))
           (App-Code (Fn-Caster tmp)
           (list tmp (Type t1) (Type t2) (Quote lbl))))))
           |#                
           ;; Here we must be representing casts as Coercions because the coercion node is present
           [(Coercion crcn)
            (if (Fn? crcn)
                (doing
                 (lbl : Uid <- (get-fn-cast/coerce (Fn-arity crcn)))
                 (return-state
                  (App-Code (Code-Label lbl)
                            (list exp (Quote-Coercion crcn)))))
                (return-state (Cast exp (Coercion crcn))))])]
        ;; Here we can be doing either casts or coercions
        ;; we could check the state and vary behavior but for now I think
        ;; this still works.
        [(Lambda f* exp)
         (uid : Uid       <- (get-fn-cast/coerce (length f*)))
         (exp : CoC1-Expr <- (lfc-expr exp))
         (return-state (Lambda f* (Castable uid exp)))]
        [(App exp exp*)
         (exp  : CoC1-Expr  <- (lfc-expr  exp))
         (exp* : CoC1-Expr* <- (lfc-expr* exp*))
         (s : LFC-State <- get-state)
         (match s
           [(hybrid-fn-state _ _ _) (return-state (App/Fn-Proxy-Huh exp exp*))]
           [(naive-fn-state _ _)    (return-state (App-Fn exp exp*))]
           [else (error 'lfc-expr "this should never happen")])]
        ;; Just Recursion Cases Follow
        ;; Should be pretty boring
        [(Letrec bnd* exp)
         (bnd* : CoC1-Bnd* <- (lfc-bnd* bnd*))
         (exp : CoC1-Expr  <- (lfc-expr exp))
         (return-state (Letrec bnd* exp))]
        [(Let bnd* exp)
         (bnd* : CoC1-Bnd* <- (lfc-bnd* bnd*))
         (exp : CoC1-Expr  <- (lfc-expr exp))
         (return-state (Let bnd* exp))]
        [(Op p exp*)
         (exp* : CoC1-Expr* <- (lfc-expr* exp*))
         (return-state (Op p exp*))]
        [(If tst csq alt)
         (tst : CoC1-Expr <- (lfc-expr tst))
         (csq : CoC1-Expr <- (lfc-expr csq))
         (alt : CoC1-Expr <- (lfc-expr alt))
         (return-state (If tst csq alt))]
        [(Begin e* e)
         (e* : CoC1-Expr* <- (lfc-expr* e*))
         (e  : CoC1-Expr  <- (lfc-expr  e))
         (return-state (Begin e* e))]
        [(Repeat i e1 e2 e3)
         (e1 : CoC1-Expr <- (lfc-expr e1))
         (e2 : CoC1-Expr <- (lfc-expr e2))
         (e3 : CoC1-Expr <- (lfc-expr e3))
         (return-state (Repeat i e1 e2 e3))]
        [(Gbox e)
         (e  : CoC1-Expr  <- (lfc-expr e))
         (return-state (Gbox e))]
        [(Gunbox e)
         (e  : CoC1-Expr  <- (lfc-expr e))
         (return-state (Gunbox e))]
        [(Gbox-set! e1 e2)
         (e1 : CoC1-Expr <- (lfc-expr e1))
         (e2 : CoC1-Expr <- (lfc-expr e2))
         (return-state (Gbox-set! e1 e2))]
        [(Gvector n e)
         (n : CoC1-Expr <- (lfc-expr n))
         (e : CoC1-Expr <- (lfc-expr e))
         (return-state (Gvector n e))]
        [(Gvector-ref e index)
         (e : CoC1-Expr <- (lfc-expr e))
         (i : CoC1-Expr <- (lfc-expr index))
         (return-state (Gvector-ref e i))]
        [(Gvector-set! e1 index e2)
         (e1 : CoC1-Expr <- (lfc-expr e1))
         (i : CoC1-Expr <- (lfc-expr index))
         (e2 : CoC1-Expr <- (lfc-expr e2))
         (return-state (Gvector-set! e1 i e2))]
        [(Mbox e t)
         (e  : CoC1-Expr  <- (lfc-expr e))
         (return-state (Mbox e t))]
        [(Munbox e)
         (e  : CoC1-Expr  <- (lfc-expr e))
         (return-state (Munbox e))]
        [(Mbox-set! e1 e2)
         (e1 : CoC1-Expr <- (lfc-expr e1))
         (e2 : CoC1-Expr <- (lfc-expr e2))
         (return-state (Mbox-set! e1 e2))]
        [(MBoxCastedRef u t)
         (return-state (MBoxCastedRef u t))]
        [(MBoxCastedSet! u e t)
         (e : CoC1-Expr <- (lfc-expr e))
         (return-state (MBoxCastedSet! u e t))]
        [(Mvector e1 e2 t)
         (e1  : CoC1-Expr  <- (lfc-expr e1))
         (e2  : CoC1-Expr  <- (lfc-expr e2))
         (return-state (Mvector e1 e2 t))]
        [(Mvector-ref e1 e2)
         (e1  : CoC1-Expr  <- (lfc-expr e1))
         (e2  : CoC1-Expr  <- (lfc-expr e2))
         (return-state (Mvector-ref e1 e2))]
        [(Mvector-set! e1 e2 e3)
         (e1 : CoC1-Expr <- (lfc-expr e1))
         (e2 : CoC1-Expr <- (lfc-expr e2))
         (e3 : CoC1-Expr <- (lfc-expr e3))
         (return-state (Mvector-set! e1 e2 e3))]
        [(MVectCastedRef u i t)
         (i : CoC1-Expr <- (lfc-expr i))
         (return-state (MVectCastedRef u i t))]
        [(MVectCastedSet! u i e t)
         (e : CoC1-Expr <- (lfc-expr e))
         (i : CoC1-Expr <- (lfc-expr i))
         (return-state (MVectCastedSet! u i e t))]
        [(Var id)    (return-state (Var id))]
        [(Quote lit) (return-state (Quote lit))])))

(: lfc-expr* (CoC0-Expr* -> (State LFC-State CoC1-Expr*)))
(define (lfc-expr* e*) (map-state lfc-expr e*))

;; Recur through binding with the casting state
(: lfc-bnd* (CoC0-Bnd* -> (State LFC-State CoC1-Bnd*)))
(define (lfc-bnd* b*)
  (: lfc-bnd (CoC0-Bnd -> (State LFC-State CoC1-Bnd)))
  (define (lfc-bnd b)
    (match-let ([(cons i e) b])
      (do (bind-state : (State LFC-State CoC1-Bnd))
          (e : CoC1-Expr <- (lfc-expr e))
        (return-state (cons i e)))))
  (map-state lfc-bnd b*))


(: get-fn-cast/coerce (Index -> (State LFC-State Uid)))
(define ((get-fn-cast/coerce arity) s)
  (if (hybrid-fn-state? s)
      (let ([bnd? (hash-ref (hybrid-fn-state-coerce s) arity #f)])
        (cond 
          [bnd? (values (car bnd?) s)]
          [else ((mk-coercer arity ) s)]))
      (let ([bnd? (hash-ref (naive-fn-state-cast s) arity #f)])
        (cond 
          [bnd? (values (car bnd?) s)]
          [else ((mk-caster arity) s)]))))

(: mk-caster (Index -> (State naive-fn-state Uid)))
(define (mk-caster arity)
  (do (bind-state : (State naive-fn-state Uid))
      ((naive-fn-state n c*) : naive-fn-state <- get-state)
      (let*-values ([(uid n) (run-state (uid-state (format "cast_fn_~a" arity)) n)]
                    [(code n) (run-state (build-caster-code arity uid) n)]
                    [(bnd) (cons uid code)]
                    [(c*)  (hash-set c* arity bnd)])
        (_ : Null <- (put-state (naive-fn-state n c*)))
        (return-state uid))))


(: build-caster-code (Index Uid -> (State Nat CoC1-Code)))
(define (build-caster-code ary name)
  (do (bind-state : (State Nat CoC1-Code))
      ;; create the uids for the casting function
      (fn : Uid <- (uid-state "f"))
      (t1 : Uid <- (uid-state "t1"))
      (t2 : Uid <- (uid-state "t2"))
      (lbl : Uid <- (uid-state "lbl"))
      (uid* : Uid* <- (map-state uid-state (make-list ary "v")))
      ;; create the vars for these uids
      (let* ([fn-var  : CoC1-Expr (Var fn)]
             [t1-var  : CoC1-Expr (Var t1)]
             [t2-var  : CoC1-Expr (Var t2)]
             [lbl-var : CoC1-Expr (Var lbl)]
             ;; arguments to the unproxied function call
             [args
              (for/list : (Listof CoC1-Expr)
                        ([u uid*]
                         [i (in-naturals)])
                (let* ([i  : CoC1-Expr (Quote i)]
                       [t1 : CoC1-Expr (Type-Fn-arg t1-var i)]
                       [t2 : CoC1-Expr (Type-Fn-arg t2-var i)])
                  (Interpreted-Cast (Var u) (Twosome t2 t1 lbl-var))))]
              ;; TODO Erase this line once the above has been tested
             #;(mk-casted-args t1-var t2-var lbl-var uid*)
             ;; formals for the casting function
             [caster-fml : Uid* (list fn t1 t2 lbl)]
             ;; cast the return of the unproxied application
             [t1-ret    : CoC1-Expr (Type-Fn-return t1-var)]
             [t2-ret    : CoC1-Expr (Type-Fn-return t2-var)]
             [call      : CoC1-Expr (App-Fn fn-var args)]
             [cast-call : CoC1-Expr
                        (Interpreted-Cast call (Twosome t1-ret t2-ret lbl-var))]
             [then-cast : CoC1-Expr (Lambda uid* (Castable name cast-call))])
        (return-state (Code caster-fml then-cast)))))

;; TODO make sure the code works then delete this chunk of unused code
;; from the function above
;; Wrong there is an invarient that you cannot make casts between
;; function types of the wrong arity but the error is raise
;; as soon as the function type is unboxed
;; Furthermore as long as this invarient is maintained the code
;; we know that all function casts in the code do not need to perform
;; arrity checks
;; application isn't performed if the arity is a mismatch
;;[arity-t1 : CoC1-Expr (Type-Fn-arity t1-var)]
;;[arity-t2 : CoC1-Expr (Type-Fn-arity t2-var)]
;;[arity-test : CoC1-Expr (Op '= (list arity-t1 arity-t2))]
;; the closure to return if arity is correct
;; I had the semantics incorrect in the following line
;; [else-blame : CoC1-Expr (Lambda '() (Castable name (Blame lbl-var)))]
;;[else-blame : CoC1-Expr (Blame lbl-var)]
;;[check : CoC1-Expr (If arity-test then-cast else-blame)])
;; The entire casting function is now all constructed

(: mk-casted-args (CoC1-Expr CoC1-Expr CoC1-Expr Uid* -> CoC1-Expr*))
(define (mk-casted-args t1 t2 lbl uid*)
  (error 'foo)
  ;(: loop (-> Uid* Index CoC1-Expr*))
  #;(define (loop uid* index)
    (let ([i (Quote index)])
      (if (null? uid*)
          '()
          (let ([val : CoC1-Expr (Var (car uid*))]
                [t1 : CoC1-Expr (Type-Fn-arg t1 i)]
                [t2 : CoC1-Expr (Type-Fn-arg t2 i)])
            (cons (Interpreted-Cast val t2 t1 lbl)        ;; contravarient argument cast
                  (loop (cdr uid*) (cast (add1 index) Index)))))))
  #;(loop uid* 0))


(define-type BC.BC (Pairof CoC1-Bnd-Code CoC1-Bnd-Code))

(: mk-coercer (Index -> (State hybrid-fn-state Uid)))
;; TODO this is really ugly because I am about 90% sure that it is
;; best to implement this code at the specify representation level
;; once the behavior of closures have been exposed it may also
;; simplify the impelementation of data structure representation
;; of fn-proxies
;; TODO if this works move this code to specify representation
;; otherwise revert this code
(define (mk-coercer arity)
  (do (bind-state : (State hybrid-fn-state Uid))
      ((hybrid-fn-state n a* c*) : hybrid-fn-state <- get-state)
      (let*-values ([(c n^)
                    (run-state
                     (do #;(bind-state : (State Nat BC.BC))
                         (bind-state : (State Nat CoC1-Bnd-Code))
                         (let ([str (number->string arity)])           
                           #;(a-u : Uid <-
                                (uid-state (string-append "app_coerced" str)))
                           (c-u : Uid <-
                                (uid-state (string-append "coerce_fn_" str)))
                           #;(a-c : CoC1-Code <- (mk-apply-code arity))
                           (c-c : CoC1-Code <- (mk-coerce-code arity #;a-u))
                           (return-state (cons c-u c-c)
                            #;(cons (cons a-u a-c)
                                  (cons c-u c-c)))))
                     n)]
                    #;[(a c) (values (car a.c) (cdr a.c))])
        (_ : Null <- (put-state
                      (hybrid-fn-state n a* #;(cons a a*) (hash-set c* arity c))))
        (return-state (car c)))))


;;(: mk-apply-code (Index -> (State Nat CoC1-Code)))
#;(define (mk-apply-code arity)
  ;;TODO unfortunately the part exposes the calling conventions for closures
  ;;Think about if there is a better way to do this.
  (do (bind-state : (State Nat CoC1-Code))
      (h-clos : Uid  <- (uid-state "hybrid_closure"))
      (r-clos : Uid  <- (uid-state "raw_closure"))
      (crcn : Uid  <- (uid-state "fn_coercion"))
      (uid* : Uid* <- (map-state uid-state (make-list arity "v")))
      ;; create the vars for these uids
      (let* ([h-clos-var  : CoC1-Expr (Var h-clos)]
             [r-clos-var  : CoC1-Expr (Var r-clos)]
             [crcn-var  : CoC1-Expr (Var crcn)]
             [code : CoC1-Code
              (Code (cons h-clos uid*)
               (Let (list (cons crcn (Fn-Proxy-Coercion h-clos-var)))
                 (Interpreted-Coerce
                   (Fn-Coercion-Return crcn-var)
                   (App-Fn
                     (Fn-Proxy-Closure h-clos-var)
                     (for/list : (Listof CoC1-Expr)
                               ([u uid*] [i (in-naturals)])
                       (Interpreted-Coerce
                        (Fn-Coercion-Arg crcn-var (Quote i))
                        (Var u)))))))])
        (return-state code))))


;;(: mk-coerce-code (Index Uid -> (State Nat CoC1-Code)))
;;(define (mk-coerce-code arity apply-uid)
(: mk-coerce-code (Index -> (State Nat CoC1-Code)))
(define (mk-coerce-code arity)
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
  ;; TODO this is likely the wrong definition once this file typechecks
  ;; make sure the composition is in the correct order
  (: compose-arg (Uid Uid Uid Index -> CoC1-Bnd))
  (define (compose-arg arg new old i)
    (cons arg (Compose-Coercions (Fn-Coercion-Arg (Var new) (Quote i))
                                 (Fn-Coercion-Arg (Var old) (Quote i)))))
    (do (bind-state : (State Nat CoC1-Code))
      (u-clos   : Uid <-  (uid-state "unknown_closure"))
      (new-crcn : Uid <-  (uid-state "new_fn_coercion"))
      (arg*     : Uid* <-
         (map-state uid-state (make-list arity "arg_coercion")))
      (ret      : Uid <- (uid-state "ret_coercion"))
      (old-crcn : Uid <-    (uid-state "old_fn_coercion"))
      (r-clos   : Uid <- (uid-state "raw_closure"))
      (return-state
       (Code (list u-clos new-crcn)
        ;; Is the closure we are casting a hybrid proxy
        (If (Fn-Proxy-Huh (Var u-clos))
            ;; If so we have to compose the old and new fn-coercions
            ;; First get the old fn-coercion
            (Let `((,old-crcn  . ,(Fn-Proxy-Coercion (Var u-clos)))
                   (,r-clos    . ,(Fn-Proxy-Closure  (Var u-clos))))
                 ;; Then compose each sub coercion
                 ;; this loop reverses the list arguments hence the
                 ;; reverse that is used in the argument list
             (Let (for/fold ([b* : CoC1-Bnd*
                                 (compose-return ret new-crcn old-crcn)])
                            ([a : Uid (reverse arg*)]
                             [i : Integer (in-range (- arity 1) -1 -1)])
                    (unless (index? i)
                      (error 'lower-function-casts "bad index made"))
                    (cons (compose-arg a new-crcn old-crcn i) b*))
                  ;; Check if all Composes resulted in Id Coercions
                  (If (or-help (id-c*? (cons ret arg*)))
                      ;; If so the original closure is the correct type
                      (Var r-clos)
                      ;; Otherwise a new proxy is needed
                      (Fn-Proxy arity
                                (Var r-clos)
                                (new-fn-crcn arg* ret)))))
            ;; Closure is a regular on --> just make a new proxy
            (Fn-Proxy arity (Var u-clos) (Var new-crcn)))))))



#;
(define (build-fun-caster arity)
  )

#;
(define (build-fun-coerce arity)
  (do (bind-state : (State Natural CoC1-Bnd-Code))
      (uid : Uid  <- (uid-state (format "apply_coerced_fn_~a" ary)))
    (code : CoC1-Code <- (build-fun- arity uid))
    (return-state (cons uid code))))

;; make a function that builds a function proxy for the cast of a specific
;; arity. This function is always of the following form
#|
   
  (code (v t1 t2 l)
    (lambda (a ...)
      (cast (v (cast a (fn-type-arg t2) (fn-type-arg t1) l) ...)
            (fn-type-ret t1) (fn-type-ret t2) l)))
  |#
  

;; coerce-raw-closure-n
;; Arrity Specific because c doesn't have a way of applying to an unkown number of values
#;
(Code (coercion fun)
      (Lambda-Proxy (list a1 ...) (list coercion fun)
                    (Interp-Coerce (Fn-Coercion-return coercion)
                                   (App-Uncoerced-closure fun
                                                          (list (Interp-Coerce (Fn-Coercion-argument coercion 1) a1) ...)))))


(define gcf-key -1)

#;(: get-general-coerce-fn (State LFC-State Uid))
#;(define get-general-coerce-fn
  (do (bind-state : (State LFC-State Uid))
      (crcn-uid : Uid <- (uid-lfc "new_coercion"))
      (clos-uid : Uid <- (uid-lfc "closure"))
      (let ([nfc (Code (list crcn-uid clos-uid) (error 'todo "normalization code"))])
        (gcf-uid : Uid <- (uid-lfc "coerce-fn"))
        ;;(_ : Null <- (put-bnd gcf-key (cons gcf-uid nfc)))
        (return-state gcf-uid))))

(: get-coerce-fn-n (Index -> (State LFC-State Uid)))
(define (get-coerce-fn-n n)
  (do (bind-state : (State LFC-State Uid))
      (crcn-uid : Uid <- (uid-lfc "new_coercion"))
      (clos-uid : Uid <- (uid-lfc "closure"))
      (error 'todo "create code for specific arity")))

#;(: get-apply-coerced-fn-n (Integer -> (State LFC-State Uid)))
#;(define (get-apply-coerced-fn-n n)
  (do (bind-state : (State LFC-State Uid))
      (s : LFC-State <- get-state)
      (if (not (hybrid-fn-state? s))
          (error 'lower-function-casts
                 "get-apply-coerced-fn-n called with wrong state")
          (let ([apply? (hash-ref (hybrid-fn-state-apply s) n )])
            (if apply?
                (return-state (car apply?))
                (error 'todo "(mk-apply-fn-coercion n)"))))))

                 
#|(Code (list coercion fun)
 (Let ([proxied? (Proxied-fn? fun)])
  (Switch (Fn-Coersion-arity coercion)
   (build-switch-cases coercion fun proxied?)
   (If proxied?
       (Let ([raw-fun (Proxied-Fn-closure)])) (proxied?))       (build-list (Arity-Cases)
                        (lambda ([i : Natural]) 
                          (cons i (apply-fun-coercion-aux i crcn crcn2 raw-fun)))
                        (If proxied?
                            (Let ([raw]))))
       [0  (mk-fn-coerce-aux crcn fun) (let ([ret-coercion (Compose-Coercions (Fn-Coercion-return coercions) ()) ]) (Fn-Coercion-return ))])
(Let ([raw-fun (if proxied? (Proxied-fn-closure  fun) fun)]
[crcn2   (if proxied? (Proxied-fn-coercion fun) crcn)])
))

      (Let ([arity (Function-Coercion-arity c1)])
      (Let ([new_crcn (New-Function-Coercion arity)]
[id?      (Quote #t)])  
(Begin
(Repeat i 1 arity
(Let ([tmp (Compose-Coercions (Function-Coercion-argument c1 i)
(Function-Coercion-argument c2 i))])
(Begin
(Function-Coercion-argument! c3 i tmp)
(If id?
(If (Identity-Coercion? tmp)
(Assign tmp #t)
(Assign tmp #f))
()))))
(Let ([tmp (Compose-Coercions (Function-Coercion-return c1)
(Function-Coercion-return c2))])
(Begin
(Function-Coercion-return! c3 tmp)
(If id?
(If (Identity-Coercion? tmp)
(Identity-Coercion)
tmp)
tmp))))) ))
|#
#|
;; Code Template for coerce-hybrid-closure
;; I am unsure of the best way to (Coerce-Raw-Closure n)
                                        #;
(Code (coercion code)
(If (Proxied-Function? fun)
(Let ([coercion (Compose-Coercions-Function-Coercion (Proxied-Function-coercion fun) coercion)]
[fun      (Proxied-Function-function fun)])
;; No need to check for a Failed coercion here the only thing that ;
;; should create a fail here is an arity mismatch wich I am convinced is
;; gaurenteed statically at this point
(If (Identity-Coercion? coercion)
fun
(App-Label (Coerce-Raw-Closure n) '(coercion fun))))
(App-Label (Coerce-Raw-Closure n) '(coercion fun))))

;; Function-coercion-compose -- Probably introduce during interpret-casts
;; Could be alot easier if this were also arrity specific
                                        #;

)
;; With arity specialization
;; And here is expanded to Ifs in the actual code generated
                                        #;
(Code (c1 c2)
(Let ([arg-n (Compose-Coercions (Funciton-Coercion-argument c1 n)
(Function-Coercion-argument c2 n))] ...
[ret   (Compose-Coercions (Function-Coercion-return c1)
(Function-Coercion-return c2))])
(If (AND (Identity? arg-n) ...)
(Identity-Coercion)
(Function-Coercions (List arg-n ...) ret))))
|#

(: mk-caster-ids (Index Uid* -> (State LFC-State Uid*)))
(define (mk-caster-ids ary vars)
  (if (zero? ary)
      (return-state vars)
      (do (bind-state : (State LFC-State Uid*))
          (uid : Uid <- (uid-lfc "v"))
          (mk-caster-ids (- ary 1) (cons uid vars)))))

;; Since they are just helper functions the
(: uid-lfc (String -> (State LFC-State Uid)))
(define ((uid-lfc name) s)
  (match s
    [(hybrid-fn-state n a c)
     (values (Uid name n) (hybrid-fn-state (add1 n) a c))]
    [(naive-fn-state n c)
     (values (Uid name n) (naive-fn-state (add1 n) c))]))
