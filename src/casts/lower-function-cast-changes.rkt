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
         "../configuration.rkt"
         "../language/cast-or-coerce0.rkt"
         "../language/cast-or-coerce1.rkt")

(provide lower-function-casts
         (all-from-out
          "../language/cast-or-coerce0.rkt"
          #;"../language/casts-or-coercions.rkt"
          "../language/cast-or-coerce1.rkt"))

;; The entry point for this pass it is called by impose-casting semantics
(: lower-function-casts (Cast-or-Coerce0-Lang Config . -> . Cast-or-Coerce1-Lang))
(define (lower-function-casts prgm config)
  (match-define (Prog (list name next type) exp) prgm)
  (match (Config-cast-representation config)
    ['Twosomes
     (define-values (exp^ state^)
       (run-state (lfc-expr exp) (cons next no-casters)))
     (Prog (list name (car state^) type)
           (Letrec (hash-values (cdr state^)) exp^))
     ]
    ['Coercions
     (define-values)])
  (match-let ([(Prog (list name next type) exp) prgm])
))

(: lfc-expr (-> C0-Expr (State LFC-State CoC1-Expr)))
(define (lfc-expr exp)
  (do (bind-state : (State LFC-State CoC1-Expr))
      (match exp
        ;; Here we must be representing casts as twosomes because the
        ;; cast nodes are still in the tree
        [(Cast exp t1 t2 lbl)
         (exp : CoC1-Expr <- (lfc-expr exp))
         (if (and (Fn? t1) (Fn? t2))
             ;; TODO maybe this should be (App (Fn-Caster exp) exp t1 t2 lbl)
             (return-state (Fn-Cast exp t1 t2 lbl))
             (return-state (Cast exp t1 t2 lbl)))]
        ;; Here we must be representing casts as Coercions because the coercion node is present
        [(Coerce (Proxy-Fn n args ret) exp)
         (exp : CoC1-Expr <- (lfc-expr exp))
         (lbl : Code-Label <- (get-normalize-coercion n))
         (App-Code lbl (list (Proxy-Fn n args ret) exp))]
        ;; TODO make this code work for both casts and coercions
        [(Lambda f* exp)
         (uid : Uid     <- (get-caster (length f*)))
         (exp : CoC1-Expr <- (lfc-expr exp))
         (return-state (Lambda f* (Castable uid exp)))]
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
        [(App exp exp*)
         (exp  : CoC1-Expr  <- (lfc-expr  exp))
         (exp* : CoC1-Expr* <- (lfc-expr* exp*))
         (return-state (App exp exp*))]
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
         (return (Gbox e))]
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
        [(Var id)    (return-state (Var id))]
        [(Quote lit) (return-state (Quote lit))]))))))))

(: lfc-expr* (-> C0-Expr* (State LFC-State CoC1-Expr*)))
(define (lfc-expr* e*) (map-state lfc-expr e*))

;; Recur through binding with the casting state
(: lfc-bnd* (-> C0-Bnd* (State LFC-State CoC1-Bnd*)))
(define (lfc-bnd* b*)
  (: lfc-bnd (-> C0-Bnd (State LFC-State CoC1-Bnd)))
  (define (lfc-bnd b)
    (match-let ([(cons i e) b])
      (do (bind-state : (State LFC-State CoC1-Bnd))
          (e : CoC1-Expr <- (lfc-expr e))
          (return-state (cons i e)))))
  (map-state lfc-bnd b*))

;; Using state passing style with a imutable hashtable that maps the arity of
;; a function to the untyped binding of a function that can cast that arity.
(define-type LFC-State (Pair Natural (HashTable Index CoC1-Bnd-Code)))
(define-type LFC-Ctr   (Index -> (State Nat CoC1-Bnd-Code)))

(: no-casters (HashTable Index CoC1-Bnd-Code))
(define no-casters (hasheq))

(: get-caster (LFC-Ctr -> (Index -> (State LFC-State Uid))))
(define ((get-coercer build) arity)
  (do (bind-state : (State Coercions Uid))
      ((cons next table) : LFC-State <- get-state)
      (let ([bnd? (hash-ref table arity #f)])
        (if bnd?
            (return-state (car bnd?))
            (let-values ([(bnd next) (run-state (build-code arity) next)])
              (do (bind-state : (State LFC-State Uid))
                  (_ : Null <-
                     (put-state (cons next (hash-set table arity bnd))))
                  (return-state (car bnd))))))))

(: get-caster (Index . -> . (State LFC-State Uid)))
(define (get-caster arity)
  (do (bind-state : (State LFC-State Uid))
      (state : LFC-State <- get-state)
      (match-let ([(cons next table) state])
        (let ([bnd? (hash-ref table arity #f)])
	  (if bnd?
	      (return-state (car bnd?))
	      (let-values ([(bnd next) (run-state (mk-caster-bnd arity) next)])
		(do (bind-state : (State LFC-State Uid))
                    (_ : Null <- (put-state (cons next (hash-set table arity bnd))))
                    (return-state (car bnd)))))))))

(define (build-fun-caster arity)
  (do (bind-state : (State Natural CoC1-Bnd-Code))
      (uid : Uid  <- (uid-state (format "cast_fn_~a" ary)))
      (code : CoC1-Code <- (build-caster-code arity uid))
      (return-state (cons uid code))))

(define (build-fun-coerce arity)
  (do (bind-state : (State Natural CoC1-Bnd-Code))
      (uid : Uid  <- (uid-state (format "apply_coerced_fn_~a" ary)))
      (code : CoC1-Code <- (build-fun- arity uid))
      (return-state (cons uid code))))

;; make a function that builds a function proxy for the cast of a specific
;; arity. This function is always of the following form
#|
(lambda (v t1 t2 l)
  (if (= (fn-type-arity t1) (fn-type-arity t2))
      (lambda (a ...)
        (cast (v (cast a (fn-type-arg t2) (fn-type-arg t1) l) ...)
              (fn-type-ret t1) (fn-type-ret t2) l)))
|#
(: mk-caster-fn (-> Index Uid (State Natural CoC1-Expr)))
(define (mk-caster-fn ary name)
  ;; create uids for all arguments to the proxy
  (: mk-caster-ids (-> Index Uid* (State Natural Uid*)))
  (define (mk-caster-ids ary vars)
    (if (zero? ary)
	(return-state vars)
        (do (bind-state : (State Natural Uid*))
            (uid : Uid <- (uid-state "v"))
            (mk-caster-ids (- ary 1) (cons uid vars)))))
  ;; create expression for the runtime cast of arguments to the proxy
  (: mk-casted-args (-> CoC1-Expr CoC1-Expr CoC1-Expr Uid* CoC1-Expr*))
  (define (mk-casted-args t1 t2 lbl uid*)
    (: loop (-> Uid* Index CoC1-Expr*))
    (define (loop uid* index)
      (let ([i (Quote index)])
        (if (null? uid*)
            '()
            (let ([val : CoC1-Expr (Var (car uid*))]
                  [t1 : CoC1-Expr (Type-Fn-arg t1 i)]
                  [t2 : CoC1-Expr (Type-Fn-arg t2 i)])
              (cons (Runtime-Cast val t2 t1 lbl)        ;; contravarient argument cast
                    (loop (cdr uid*) (cast (add1 index) Index)))))))
    (loop uid* 0))
  (do (bind-state : (State Natural CoC1-Expr))
      ;; create the uids for the casting function
      (fn : Uid <- (uid-state "f"))
      (t1 : Uid <- (uid-state "t1"))
      (t2 : Uid <- (uid-state "t2"))
      (lbl : Uid <- (uid-state "lbl"))
      (uid* : Uid* <- (mk-caster-ids ary '()))
      ;; create the vars for these uids
      (let* ([fn-var  : CoC1-Expr (Var fn)]
             [t1-var  : CoC1-Expr (Var t1)]
             [t2-var  : CoC1-Expr (Var t2)]
             [lbl-var : CoC1-Expr (Var lbl)]
             ;; arguments to the unproxied function call
             [args : CoC1-Expr* (mk-casted-args t1-var t2-var lbl-var uid*)]
             ;; formals for the casting function
             [caster-fml : Uid* (list fn t1 t2 lbl)]
             ;; cast the return of the unproxied application
             [t1-ret    : CoC1-Expr (Type-Fn-return t1-var)]
             [t2-ret    : CoC1-Expr (Type-Fn-return t2-var)]
             [call      : CoC1-Expr (App fn-var args)]
             [cast-call : CoC1-Expr (Runtime-Cast call t1-ret t2-ret lbl-var)]
             ;; application isn't performed if the arity is a mismatch
             [arity-t1 : CoC1-Expr (Type-Fn-arity t1-var)]
             [arity-t2 : CoC1-Expr (Type-Fn-arity t2-var)]
             [arity-test : CoC1-Expr (Op '= (list arity-t1 arity-t2))]
             ;; the closure to return if arity is correct
             [then-cast : CoC1-Expr (Lambda uid* (Castable name cast-call))]
             ;; the closure to return if arity is incorrect
             [else-blame : CoC1-Expr (Lambda '() (Castable name (Blame lbl-var)))]
             [check : CoC1-Expr (If arity-test then-cast else-blame)])
        ;; The entire casting function is now all constructed
        (return-state (Code caster-fml (Castable #f check))))))

;; coerce-raw-closure-n
;; Arrity Specific because c doesn't have a way of applying to an unkown number of values
(Code (coercion fun)
 (Lambda-Proxy (list a1 ...) (list coercion fun)
  (Interp-Coerce (Fn-Coercion-return coercion)
   (App-Uncoerced-closure fun
    (list (Interp-Coerce (Fn-Coercion-argument coercion 1) a1) ...)))))

;; Code Template for coerce-hybrid-closure
;; I am unsure of the best way to (Coerce-Raw-Closure n)
(Code (coercion code)
 (If (Proxied-Function? fun)
     (Let ([coercion (Compose-Function-Coercion (Proxied-Function-coercion fun) coercion)]
           [fun      (Proxied-Function-function fun)])
          ;; No need to check for a Failed coercion here the only thing that
          ;; should create a fail here is an arity mismatch wich I am convinced is
          ;; gaurenteed statically at this point
      (If (Identity-Coercion? coercion)
              fun
              (App-Label (Coerce-Raw-Closure n) '(coercion fun))))
     (App-Label (Coerce-Raw-Closure n) '(coercion fun))))

;; Function-coercion-compose -- Probably introduce during interpret-casts
;; Could be alot easier if this were also arrity specific
(Code (c1 c2)
 (Let ([arity (Function-Coercion-arity c1)])
  (Let ([new_crcn (New-Function-Coercion arity)]
        [id?      (Quote #t)])  
      (Begin
        (Repeat i 1 arity
         (Let ([tmp (Compose (Function-Coercion-argument c1 i)
                             (Function-Coercion-argument c2 i))])
          (Begin
            (Function-Coercion-argument! c3 i tmp)
            (If id?
                (If (Identity-Coercion? tmp)
                    (Assign tmp #t)
                    (Assign tmp #f))
                ()))))
        (Let ([tmp (Compose (Function-Coercion-return c1)
                            (Function-Coercion-return c2))])
         (Begin
           (Function-Coercion-return! c3 tmp)
           (If id?
               (If (Identity-Coercion? tmp)
                   (Identity-Coercion)
                   tmp)
               tmp))))) ))
;; With arity specialization
;; And here is expanded to Ifs in the actual code generated
(Code (c1 c2)
 (Let ([arg-n (Compose (Funciton-Coercion-argument c1 n)
                            (Function-Coercion-argument c2 n))] ...
            [ret   (Compose (Function-Coercion-return c1)
                            (Function-Coercion-return c2))])
  (If (AND (Identity? arg-n) ...)
      (Identity-Coercion)
      (Function-Coercions (List arg-n ...) ret))))
