#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: src/casts/convert-closures                                               |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
| Description: This pass sets up the structure of closures
+-------------------------------------------------------------------------------+
| Grammer:
+------------------------------------------------------------------------------|#
(require "../helpers.rkt"
         "../errors.rkt"
         "../configuration.rkt"
         "../language/cast-or-coerce5.rkt"
         "../language/cast-or-coerce6.rkt")
(provide
 convert-closures
 (all-from-out
  "../language/cast-or-coerce5.rkt"
  "../language/cast-or-coerce6.rkt"))

(define optimize-self-reference?
  (make-parameter #f))

(define optimize-known-call?
  (make-parameter #f))

(define optimize-well-known?
  (make-parameter #f))

(: convert-closures (Cast-or-Coerce5-Lang -> Cast-or-Coerce6-Lang))
(define (convert-closures prgm)
  (define cr  (cast-representation))
  (define fr 'Hybrid)
  (match-let ([(Prog (list name count type)
                     (Let-Static* tbnd* cbnd* exp)) prgm])
    (let* ([next : (Boxof Nat) (box count)]
           [bndp : (HashTable Integer BP) (make-hash)]
           [exp ;; This is an abusively long function call
            (cc-expr cr fr next bndp empty-env empty-env no-selfp exp)]
           [next : Nat (unbox next)])
      (Prog (list name count type)
       (Let-Static* tbnd* cbnd*
        (LetP (hash-values bndp) exp))))))

(define-type BP  CoC6-Bnd-Procedure)
(define-type BC  CoC6-Bnd-Closure)
(define-type BP* CoC6-Bnd-Procedure*)
(define-type BC* CoC6-Bnd-Closure*)
(define-type BD* CoC6-Bnd-Data*)

(define-type Env (HashTable Uid CoC6-Expr))
(define empty-env : Env (hash))

(define-type SelfP (-> Uid (Option CoC6-Expr)))
(define no-selfp : SelfP
  (lambda (u) #f))

(define-type Fn-Proxy-Representation (U 'Hybrid 'Data))

(: cc-expr (Cast-Representation
            Fn-Proxy-Representation
            (Boxof Nat) (HashTable Integer BP)
            Env Env SelfP CoC5-Expr
            -> CoC6-Expr))
(define (cc-expr cast-rep fn-proxy-rep next new-bnd-procs
                 code-env data-env selfp exp)
  ;; This is the only piece of code that should touch the unique counter
  (: next-uid! (String -> Uid))
  (define (next-uid! x)
    (let ([n (unbox next)])
      (set-box! next (add1 n))
      (Uid x n)))

  ;; Lookup or Create a Apply-Hybrid-Proxy-Fn
  (: get-apply-uid! (Integer Uid -> Uid))
  (define (get-apply-uid! i cast)
    (define apply-bnd? (hash-ref new-bnd-procs i #f))
    (cond
      [apply-bnd? => car]
      [else
       (define cp
         (next-uid! (string-append "apply_hybrid_" (number->string i))))
       (define hc (next-uid! "hybrid_closure"))
       (define a* (build-list i (lambda (a) (next-uid! "arg"))))
       (define v* (map (inst Var Uid) a*))
       (define clos-clos (next-uid! "closure_field"))
       (define crcn-clos (next-uid! "corcion_field"))
       (define clos      (next-uid! "closure"))
       (define crcn      (next-uid! "corcion"))
       (define bnd
         (cons
          cp
          (Procedure hc a* cp #f (list clos-clos crcn-clos)
                     (Let `((,clos . ,(Closure-ref hc clos-clos))
                            (,crcn . ,(Closure-ref hc crcn-clos)))
                          (cast-apply-cast cast (Var clos) v* (Var crcn))))))
       (hash-set! new-bnd-procs i bnd)
       cp]))


  ;; Make a procedure that references a closure data structure
  (: cc-make-proc (Env -> ((Pairof Uid CoC5-Bnd-Lambda) -> BP)))
  (define ((cc-make-proc code-env) b)
    ;; destructure the inner-closure-id and lambda binding
    (match-let ([(cons clos (cons code (Lambda fml* (Free ctr? uid* e)))) b])
      ;; create expressions that represent accessing the closure for each free var
      (define clos-ref* (map (mk-clos-ref clos) uid*))
      ;; create a new environment that maps uses of variables to closure access
      (define data-env   (extend* (hash) uid* clos-ref*))
      ;; create a new self reference check
      (define self (Var clos))
      (define (selfp [u : Uid]) (and (equal? u code) self))
      ;; Process the body of the lambda replacing free variable uses
      ;; with closure accesses
      (define body (recur/env code-env data-env selfp e))
      ;; return the new procedure binding after recuring on
      (cons code (Procedure clos fml* code ctr? uid* body))))


  ;; Make a closure data structure that can be reference by a procedure
  (: cc-make-clos (Env Env SelfP -> ((Pairof Uid CoC5-Bnd-Lambda) -> BC)))
  (define ((cc-make-clos code-env data-env selfp) b)
    (match-let ([(cons clos (cons code (Lambda fml* (Free ctr? uid* _)))) b])
      ;; Get the Variables that need to be bound by the closure
      (define bound* (map (lookup-in data-env selfp) uid*))
      (define ctr    (if ctr? (Code-Label ctr?) #f))
      (define data   (Closure-Data (Code-Label code) ctr bound*))
      ;; return the new closure binding
      (cons clos data)))

  ;; Process Expression in a particular closure environment
  (: recur/env (Env Env SelfP CoC5-Expr -> CoC6-Expr))
  (define (recur/env code-env data-env selfp exp)
    
    ;; Build new closure binding forms
    (: cc-bnd-lambda* (CoC5-Bnd-Lambda* -> (Values BP* BC* Env Env)))
    (define (cc-bnd-lambda* b*)
      ;; Make some code labels from the original bindings
      (define cp* : Uid* (map (inst car Uid Any) b*))
      (define cl* : CoC6-Expr* (map (inst Code-Label Uid) cp*))
      ;; Make some uids for the inner(ic) and outer(oc) closure variables
      (define ic* : Uid* (map (clos-uid next-uid!) cp*))
      (define oc* : Uid* (map (clos-uid next-uid!) cp*))
      ;; Extend the environments with new closure and code info
      (define oc-var* (map (inst Var Uid) oc*))
      (define ic-env (extend* code-env ic* cl*))
      (define oc-env (extend* code-env oc* cl*))
      (define od-env (extend* data-env cp* oc-var*))
      (define ic.b*  (map (inst cons Uid CoC5-Bnd-Lambda) ic* b*))
      (define oc.b*  (map (inst cons Uid CoC5-Bnd-Lambda) oc* b*))
      ;; Make the new bindings
      (define bp* : BP* (map (cc-make-proc ic-env) ic.b*))
      (define bd* : BC* (map (cc-make-clos oc-env od-env selfp) oc.b*))
      ;; return the new bindings and environments
      (values bp* bd* oc-env od-env))

    ;; recure through the expressions in the grammar
    (: recur (CoC5-Expr -> CoC6-Expr))
    (define (recur e)
      (match e
        ;; The interesting cases:
        ;; letrec are actually code bindings with shared state closure creating bodies
        [(Letrec b* e)
         (let-values ([(bp bc c-env d-env) (cc-bnd-lambda* b*)])
           (LetP bp (LetC bc (recur/env c-env d-env selfp e))))]
        ;; Function cast extraction removes the cast closure (could optimize to cast pointer)
        [(Fn-Caster e) (Closure-caster (recur e))]
        ;; Applications get the code pointer and pass the closure as the first argument
        [(App-Fn (app recur e) (app recur* e*))
         ;; When optimize known call is in effect any known closure
         ;; that is applied has the code label inlined
         ;; otherwise the closure pointer is extracted
         (cond
           [(Var? e)
            (define code?
              (and (optimize-known-call?) (hash-ref code-env (Var-id e) #f)))
            (cond
              [code? (App-Closure code? e e*)]
              [else (App-Closure (Closure-code e) e e*)])]
           [else
            (define id  (next-uid! "tmp_closure"))
            (define var (Var id))
            (Let (list (cons id e))
                 (App-Closure (Closure-code var) var e*))])]
        ;; If we are using hybrid representation we can treat
        ;; this like a normal apply-closure.
        ;; Alternatively if we are using data representation
        ;; we need to cast the arguments
        [(App-Fn-or-Proxy cast-uid e e*)
         (case cast-rep
           [(Coercions Hyper-Coercions)
            (case fn-proxy-rep
              ;; If it is a hybrid representation then proxies and closures
              ;; are applied the same way.
              [(Hybrid)
               (recur (App-Fn e e*))]
              ;; If it a data representation and it is a known definition
              ;; site then it can't be a proxy
              [(Data)
               (cond
                 [(and (Var? e) (optimize-known-call?)
                       (hash-ref code-env (Var-id e) #f))
                  => (lambda ([c : CoC6-Expr])
                       (App-Closure c e (recur* e*)))]
                 ;; othewise we build code to cast the arguments
                 ;; at the call site.
                 [else
                  (define e^  : CoC6-Expr  (recur  e))
                  (define e*^ : CoC6-Expr* (recur* e*))

                  (define prox (next-uid! "maybe_proxy"))
                  (define pvar (Var prox))
                  (define func (next-uid! "closure"))
                  (define fvar (Var prox))
                  (define crcn (next-uid! "coercion"))
                  (define cvar (Var crcn))
                  (Let (list (cons prox e^))
                       (If (Fn-Proxy-Huh (Var prox))
                           (Let (list (cons func (Fn-Proxy-Closure pvar))
                                      (cons crcn (Fn-Proxy-Coercion pvar)))
                                (cast-apply-cast cast-uid fvar e*^ cvar))
                           (App-Closure (Closure-code pvar) pvar e*^)))])]
              [else (error 'covert-closures "Unkown Fn-Proxy Representaion")])]
           [else (error 'covert-closures "Unkown Cast Representaion")])]
        [(Fn-Proxy (list i cast) (app recur e1)(app recur e2))
         (case cast-rep
           [(Coercions Hyper-Coercions)
            (case fn-proxy-rep              
              [(Hybrid) (Hybrid-Proxy (get-apply-uid! i cast) e1 e2)]
              [(Data)   (Fn-Proxy i e1 e2)]
              [else (error 'convert-closures "Unkown Fn-Proxy Representation")])]
           [else (error 'convert-closures "unkown cast representation")])]
        [(Fn-Proxy-Huh (app recur e))
         (case cast-rep
           [(Coercions Hyper-Coercions)
            (case fn-proxy-rep
              [(Hybrid) (Hybrid-Proxy-Huh e)]
              [(Data)   (Fn-Proxy-Huh e)]
              [else (error 'convert-closures "Unkown Fn-Proxy Representation")])]
           [else (error 'convert-closures "unkown cast representation")])]
        [(Fn-Proxy-Closure (app recur e))
         (case cast-rep
           [(Coercions Hyper-Coercions)
            (case fn-proxy-rep
              [(Hybrid) (Hybrid-Proxy-Closure e)]
              [(Data)   (Fn-Proxy-Closure e)]
              [else (error 'convert-closures "Unkown Fn-Proxy Representation")])]
           [else (error 'convert-closures "unkown cast representation")])]
        [(Fn-Proxy-Coercion (app recur e))
         (case cast-rep
           [(Coercions Hyper-Coercions)
            (case fn-proxy-rep
              [(Hybrid) (Hybrid-Proxy-Coercion e)]
              [(Data)   (Fn-Proxy-Coercion e)]
              [else (error 'convert-closures "Unkown Fn-Proxy Representation")])]
           [else (error 'convert-closures "unkown cast representation")])]
        ;; varibles that are free are extracted from the closure
        ;; while variable that are not bound by the closure are rebuilt
        [(Var u) (lookup data-env selfp u)]
        ;; The rest of the cases are just recuring into sub expressions
        [(Let (app (cc-bnd-data* recur) b*) (app recur e)) (Let b* e)]
        [(If (app recur t) (app recur c) (app recur a)) (If t c a)]
        [(Switch e c* d)
         (: recur-case : (Switch-Case CoC5-Expr) -> (Switch-Case CoC6-Expr))
         (define/match (recur-case c)
           [((cons l r)) (cons l (recur r))])
         (Switch (recur e) (map recur-case c*) (recur d))]
        [(Op p (app recur* e*)) (Op p e*)]
        [(and nop (No-Op)) nop]
        [(Quote k) (Quote k)]
        [(Tag t)   (Tag t)]
        ;; Observables Representation
        [(Blame (app recur e)) (Blame e)]
        [(Observe (app recur e) t) (Observe e t)]
        ;; control flow for effects
        [(Begin (app recur* e*) (app recur e)) (Begin e* e)]
        [(Repeat i (app recur e1) (app recur e2) a (app recur e3) (app recur e4))
         (Repeat i e1 e2 a e3 e4)]
        [(Break-Repeat) (Break-Repeat)]
        ;; Type Representation
        [(Type t) (Type t)]
        [(Type-Tag (app recur e)) (Type-Tag e)]
        [(Type-GRef-Of (app recur e)) (Type-GRef-Of e)]
        [(Type-GVect-Of (app recur e)) (Type-GVect-Of e)]
        [(Type-Fn-arg (app recur e) (app recur i)) (Type-Fn-arg e i)]
        [(Type-Fn-return (app recur e)) (Type-Fn-return e)]
        [(Type-Fn-arity (app recur e)) (Type-Fn-arity e)]
        [(Type-Fn-Huh (app recur e)) (Type-Fn-Huh e)]
        [(Type-GVect-Huh (app recur e)) (Type-GVect-Huh e)]
        [(Type-GRef-Huh (app recur e)) (Type-GRef-Huh e)]
        [(Type-Dyn-Huh (app recur e)) (Type-Dyn-Huh e)]
        
        [(Labels (app (cc-bnd-code* recur) b*) (app recur e))
         (Labels b* e)]
        [(App-Code (app recur e) (app recur* e*))
         (App-Code e e*)]
        [(Code-Label u)  (Code-Label u)]

        ;; Coercion Representation Stuff
        [(Quote-Coercion c)
         (Quote-Coercion c)]
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
        [(Make-Fn-Coercion u (app recur e1) (app recur e2) (app recur e3))
         (Make-Fn-Coercion u e1 e2 e3)]
        [(Fn-Coercion (app recur* e*) (app recur e))
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
        ;; Function Proxy Representation
        
        ;; Gaurded Representation
        [(Unguarded-Box (app recur e))(Unguarded-Box e)]
        [(Unguarded-Box-Ref (app recur e)) (Unguarded-Box-Ref e)]
        [(Unguarded-Box-Set! (app recur e1) (app recur e2))
         (Unguarded-Box-Set! e1 e2)]
        [(Unguarded-Vect (app recur e1) (app recur e2))
         (Unguarded-Vect e1 e2)]
        [(Unguarded-Vect-Ref (app recur e1) (app recur e2))
         (Unguarded-Vect-Ref e1 e2)]
        [(Unguarded-Vect-Set!
          (app recur e1) (app recur e2) (app recur e3))
         (Unguarded-Vect-Set! e1 e2 e3)]
        [(Guarded-Proxy-Huh (app recur e))
         (Guarded-Proxy-Huh e)]
        [(Guarded-Proxy (app recur e) r)
         (match r
           [(Twosome (app recur t1) (app recur t2) (app recur l))
            (Guarded-Proxy e (Twosome t1 t2 l))]
           [(Coercion (app recur c))
            (Guarded-Proxy e (Coercion c))])]
        [(Guarded-Proxy-Ref (app recur e))
         (Guarded-Proxy-Ref e)]
        [(Guarded-Proxy-Source (app recur e))
         (Guarded-Proxy-Source e)]
        [(Guarded-Proxy-Target (app recur e))
         (Guarded-Proxy-Target e)]
        [(Guarded-Proxy-Blames (app recur e))
         (Guarded-Proxy-Blames e)]
        [(Guarded-Proxy-Coercion (app recur e))
         (Guarded-Proxy-Coercion e)]
        [(Unguarded-Vect-length (app recur e)) (Unguarded-Vect-length e)]
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
        [(Create-tuple (app recur* e*)) (Create-tuple e*)]
        [(Copy-Tuple (app recur n) (app recur v))
         (Copy-Tuple n v)]
        [(Tuple-proj e i) (Tuple-proj (recur e) (recur i))]
        [(Tuple-Coercion-Huh e) (Tuple-Coercion-Huh (recur e))]
        [(Tuple-Coercion-Num e) (Tuple-Coercion-Num (recur e))]
        [(Tuple-Coercion-Item e i) (Tuple-Coercion-Item (recur e) (recur i))]
        [(Coerce-Tuple uid e1 e2) (Coerce-Tuple uid (recur e1) (recur e2))]
        [(Coerce-Tuple-In-Place uid e1 e2 e3)
         (Coerce-Tuple-In-Place uid (recur e1) (recur e2) (recur e3))]
        [(Cast-Tuple uid e1 e2 e3 e4) (Cast-Tuple uid (recur e1) (recur e2) (recur e3) (recur e4))]
        [(Cast-Tuple-In-Place uid e1 e2 e3 e4 e5)
         (Cast-Tuple-In-Place uid (recur e1) (recur e2) (recur e3) (recur e4) (recur e5))]
        [(Type-Tuple-Huh e) (Type-Tuple-Huh (recur e))]
        [(Type-Tuple-num e) (Type-Tuple-num (recur e))]
        [(Type-Tuple-item e i) (Type-Tuple-item (recur e) (recur i))]
        [(Make-Tuple-Coercion uid t1 t2 lbl) (Make-Tuple-Coercion uid (recur t1) (recur t2) (recur lbl))]
        [(Mediating-Coercion-Huh e) (Mediating-Coercion-Huh (recur e))]
        [(Construct t v (app recur* e*))
         (Construct t v e*)]
        [(Access t f (app recur e) i?)
         (Access t f e (if i? (recur i?) #f))]
        [(Check t p (app recur e) (app recur* e*))
         (Check t p e e*)]
        [other (error 'Convert-Closures "unmatched ~a" other)]))

    ;; recur through a list of expressions
    (: recur* (CoC5-Expr* -> CoC6-Expr*))
    (define (recur* e*) (map recur e*))
    
    (recur exp))
  (recur/env code-env data-env selfp exp))

;; THE following are helpers that don't require any
;; of the internal state of this pass


;; make an expression the expresses the notion of fetching
;; a variable from a closure
(: mk-clos-ref : Uid -> (Uid -> CoC6-Expr))
(define (mk-clos-ref clos)
  (lambda ([fvar : Uid]) : CoC6-Expr
          (Closure-ref clos fvar)))

;; Recur through bound data
(: cc-bnd-data*
   ((CoC5-Expr -> CoC6-Expr) -> CoC5-Bnd-Data* -> CoC6-Bnd-Data*))
(define ((cc-bnd-data* cc-expr) b*)
  (: help (CoC5-Bnd-Data -> CoC6-Bnd-Data))
  (define (help b)
    (cons (car b) (cc-expr (cdr b))))
  (map help b*))

;; Recur through bound code
(: cc-bnd-code*
   ((CoC5-Expr -> CoC6-Expr) -> CoC5-Bnd-Code* -> CoC6-Bnd-Code*))
(define ((cc-bnd-code* cc-expr) b*)
  ;; Handle a single code binding 
  (: help (CoC5-Bnd-Code -> CoC6-Bnd-Code))
  (define (help b)
    (match-let ([(cons u (Code u* e)) b])
      (cons u (Code u* (cc-expr e)))))
  (map help b*))

;; create a clos annotated unique name
(: clos-uid ((String -> Uid) -> (Uid -> Uid)))
(define ((clos-uid uid!) u)
  (uid! (string-append (Uid-prefix u) "_clos")))

;; create a casted fn application call site
(: cast-apply-cast (Uid (Var Uid) CoC6-Expr* (Var Uid) -> CoC6-Expr))
(define (cast-apply-cast cast-uid fun arg* crcn)
  (define cast (Code-Label cast-uid))
  (define i*   (build-list (length arg*) (lambda ([i : Index]) i)))
  (: help (CoC6-Expr Integer -> CoC6-Expr))
  (define (help e i)
    (App-Code cast (list e (Fn-Coercion-Arg crcn (Quote i)) (Quote 0))))
  (App-Code cast (list (App-Closure (Closure-code fun) fun (map help arg* i*))
                       (Fn-Coercion-Return crcn)
                       (Quote 0))))


;; Lookup a variables local access instruction
;; within an environment that represents 
(: lookup : Env SelfP Uid -> CoC6-Expr)
(define (lookup env selfp uid)
  ;; If optimize self reference is enabled then check
  ;; to see if this is a self reference.
  (or (and (optimize-self-reference?) (selfp uid))
      (hash-ref env uid (thunk (Var uid)))))

(: lookup-in : Env SelfP -> (Uid -> CoC6-Expr))
(define ((lookup-in env selfp) uid)
  (lookup env selfp uid))

(: extend* :  Env Uid* CoC6-Expr* -> Env)
(define (extend* env k* v*)
  (for/fold ([env : Env env])
            ([k : Uid k*] [v : CoC6-Expr v*])
    (hash-set env k v)))


