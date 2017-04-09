#lang typed/racket/base
#|
Pass: interpret casts with hyper-coercions
Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                            
Used By: impose-cast-semantics (./impose-cast-semantics.rkt)
Cliff Notes: Implement cast samantics via a runtime data structure
currently know as hyper-coercions (minmally bushy version of 
coercions). We hope that these lead to significantly better performance
due to better locality, space consumption, and code organization.

TODO: abstract all common code between coercions and hyper-coercions
TODO: come up with iterative code that implements tuple and function cast
      in the abstracted common code
TODO: implement optimizer.
|#
(provide interpret-casts/hyper-coercions)
(require "../language/cast-or-coerce3.rkt"
         "../language/cast0.rkt"
         "../language/syntax.rkt"
         "../configuration.rkt"
         (except-in "./interpret-casts-help.rkt"
                    cond$ op=? and$ or$)
         (submod "../logging.rkt" typed)
         racket/match
         racket/list
         (for-syntax syntax/parse typed/racket/base))




(: interpret-casts/hyper-coercions
   : Cast0.5-Lang -> Cast-or-Coerce3-Lang)
#|
Compile the cast in the program to calls to a cast interpreter 
that applies runtime cast object (hyper-coercions) to the
expression being casted. This cast interpreter is the runtime
function know as "apply_hyper_coercion", it relies on several
other runtime functions that are also generated and added
to the program via this code.

This code breaks the task down into several distinct subtasks
that can be located throughout this file:
1) Convert type-based casts to hyper-coercion plus call to
   runtime interpreter.
2) Generate code for the runtime that implements
3) Manually optimize operations on dynamic values to prevent
   obvious one-time use allocations.
|#
(define (interpret-casts/hyper-coercions prgm)
  (match-define (Prog (list name next type) e) prgm)
  
  ;; All the implicit state of this program
  (define next-unique (make-unique-counter next))
  (define fn-casts : (HashTable Nat CoC3-Bnd-Code)
    (make-hasheq))

  (parameterize ([current-unique-counter next-unique])

    (define-values (interp-cast-uid
                    interp-cast
                    interp-med-cast
                    make-coercion
                    interp-make-coercion
                    interp-compose-coercions
                    interp-compose-med-coercions
                    cast-bindings)
      (make-casting-helpers))    
    
    (define-values (pbox-ref pbox-set!
                    pvec-ref pvec-set! pvec-len
                    proxied-reference-bindings)
      (make-proxied-reference-helpers! interp-cast))

    (define-values (mbox-ref mbox-set!
                    mvec-ref mvec-set!
                    mono-reference-bindings)
      (make-mono-reference-helpers! interp-cast interp-make-coercion))

    (define-values (dyn-pbox-ref dyn-pbox-set!
                    dyn-pvec-ref dyn-pvec-set! dyn-pvec-len
                    dyn-mbox-ref dyn-mbox-set!
                    dyn-mvec-ref dyn-mvec-set!
                    dyn-fn-app dyn-tup-prj
                    dynamic-operations-bindings)
      (make-dynamic-operations-helpers! interp-cast-uid
                                        interp-cast
                                        interp-make-coercion
                                        pbox-ref pbox-set!
                                        pvec-ref pvec-set! pvec-len
                                        mbox-ref mbox-set!
                                        mvec-ref mvec-set!))
    
    (define get-fn-cast!
      (make-get-fn-cast! fn-casts "coerce_fn" interp-cast-uid
                         interp-compose-coercions))

    (define map-expr! (make-map-expr interp-cast-uid
                                     interp-cast
                                     interp-med-cast
                                     make-coercion
                                     interp-make-coercion
                                     pbox-ref pbox-set!
                                     pvec-ref pvec-set! pvec-len
                                     mbox-ref mbox-set!
                                     mvec-ref mvec-set!
                                     dyn-pbox-ref dyn-pbox-set!
                                     dyn-pvec-ref dyn-pvec-set! dyn-pvec-len
                                     dyn-mbox-ref dyn-mbox-set!
                                     dyn-mvec-ref dyn-mvec-set!
                                     dyn-fn-app dyn-tup-prj
                                     get-fn-cast!))
    ;; This map-expr! call must occur before next-unique is
    ;; extracted because new unique variable are allocated in
    ;; this pass. Additionally calls to get-fn-caster! will
    ;; cause additional code to be generated for each arity
    ;; of function that exists; this code is extracted from
    ;; fn-casts.
    (define new-e (map-expr! e))

    ;; Reconstruct the entire program
    (Prog (list name (unique-counter-next! next-unique) type)
     (Labels (append
              cast-bindings 
              proxied-reference-bindings
              mono-reference-bindings
              dynamic-operations-bindings
              (hash-values fn-casts))
       new-e))))


;; Return a procedure that maps the pass over an entire expression AST
;; I am including this as the second definition because it is a good
;; guide to the rest of this pass -- Andre 
(define (make-map-expr
         [interp-cast-uid : Uid]
         [interp-cast  : Interp-Cast-Type]
         [interp-med-cast : Interp-Cast-Type]
         [make-coercion : Make-Coercion-Type]
         [interp-make-coercion : Interp-Make-Coercion-Type]
         [pbox-ref  : PBox-Ref-Type]
         [pbox-set! : PBox-Set-Type]
         [pvec-ref  : PVec-Ref-Type]
         [pvec-set! : PVec-Set-Type]
         [pvec-len  : PVec-Len-Type]
         [mbox-ref  : MBox-Ref-Type]
         [mbox-set! : MBox-Set-Type]
         [mvec-ref  : MVec-Ref-Type]
         [mvec-set! : MVec-Set-Type]
         [dyn-pbox-ref  : Dyn-PBox-Ref-Type]
         [dyn-pbox-set! : Dyn-PBox-Set-Type]
         [dyn-pvec-ref  : Dyn-PVec-Ref-Type]
         [dyn-pvec-set! : Dyn-PVec-Set-Type]
         [dyn-pvec-len  : Dyn-PVec-Len-Type]
         [dyn-mbox-ref  : Dyn-MBox-Ref-Type]
         [dyn-mbox-set! : Dyn-MBox-Set-Type]
         [dyn-mvec-ref  : Dyn-MVec-Ref-Type]
         [dyn-mvec-set! : Dyn-MVec-Set-Type]
         [dyn-fn-app    : Dyn-Fn-App-Type]
         [dyn-tup-prj   : Dyn-Tup-Prj-Type]
         [get-fn-cast! : (Nat -> Uid)])
  : (C0-Expr -> CoC3-Expr) 
  ;; map the pass through lists of expressions
  (: recur* (C0-Expr* -> CoC3-Expr*))
  (define (recur* e*) (map recur e*))
  ;; map the pass through lists of bindings
  (: recur-bnd* (C0-Bnd* -> CoC3-Bnd*))
  (define (recur-bnd* b*)
    (map (λ ([b : C0-Bnd]) (cons (car b) (recur (cdr b)))) b*))
  (: recur : C0-Expr -> CoC3-Expr)
  (define (recur e)
    (debug 'interpret-cast-with-hyper-coercions/map-expr e)
    (match e
      ;; Casts get turned into calls to the cast interpreter with
      ;; hyper-coercion. The later pass makes this slightly more
      ;; efficient by statically allocating the coercion object.
      [(Cast (app recur e) (Twosome t1 t2 l))
       (define hc (make-coercion t1 t2 l))
       (debug 'interp-cast/hyper-coercions hc)
       (cond
         [(not (optimize-first-order-coercions?)) (interp-cast e hc)]
         [else 
          (match hc
            [(Quote-HCoercion (HC #f t1 #f #f t2 m))
             (match m
               [(Identity) e]
               [(Fn a _ _) #:when (direct-fn-cast-optimization?)
                ;; Fn-casts use specialize code for their arity
                ;; Since we are generating it here it is trivial to
                ;; directly call that code.
                (let ([caster (get-fn-cast! a)])
                  (App-Code (Code-Label caster) (list e (Quote-HCoercion m))))]
               [m (interp-med-cast e (Quote-HCoercion m))])]
            [hc (interp-cast e hc)])])]
      ;; Lambdas add a extra meta information field that ultimately
      ;; turns into a method for casting a particular arrity at runtime.
      [(Lambda f* (app recur exp))
       (let ([caster (get-fn-cast! (length f*))])
         (Lambda f* (Castable caster exp)))]
      ;; Applications get turned into an application that "checks for the
      ;; the presence of proxies" This eventually gets optimized aways
      ;; into a functional proxy representation. 
      [(App (app recur e) (app recur* e*))
       (App-Fn-or-Proxy interp-cast-uid e e*)]
      ;; Use make coercion to implement dynamic application without
      ;; allocating a one time use coercion.
      [(Dyn-Fn-App (app recur e) (app recur* e*) t* l)
       (define arg-names
         `("blame_info" "dyn_fn" . ,(make-list (length e*) "dyn_fn_arg")))
       (match-define-values (b* (cons lbl (cons v v*)))
        (bnd-non-vars next-uid! (cons (Quote l) (cons e e*)) #:names arg-names))
       (cond
         [(null? b*) (dyn-fn-app v v* t* lbl)]
         [else (Let b* (dyn-fn-app v v* t* lbl))])]
      ;; Transformation to lower guarded reference types
      ;; Guarded Operations on Guarded values are turned into
      ;; calls/inlinings of the runtime proceedures that perform
      ;; proxied reads and writes.
      ;;-------------------------------------------------------------------
      ;; Every Guarded Reference Starts As an Unguarded box
      [(Gbox (app recur e)) (Unguarded-Box e)]
      [(Gvector (app recur n) (app recur init)) (Unguarded-Vect n init)]
      ;; Unboxing calls off to the helpers we have defined
      [(Gvector-ref (app recur v) (app recur i))
       (match-define-values (b* (list v^ i^))
        (bnd-non-vars next-uid! (list v i) #:names '("pvec" "index")))
       (cond
         [(null? b*) (pvec-ref v^ i^)]
         [else (Let b* (pvec-ref v^ i^))])]
      [(Gunbox (app recur b))
       (if (Var? b)
           (pbox-ref b)
           (let ([u (next-uid! "pbox")])
             (Let (list (cons u b)) (pbox-ref (Var u)))))]
      [(Dyn-GRef-Ref (app recur e) l)
       (match-define-values (b* (list e^ l^))
         (bnd-non-vars next-uid! (list e (Quote l))
                       #:names (list "dyn_pbox" "blame_info")))
       (cond
         [(null? b*) (dyn-pbox-ref e^ l^)]
         [else (Let b* (dyn-pbox-ref e^ l^))])]
      [(Dyn-GVector-Ref (app recur e) (app recur i) l)
       (match-define-values (b* (list e^ i^ l^))
         (bnd-non-vars next-uid! (list e i (Quote l))
                       #:names (list "dyn_pvec" "index" "blame_info")))
       (cond
         [(null? b*) (dyn-pvec-ref e^ i^ l^)]
         [else (Let b* (dyn-pvec-ref e^ i^ l^))])]
      ;; Setting a Gaurded reference results in iteratively applying
      ;; all the guarding casts to the value to be written.
      ;; Most of the work is already done but the code requires values
      ;; so there is a little repetative to let bind any values that
      ;; haven't been evaluated.
      [(Gbox-set! (app recur b) (app recur w))
       (match-define-values (b* (list b^ w^))
        (bnd-non-vars next-uid! (list b w)
                      #:names '("pbox" "write_val")))
       (cond
         [(null? b*) (pbox-set! b^ w^)]
         [else (Let b* (pbox-set! b^ w^))])]
      [(Gvector-set! (app recur v) (app recur i) (app recur w))
       (match-define-values (b* (list v^ i^ w^))
        (bnd-non-vars next-uid! (list v i w)
                      #:names '("pvec" "index" "write_val")))
       (cond
         [(null? b*) (pvec-set! v^ i^ w^)]
         [else (Let b* (pvec-set! v^ i^ w^))])]
      [(Dyn-GRef-Set! (app recur e1) (app recur e2) t l)
       (match-define-values (b* (list e1^ e2^ l^))
        (bnd-non-vars next-uid! (list e1 e2 (Quote l))
                      #:names (list "dyn_pbox" "write_val" "blame_info")))
       (cond
         [(null? b*) (dyn-pbox-set! e1^ e2^ (Type t) l^)]
         [else (Let b* (dyn-pbox-set! e1^ e2^ (Type t) l^))])]
      [(Dyn-GVector-Set! (app recur e1) (app recur i) (app recur e2) t l)
       (match-define-values (b* (list e1^ i^ e2^ l^))
        (bnd-non-vars next-uid! (list e1 i e2 (Quote l))
                      #:names (list "dyn_pvec" "index" "write_val" "blame_info")))
       (cond
         [(null? b*) (dyn-pvec-set! e1^ i^ e2^ (Type t) l^)]
         [else (Let b* (dyn-pvec-set! e1^ i^ e2^ (Type t) l^))])]
      [(Gvector-length (app recur e))
       (cond
         [(Var? e) (pvec-len e)]
         [else
          (define u (next-uid! "pvect"))
          (Let `([,u . ,e]) (pvec-len (Var u)))])]
      #;[(Dyn-Gvector-length (app recur e)) (error 'todo)]
      ;; TODO figure out how to handle Monotonic reference types
      ;; Get rid of this pain in the ass having uids in expression
      ;; position (why is this needed?!?)
      [(MBoxCastedRef addr t)
       (match-define-values (b* (list t^))
         (bnd-non-vars next-uid! (list (Type t)) #:names '("type")))
       (if (null? b*)
           (mbox-ref (Var addr) t^)
           (Let b* (mbox-ref (Var addr) t^)))]
      [(MBoxCastedSet! addr (app recur e) t)
       (match-define-values (b* (list e^ t^))
         (bnd-non-vars next-uid! (list e (Type t)) #:names '("write_val" "type")))
       (if (null? b*)
           (mbox-set! (Var addr) e^ t^)
           (Let b* (mbox-set! (Var addr) e^ t^)))]
      [(MVectCastedRef addr (app recur i) t)
       (match-define-values (b* (list i^ t^))
         (bnd-non-vars next-uid! (list i (Type t)) #:names '("index" "type")))
       (if (null? b*)
           (mvec-ref (Var addr) i^ t^)
           (Let b* (mvec-ref (Var addr) i^ t^)))]
      [(MVectCastedSet! addr (app recur i) (app recur e) t)
       (match-define-values (b* (list i^ e^ t^))
         (bnd-non-vars next-uid! (list i e (Type t)) #:names '("index" "write_val" "type")))
       (if (null? b*)
           (mvec-set! (Var addr) i^ e^ t^)
           (Let b* (mvec-set! (Var addr) i^ e^ t^)))]
      [(Dyn-MRef-Ref (app recur e) l)
       (match-define-values (b* (list e^ l^))
         (bnd-non-vars next-uid! (list e (Quote l))
                       #:names (list "dyn_mbox" "blame_info")))
       (Let b* (dyn-mbox-ref e^ l^))]
      [(Dyn-MRef-Set! (app recur e1) (app recur e2) t l)
       (match-define-values (b* (list e1^ e2^ l^))
         (bnd-non-vars next-uid! (list e1 e2 (Quote l))
                       #:names (list "dyn_mbox" "write_val" "blame_info")))
       (Let b* (dyn-mbox-set! e1^ e2^ (Type t) l^))]
      [(Dyn-MVector-Ref (app recur e) (app recur i) l)
       (match-define-values (b* (list e^ i^ l^))
         (bnd-non-vars next-uid! (list e i (Quote l))
                       #:names (list "dyn_mvec" "index" "blame_info")))
       (Let b* (dyn-mvec-ref e^ i^ l^))]
      [(Dyn-MVector-Set! (app recur e1) (app recur i) (app recur e2) t l)
       (match-define-values (b* (list e1^ i^ e2^ l^))
         (bnd-non-vars next-uid! (list e1 i e2 (Quote l))
                       #:names (list "dyn_mvec" "index" "write_val" "blame_info")))
       (Let b* (dyn-mvec-set! e1^ i^ e2^ (Type t) l^))]
      ;; Completely statically typed monotonic need no runtime proceedures.
      ;; They are only kept different seperate from unguarded ops because
      ;; there layout is slightly different latter. 
      ;; Long-Term TODO: Why does the name ove these change?
      ;; Does their semantics change?
      [(Mvector-ref (app recur e1) (app recur e2))
       (Mvector-val-ref e1 e2)]
      [(Mvector-set! (app recur e1) (app recur e2) (app recur e3))
       (Mvector-val-set! e1 e2 e3)]
      [(Munbox (app recur e))
       (Mbox-val-ref e)]
      [(Mbox-set! (app recur e1) (app recur e2))
       (Mbox-val-set! e1 e2)]
      [(Mvector-length e) (Mvector-length (recur e))]
      [(Mbox (app recur e) t) (Mbox e t)]
      [(Mvector (app recur e1) (app recur e2) t) (Mvector e1 e2 t)]      
      ;; While tuples don't get any special attention in this pass
      ;; dynamic tuple projection needs to get dusugared
      [(Create-tuple e*) (Create-tuple (recur* e*))]
      [(Tuple-proj e i) (Tuple-proj (recur e) (Quote i))]
      #;[(Dyn-Tuple-proj (app recur e) i l) (dyn-tup-prj e (Quote i) l)]
      ;; Simple Recursion Patterns
      [(Observe (app recur e) t)
       (Observe e t)]
      [(Letrec bnd* exp)
       (Letrec (recur-bnd* bnd*) (recur exp))]
      [(Let bnd* exp)
       (Let (recur-bnd* bnd*) (recur exp))]
      [(Op p exp*)
       (Op p (recur* exp*))]
      [(If tst csq alt)
       (If (recur tst) (recur csq) (recur alt))]
      [(Switch e c* d)
       (Switch (recur e) (map-switch-case* recur c*) (recur d))]
      [(Begin e* e)
       (Begin (recur* e*) (recur e))]
      [(Repeat i e1 e2 a e3 e4)
       (Repeat i (recur e1) (recur e2) a (recur e3) (recur e4))]
      [(Var id)    (Var id)]
      [(Quote lit) (Quote lit)]
      [(and noop (No-Op)) noop]      
      [other
       (error 'cast/interp-casts-with-hyper-coercion "unmatched ~a" other)]))
  ;; Body of map-expr
  ;; just return the recursion procedure
  recur)

;; This next section of code are the procedures that build all
;; of the runtime code for hyper coercions based casting.

(define-type Interp-Compose-Coercions-Type (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Interp-Cast-Type (->* (CoC3-Expr CoC3-Expr) (CoC3-Expr) CoC3-Expr))
(define-type Interp-Make-Coercion-Type
  (CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Greatest-Lower-Bound-Type (CoC3-Expr CoC3-Expr -> CoC3-Expr))
(define-type Copy-Mref-Type (CoC3-Expr -> CoC3-Expr))
(define-type Make-Coercion-Type (Schml-Type Schml-Type Blame-Label -> CoC3-Expr))

(define (make-casting-helpers)
  : (Values Uid
            Interp-Cast-Type
            Interp-Cast-Type
            Make-Coercion-Type
            Interp-Make-Coercion-Type
            Interp-Compose-Coercions-Type
            Interp-Compose-Coercions-Type
            CoC3-Bnd-Code*)
  ;; Uid for runtime procedures
  (define interp-cast-uid (next-uid! "apply_hyper_coercion"))
  (define interp-med-cast-uid (next-uid! "apply_mediating_coercion"))
  (define interp-compose-coercions-uid (next-uid! "compose_coercions"))
  (define interp-compose-med-coercions-uid (next-uid! "compose_med_coercions"))
  (define interp-compose-fn-coercions-uid (next-uid! "compose-fn-coercions"))
  (define interp-compose-tuple-coercions-uid (next-uid! "compose-tuple-coercions"))
  (define interp-make-coercion-uid (next-uid! "make_hyper_coercion"))
  (define interp-make-med-coercion-uid (next-uid! "make_med_coercion"))
  (define greatest-lower-bound-uid (next-uid! "greatest_lower_bound"))
  (define copy-mref-uid (next-uid! "copy_mref_value"))
  ;; Generate calls to runtime procedures
  (define interp-compose-coercions : Interp-Compose-Coercions-Type
    (apply-code interp-compose-coercions-uid))
  (define interp-compose-fn-coercions
    : (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
    (apply-code interp-compose-fn-coercions-uid))
  (define interp-compose-tuple-coercions
    : (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
    (apply-code interp-compose-tuple-coercions-uid))
  (define interp-compose-med-coercions : Interp-Compose-Coercions-Type
    (apply-code interp-compose-med-coercions-uid))
  (define interp-make-coercion : Interp-Make-Coercion-Type
    (apply-code interp-make-coercion-uid))
  (define interp-make-med-coercion : Interp-Make-Coercion-Type
    (apply-code interp-make-med-coercion-uid))
  (define greatest-lower-bound : Greatest-Lower-Bound-Type
    (apply-code greatest-lower-bound-uid))
  (define copy-mref : Copy-Mref-Type
    (apply-code copy-mref-uid))
  (define Id (Quote-Coercion IDENTITY))
  (: interp-cast : Interp-Cast-Type)
  (define (interp-cast [e : CoC3-Expr][c : CoC3-Expr][m : CoC3-Expr (Quote 0)])
    ;; I am only using (Quote 0) because that was what the coercions
    ;; version of this pass used.
    (((inst apply-code CoC3-Expr) interp-cast-uid) e c m))
  (: interp-med-cast : Interp-Cast-Type)
  (define (interp-med-cast [e : CoC3-Expr][c : CoC3-Expr][m : CoC3-Expr (Quote 0)])
    ;; I am only using (Quote 0) because that was what the coercions
    ;; version of this pass used.
    (((inst apply-code CoC3-Expr) interp-med-cast-uid) e c m))
  ;; Genereate Runtime Code
  (: make-interp-cast-code : -> (Code Uid* CoC3-Expr))
  (define (make-interp-cast-code)
    (code$ (v c mt)
      (let$* ([v1 (cond$
                   [(HC-Project-Huh c)
                    (let$* ([t1 (Dyn-type v)]
                            [dv (Dyn-value v)]
                            [t2 (HC-T1 c)]
                            [l  (HC-Label c)])
                      (If (op=? t1 t2)
                          dv
                          ;; t1 != t2 -> < t1 =>^l t2> != Id
                          ;; therefore no need to make Id case fast
                          ;; was = (interp-cast dv (interp-make-coercion t1 t2 l))
                          (interp-med-cast dv (interp-make-med-coercion t1 t2 l))))]
                   [else v])]
              [m (HC-Med c)]
              [v2 (If (Id-Coercion-Huh m)
                      v1
                      (interp-med-cast v1 m mt))])
        (If (HC-Inject-Huh c)
            (If (and$ (Id-Coercion-Huh m) (HC-Project-Huh c))
                v
                (Dyn-make v2 (HC-T2 c)))
            v2))))
  (: make-interp-med-cast-code : -> (Code Uid* CoC3-Expr))
  (define (make-interp-med-cast-code) 
    (code$ (v m mt)
      (precondition$
       (not$ (Id-Coercion-Huh m))
       (cond$
        [(Mediating-Coercion-Huh m)
         (cond$
          [(Fn-Coercion-Huh m)
           ;; TODO!!! consider
           #; (If (Fn-Proxy-Huh v)
                  (App-Code (Fn-Proxy-Caster v) (list v m))
                  (Fn-Proxy v (Coercion m)))
           ;; ie move the Fn-Caster to Fn-Proxy objects only.
           ;; This has three benifits
           ;; there no longer needs to be a branch in the fn-casts code 
           ;; Closures no longer pay a space penalty
           ;; This code no longer has to do a double memory indirect
           (If (Fn-Proxy-Huh v)
               (App-Code (Fn-Caster (Fn-Proxy-Closure v)) (list v m))
               (App-Code (Fn-Caster v) (list v m)))]
          [(Tuple-Coercion-Huh m)
           (If (Op '= (list (Quote 0) mt))
               (Coerce-Tuple interp-cast-uid v m)
               (Coerce-Tuple-In-Place interp-cast-uid v m mt))]
          [(Ref-Coercion-Huh m)
           ;; There is a small amount of specialization here because
           ;; we know precisely which case of inter-compose-med will
           ;; match...
           (If (Guarded-Proxy-Huh v)
               (precondition$
                (Ref-Coercion-Huh (Guarded-Proxy-Coercion v))
                (let*$ ([old-v  (Guarded-Proxy-Ref v)]
                        [old-m  (Guarded-Proxy-Coercion v)]
                        [o-write (Ref-Coercion-Write old-m)]
                        [m-write (Ref-Coercion-Write m)]
                        [r-write (interp-compose-coercions m-write o-write)]
                        [o-read  (Ref-Coercion-Read old-m)]
                        [m-read  (Ref-Coercion-Read m)]
                        [r-read  (interp-compose-coercions o-read m-read)])
                  (If (and$ (HC-Identity-Huh r-read)
                            (HC-Identity-Huh r-write))
                      old-v
                      (Guarded-Proxy
                       old-v (Coercion (Ref-Coercion r-read r-write))))))
               (Guarded-Proxy v (Coercion m)))]
          [(MRef-Coercion-Huh m)
           (let*$ ([t2 (MRef-Coercion-Type m)])
             (cond$
              [(Type-Dyn-Huh t2) v]
              [else
               (let*$ ([t1 (Mbox-rtti-ref v)]
                       [t3 (greatest-lower-bound t1 t2)])
                 (cond$
                  [(op=? t1 t3) v]
                  [else
                   (Mbox-rtti-set! v t3)
                   (let*$ ([v-copy (copy-mref v)]
                           ;; This call should be replace with a specialize
                           ;; procedure for casting without creating this
                           ;; intermediate coercion. 
                           [c  (interp-make-coercion t1 t3 (Quote ""))]
                           [new-v (interp-cast v-copy c v)]
                           [t4 (Mbox-rtti-ref v)])
                     (cond$
                      [(op=? t3 t4) (Mbox-val-set! v new-v) v]
                      [else v]))]))]))]
          [(MVect-Coercion-Huh m)
           (let$* ([t2 (MVect-Coercion-Type m)])
             (cond$
              [(Type-Dyn-Huh t2) v]
              [else
               (let*$ ([t1 (Mvector-rtti-ref v)]
                       [t3 (greatest-lower-bound t1 t2)])
                 (cond$
                  [(op=? t1 t3) v]
                  [else
                   (Mvector-rtti-set! v t3)
                   ;; This blame label is fabricated from nothing because
                   ;; monotonic references are not completely implemented.
                   (let$ ([c   (interp-make-coercion t1 t3 (Quote ""))]
                          [len (Mvector-length v)])
                     (cond$
                      [(Type-Tuple-Huh t3)
                       (let$* ([n (Type-Tuple-num t3)])
                         (repeat$ (i (Quote 0) len) ()
                           (let*$ ([vi (Mvector-val-ref v i)]
                                   [cvi (Copy-Tuple n vi)])
                             (Mvector-val-set! v i cvi)
                             (let*$ ([ccvi (interp-cast cvi c v)]
                                     [t4 (Mvector-rtti-ref v)])
                               (if$ (op=? t3 t4)
                                    (Mvector-val-set! v i ccvi)
                                    (Break-Repeat))))))]
                      [else
                       (repeat$ (i (Quote 0) len) ()
                         (let*$ ([vi (Mvector-val-ref v i)]
                                 [cvi (interp-cast vi c v)]
                                 [t4 (Mvector-rtti-ref v)])
                           (If (op=? t3 t4)
                               (Mvector-val-set! v i cvi)
                               (Break-Repeat))))]))
                   v]))]))]
          [else (Blame (Quote "Internal Error: hyper-coercions/interp-med-cast 1"))])]
        ;; This isn't needed with the current implementation
        ;; but I want to include it because the name indicates that it should
        ;; work.
        [(Id-Coercion-Huh m) v]
        [(Failed-Coercion-Huh m) (Blame (Failed-Coercion-Label m))] 
        [else (Blame (Quote "Internal Error: hyper-coercions/interp-med-cast 2"))]))))
  ;; Cunstruct a hyper-coercions expression from the type and blame
  ;; information contained in a type-based cast.
  ;; This coercion literal is eligable to be hoisted in a later
  ;; pass so that it only ever is creates a value once. 
  (: make-coercion : Make-Coercion-Type)
  (define (make-coercion t1 t2 l)
    (: recur : Schml-Type Schml-Type -> Hyper-Coercion)
    (define (recur t1 t2)
      (match* (t1 t2)
        [(t        t) (HC #f t #f #f t IDENTITY)]
        ;; These two lines create an invarient that the only
        ;; time t1 or t2 is Dyn the entire hyper-coercion is an identity 
        [((Dyn)    t) (HC #t t l  #f t IDENTITY)]
        [(t    (Dyn)) (HC #f t #f #t t IDENTITY)]
        [((Fn n1 a1* r1) (Fn n2 a2* r2)) #:when (= n1 n2)
         ;; The arity check here means that all coercions have
         ;; the correct arity under the static type system.
         ;; Notice that the argument types are reversed
         ;; because of contravarience of functions.
         (HC #f t1 #f #f t2 (Fn n1 (map recur a2* a1*) (recur r1 r2)))]
        [((STuple n1 t1*) (STuple n2 t2*)) #:when (= n1 n2)
         (HC #f t1 #f #f t2 (CTuple n1 (map recur t1* t2*)))]
        [((GRef t1) (GRef t2))
         (HC #f t1 #f #f t2 (Ref (recur t1 t2) (recur t2 t1)))]
        [((GVect t1) (GVect t2))
         (HC #f t1 #f #f t2 (Ref (recur t1 t2) (recur t2 t1)))]
        [((MRef _) (MRef t2))
         (HC #f t1 #f #f t2 (MonoRef t2))]
        [((MVect _) (MVect t2))
         (HC #f t1 #f #f t2 (MonoVect t2))]
        [(t1 t2)
         (HC #f t1 #f #f t2 (Failed l))]))
    (Quote-HCoercion (recur t1 t2)))
  (: make-interp-make-coercion-code : -> (Code Uid* CoC3-Expr))
  (define (make-interp-make-coercion-code)
    (define-values (t1u t2u lu)
      (values (next-uid! "t1") (next-uid! "t2") (next-uid! "blame_info")))
    (define-values (t1 t2 l)
      (values (Var t1u) (Var t2u) (Var lu)))
    (define Id (Quote-Coercion IDENTITY))
    (Code `(,t1u ,t2u ,lu)
      (cond$
       [(op=? t1 t2)
        (ann (HC (Quote #f) t1 (Quote #f) (Quote #f) t1 Id) CoC3-Expr)]
       ;; This is absolutly necisarry
       ;; While Injections and Projections are never made by
       ;; source code coercions composition can create new
       ;; projections and injections.
       [(Type-Dyn-Huh t1)
        (ann (HC (Quote #t) t2 l (Quote #f) t2 Id) CoC3-Expr)]
       [(Type-Dyn-Huh t2)
        (ann (HC (Quote #f) t1 (Quote #f) (Quote #t) t1 Id) CoC3-Expr)]
       [else
        (let* ([med (interp-make-med-coercion t1 t2 l)]) 
          (ann (HC (Quote #f) t1 (Quote #f) (Quote #f) t2 med) CoC3-Expr))
        #;(cond$
           [(and$ (Type-Fn-Huh t1) (Type-Fn-Huh t2)
                  (op=? (Type-Fn-arity t1) (Type-Fn-arity t2)))
            ;; This line is a little tricky because unless we have actual
            ;; types for this at compile time we have to generate code that
            ;; can handle arbitry fn-arity.  We delegate this task to specify
            ;; representation because it involves safely allocating an object
            ;; whos size cannot be determined until run-time.
            (Make-Fn-Coercion interp-make-coercion-uid t1 t2 l)]
           [(and$ (Type-Tuple-Huh t1) (Type-Tuple-Huh t2)
                  (op<=? (tupleT-num$ t2) (tupleT-num$ t1)))
            (Make-Tuple-Coercion interp-make-coercion-uid t1 t2 l)]
           [(and$ (Type-GVect-Huh t1) (Type-GVect-Huh t2))
            (let$* ([gv1_of (Type-GVect-Of t1)]
                    [gv2_of (Type-GVect-Of t2)]
                    [write_crcn (interp-make-coercion gv2_of gv1_of l)]
                    [read_crcn  (interp-make-coercion gv1_of gv2_of l)])
              (Ref-Coercion read_crcn write_crcn))]
           [(and$ (Type-GRef-Huh t1) (Type-GRef-Huh t2))
            (let$* ([gr1_of (Type-GRef-Of t1)]
                    [gr2_of (Type-GRef-Of t2)]
                    [read_crcn  (interp-make-coercion gr1_of gr2_of l)]
                    [write_crcn (interp-make-coercion gr2_of gr1_of l)])
              (Ref-Coercion read_crcn write_crcn))]
           [(and$ (Type-MRef-Huh t1) (Type-MRef-Huh t2))
            (MRef-Coercion (mref-of$ t2))]
           [(and$ (Type-MRef-Huh t1) (Type-MRef-Huh t2))
            (MVect-Coercion (mvect-of$ t2))]
           [else (Failed-Coercion l)])])))
  (: make-interp-make-med-coercion-code : -> (Code Uid* CoC3-Expr))
  (define (make-interp-make-med-coercion-code)
    ;; precondition: t1 != Dyn /\ t2 != Dyn /\ t1 != t2
    ;; Should be called make_non_id_med_coercion
    (code$ (t1 t2 l)
      (precondition$ (not$ (and$ (op=? t1 t2) (Type-Dyn-Huh t1) (Type-Dyn-Huh t2)))
        (cond$
         [(and$ (Type-Fn-Huh t1) (Type-Fn-Huh t2)
                (op=? (Type-Fn-arity t1) (Type-Fn-arity t2)))
          ;; This line is a little tricky because unless we have actual
          ;; types for this at compile time we have to generate code that
          ;; can handle arbitry fn-arity.  We delegate this task to specify
          ;; representation because it involves safely allocating an object
          ;; whos size cannot be determined until run-time.
          (Make-Fn-Coercion interp-make-coercion-uid t1 t2 l)]
         [(and$ (Type-Tuple-Huh t1) (Type-Tuple-Huh t2)
                (op<=? (tupleT-num$ t2) (tupleT-num$ t1)))
          (Make-Tuple-Coercion interp-make-coercion-uid t1 t2 l)]
         [(and$ (Type-GVect-Huh t1) (Type-GVect-Huh t2))
          (let$* ([gv1_of (Type-GVect-Of t1)]
                  [gv2_of (Type-GVect-Of t2)]
                  [write_crcn (interp-make-coercion gv2_of gv1_of l)]
                  [read_crcn  (interp-make-coercion gv1_of gv2_of l)])
            (Ref-Coercion read_crcn write_crcn))]
         [(and$ (Type-GRef-Huh t1) (Type-GRef-Huh t2))
          (let$* ([gr1_of (Type-GRef-Of t1)]
                  [gr2_of (Type-GRef-Of t2)]
                  [write_crcn (interp-make-coercion gr2_of gr1_of l)]
                  [read_crcn  (interp-make-coercion gr1_of gr2_of l)])
            (Ref-Coercion read_crcn write_crcn))]
         ;; TODO should these two line be (glb t1 t2)? 
         [(and$ (Type-MRef-Huh t1) (Type-MRef-Huh t2))
          (MRef-Coercion (mref-of$ t2))]
         [(and$ (Type-MVect-Huh t1) (Type-MVect-Huh t2))
          (MVect-Coercion (mvect-of$ t2))]
         [else (Failed-Coercion l)]))))
  (: make-interp-compose-coercions : -> (Code Uid* CoC3-Expr))
  (define (make-interp-compose-coercions)
    (code$ (fst snd)
      (precondition$
       ;; all sequences of coercions to compose are well typed because:
       ;; either the inner types are well-typed or the coercions are
       ;; some combination injections projections and dynamic
       ;; identities that are well typed
       (or$ (op=? (HC-T2 fst) (HC-T1 snd))
            (and$ (or$ (Type-Dyn-Huh (HC-T2 fst)) (HC-Inject-Huh fst))
                  (or$ (Type-Dyn-Huh (HC-T1 snd)) (HC-Project-Huh snd))))
       (let$* ([fst-t2 (HC-T2 fst)]
               [snd-t1 (HC-T1 snd)])
         (cond$
          ;; These first two cases rule out dynamic identity casts
          ;; ie (HC #f Dyn #f #f Dyn ID)
          ;; These cannot be composed under the following rules because
          ;; the usual invarient of all code following is that Dyn
          ;; isn't present as t1 or t2 in any coercion.
          [(Type-Dyn-Huh fst-t2) snd]
          [(Type-Dyn-Huh snd-t1) fst]
          [else
           (let$* ([fst-med (HC-Med fst)]
                   [mid-med
                    (cond$
                     [(and$ (HC-Inject-Huh fst)
                            (HC-Project-Huh snd)
                            ;; interp-make-med-coercion assumes that fst-t2 != snd-t1
                            (not$ (op=? fst-t2 snd-t1)))
                      (let$* ([mid (interp-make-med-coercion
                                    fst-t2 snd-t1 (HC-Label snd))])
                        ;; we know these won't be Id
                        (interp-compose-med-coercions fst-med mid))]
                     [else fst-med])]
                   [snd-med (HC-Med snd)]
                   [fnl-med
                    ;; consider trying the id-checks here
                    #;(cond$
                       [(Id-Coercion-Huh fst-med) snd-med]
                       [(Id-Coercion-Huh snd-med) fst-med]
                       [else (interp-compose-med-coercions mid-med snd-med)])
                    (interp-compose-med-coercions mid-med snd-med)])
             (HC (HC-Project-Huh fst) (HC-T1 fst) (HC-Label fst) 
                 (HC-Inject-Huh snd) (HC-T2 snd)
                 fnl-med))])))))
  (: make-interp-compose-med-coercions : -> (Code Uid* CoC3-Expr))
  (define (make-interp-compose-med-coercions)
    (code$ (fst snd)
      (cond$
       ;; TODO consider specializing this code to make identity coercions faster
       ;; by moving the Id-coercion-huh calls to before each call to compose-med.
       [(Id-Coercion-Huh fst) snd]
       [(Id-Coercion-Huh snd) fst]
       [(and$ (Mediating-Coercion-Huh fst) (Mediating-Coercion-Huh snd))
        (cond$
         ;; Each of these should be able to ditch
         [(and$ (Fn-Coercion-Huh fst) (Fn-Coercion-Huh snd))
          (if #t
              (interp-compose-fn-coercions fst snd (Quote 0) (Fn-Coercion-Arity fst) (Quote #t))
              (Compose-Fn-Coercion interp-compose-coercions-uid fst snd))]
         [(and$ (Tuple-Coercion-Huh fst) (Tuple-Coercion-Huh snd))
          (if #t
              (interp-compose-tuple-coercions fst snd (Quote 0) (Tuple-Coercion-Num fst) (Quote #t))
              (Compose-Tuple-Coercion interp-compose-coercions-uid fst snd))]
         [(and$ (Ref-Coercion-Huh fst) (Ref-Coercion-Huh snd))
          (let*$ ([fst-write (Ref-Coercion-Write fst)]
                  [snd-write (Ref-Coercion-Write snd)]
                  [com-write (interp-compose-coercions snd-write fst-write)]
                  [fst-read  (Ref-Coercion-Read fst)]
                  [snd-read  (Ref-Coercion-Read snd)]
                  [com-read  (interp-compose-coercions fst-read snd-read)])
            (If (and$ (HC-Identity-Huh com-read)
                      (HC-Identity-Huh com-write))
                (Quote-Coercion IDENTITY)
                (Ref-Coercion com-read com-write)))]
         [(and$ (MRef-Coercion-Huh fst) (MRef-Coercion-Huh snd))
          (let*$ ([fst_type  (MRef-Coercion-Type fst)]
                  [snd_type  (MRef-Coercion-Type snd)]
                  [glb       (greatest-lower-bound fst_type snd_type)])
            (MRef-Coercion glb))]
         [(and$ (MVect-Coercion-Huh fst) (MVect-Coercion-Huh snd))
          (let*$ ([fst_type  (MVect-Coercion-Type fst)]
                  [snd_type  (MVect-Coercion-Type snd)]
                  [glb       (greatest-lower-bound fst_type snd_type)])
            (MVect-Coercion glb))]
         [else (Blame (Quote "Internal Error: compose-mediating-coercions 1"))])]
       [(Failed-Coercion-Huh fst)
        (If (Failed-Coercion-Huh snd)
            fst ;; merge blame info for bidirectional behavior
            fst)]
       [(Failed-Coercion-Huh snd) snd]
       [else (Blame (Quote "Internal Error: compose-mediating-coercion 2"))])))
  (: make-interp-compose-fn-coercions : -> (Code Uid* CoC3-Expr))
  (define (make-interp-compose-fn-coercions)
    ;; TODO Implement Fn-coercion-set! Make-Fn-Coercion
    ;; TODO make this iterative
    (code$ (c1 c2 i a was-id)
      (cond$
       [(Op '= `(,i ,a))
        (let*$ ([r1 (Fn-Coercion-Return c1)]
                [r2 (Fn-Coercion-Return c2)]
                [cr (interp-compose-coercions r1 r2)])
          (cond$
           [(and$ was-id (HC-Identity-Huh cr)) (Quote-Coercion (Identity))]
           [else 
            (let$ ([fnc (Id-Fn-Coercion a)])
              (Fn-Coercion-Return-Set! fnc cr)
              fnc)]))]
       [else
        (let*$ ([i2 (Fn-Coercion-Arg c2 i)]
                [i1 (Fn-Coercion-Arg c1 i)]
                [ca (interp-compose-coercions i2 i1)]
                [is-id (and$ was-id (HC-Identity-Huh ca))]
                [next-i (Op '+ `(,i ,(Quote 1)))]
                [m  (interp-compose-fn-coercions c1 c2 next-i a is-id)])
          (cond$
           [(Id-Coercion-Huh m) m]
           [else (Fn-Coercion-Arg-Set! m i ca) m]))])))
  (: make-interp-compose-tuple-coercions : -> (Code Uid* CoC3-Expr))
  (define (make-interp-compose-tuple-coercions)
    (code$ (c1 c2 i a was-id)
      (cond$
       [(Op '= `(,i ,a))
        (cond$
         [was-id (Quote-Coercion (Identity))]
         [else (Id-Tuple-Coercion a)])]
       [else
        (let*$ ([e1 (Tuple-Coercion-Item c1 i)]
                [e2 (Tuple-Coercion-Item c2 i)]
                [ce (interp-compose-coercions e1 e2)]
                [is-id (and$ was-id (HC-Identity-Huh ce))]
                [new-i (Op '+ `(,(Quote 1) ,i))]
                [m  (interp-compose-tuple-coercions c1 c2 new-i a is-id)])
          (cond
            [(Id-Coercion-Huh m) m]
            [else (Tuple-Coercion-Item-Set! m i ce)]))])))
  (define (make-greatest-lower-bound-code)
    (code$ (t1 t2)
      ((gen-greatest-lower-bound-type-code
        next-uid!
        greatest-lower-bound
        greatest-lower-bound-uid)
       t1 t2)))
  (define (make-copy-mref-code)
    (code$ (mref) ((gen-copy-value-in-monoref-code next-uid!) mref)))
  
  (values
   interp-cast-uid
   interp-cast
   interp-med-cast
   make-coercion
   interp-make-coercion
   interp-compose-coercions
   interp-compose-med-coercions
   `([,interp-cast-uid . ,(make-interp-cast-code)]
     [,interp-med-cast-uid . ,(make-interp-med-cast-code)]
     [,interp-make-coercion-uid . ,(make-interp-make-coercion-code)]
     [,interp-make-med-coercion-uid . ,(make-interp-make-med-coercion-code)]
     [,interp-compose-coercions-uid . ,(make-interp-compose-coercions)]
     [,interp-compose-med-coercions-uid . ,(make-interp-compose-med-coercions)]
     [,interp-compose-fn-coercions-uid . ,(make-interp-compose-fn-coercions)]
     [,interp-compose-tuple-coercions-uid . ,(make-interp-compose-tuple-coercions)]
     [,greatest-lower-bound-uid . ,(make-greatest-lower-bound-code)]
     [,copy-mref-uid . ,(make-copy-mref-code)]
     )))


(define (make-get-fn-cast!
         [fn-casts  : (HashTable Nat CoC3-Bnd-Code)]
         [name-base : String]
         [interp-cast-uid : Uid]
         [interp-compose : (CoC3-Expr CoC3-Expr -> CoC3-Expr)])
  : (Nat -> Uid)
  ;; build-caster! generates a piece of code that performs a fn-cast
  ;; for a specific arity. 

  ;; This could be done as 3 seperate casters one that handles both
  ;; casted and uncasted closures and another each seperately.
  ;; The general caster is used in static code
  ;; The uncasted is attatched to the original closure
  ;; The casted is attatched to the proxy
  (: build-caster! : Nat Uid -> (Code Uid* CoC3-Expr))
  (define (build-caster! arity name)
    ;; This function generates a series of if expressions
    ;; equivalent to an or of the list of input expressions. 
    (: or-help : (Listof CoC3-Expr) -> CoC3-Expr)
    (define (or-help a)
      (cond
        [(null? a) (Quote #t)]
        [(null? (cdr a)) (car a)]
        [else (If (car a) (or-help (cdr a)) (Quote #f))]))
    ;; This function generate a series of a list of checks
    ;; for identity coercions?
    (: id-c*? : Uid* -> CoC3-Expr*)
    (define (id-c*? x*)
      (map (lambda ([x : Uid]) (HC-Identity-Huh (Var x))) x*))
    ;; new fn-crcn generates the result if a new coercion is needed
    (: new-fn-crcn : Uid Uid Nat Uid* Uid -> CoC3-Expr)
    (define (new-fn-crcn t1 t2 arity args ret)
      (Fn-Coercion (map (inst Var Uid) args) (Var ret)))
    ;; compose-return generate a bindining that composes the return
    ;; of two hyper-coercions if present
    (: compose-return (Uid Uid Uid -> CoC3-Bnd))
    (define (compose-return ret new old)
      `[,ret . ,(interp-compose (Fn-Coercion-Return (Var old))
                                (Fn-Coercion-Return (Var new)))])
    ;; compose-arg generates a binding that composes the ith
    ;; fn-coercion of the two input coercions. 
    (: compose-arg (Uid Uid Uid Index -> CoC3-Bnd))
    (define (compose-arg arg new old i)
      `[,arg . ,(interp-compose (Fn-Coercion-Arg (Var new) (Quote i))
                                (Fn-Coercion-Arg (Var old) (Quote i)))])

    ;; the ids to build a new code
    (define u-clos (next-uid! "unknown_closure"))
    (define new-crcn (next-uid! "new_fn_coercion"))
    (define arg* (for/list : (Listof Uid) ([i (in-range arity)])
                   (next-uid! "arg_coercion")))
    ;; TODO remove this sanity check once we know the code works
    (unless (= (length arg*) arity)
      (error 'interp-casts-with-hc/build-caster/sanity-check))
    (define ret  (next-uid! "ret_coercion"))
    (define old-crcn (next-uid! "old_fn_coercion"))
    (define r-clos (next-uid! "raw_closure"))
    (define t1 (next-uid! "from_type"))
    (define t2 (next-uid! "to_type"))

    
    (define destructure-old-closure : CoC3-Bnd*
      `((,old-crcn  . ,(Fn-Proxy-Coercion (Var u-clos)))
        (,r-clos    . ,(Fn-Proxy-Closure  (Var u-clos)))))

    ;; generate all the compositions
    (define composed-coercions-bindings : CoC3-Bnd*
      (for/fold ([b* : CoC3-Bnd*
                     (list (compose-return ret new-crcn old-crcn))])
                ([a : Uid (reverse arg*)]
                 [i : Integer (in-range (- arity 1) -1 -1)])
        (unless (index? i)
          (error 'lower-function-casts "bad index made"))
        (cons (compose-arg a new-crcn old-crcn i) b*)))
    ;; an expression that checks if all composes resulted in ID
    (define all-result-in-id-huh : CoC3-Expr
      (or-help (id-c*? (cons ret arg*))))
    
    (unless (index? arity)
      (error 'lower-function-cast/build-fn-cast-with-coercion
             "arity grew too large to be a valid index"))

    ;; Body of build-fn-caster stitch together the AST of the
    ;; code that forms a fn-cast for a particular arity. 
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
                  (Fn-Proxy (list #{arity :: Index} interp-cast-uid)
                            (Var r-clos)
                            (new-fn-crcn t1 t2 arity arg* ret)))))
          ;; Closure is unproxied --> just make a new proxy
          (Fn-Proxy (list #{arity :: Index} interp-cast-uid)
                    (Var u-clos) (Var new-crcn)))))
  
  ;; Body of make-get-fn-cast! returns a procedure that generates/looksup
  ;; the runtime code to perform fn-casts.
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





;; Functions for use sites of guarded references with coercions
(define-type PBox-Ref-Type ((Var Uid) -> CoC3-Expr))
(define-type PBox-Set-Type ((Var Uid) (Var Uid) -> CoC3-Expr))
(define-type PVec-Ref-Type ((Var Uid) (Var Uid) -> CoC3-Expr))
(define-type PVec-Set-Type ((Var Uid) (Var Uid) (Var Uid) -> CoC3-Expr))
(define-type PVec-Len-Type ((Var Uid) -> CoC3-Expr))
;; This is abstracted for two reasons
;; 1) There isn't a good reason to check the inlining-proxied-reference-branch
;;    more than once.
;; 2) If it isn't inlined then the code needs to be registered the runtime
;;    procedures which can be done from the mapping context, but it is
;;    easier from the top-level. 
(define (make-proxied-reference-helpers! [interp-cast : Interp-Cast-Type])
  : (Values PBox-Ref-Type PBox-Set-Type PVec-Ref-Type PVec-Set-Type
            PVec-Len-Type CoC3-Bnd-Code*)
  (: make-gbox-ref-code PBox-Ref-Type)
  (define (make-gbox-ref-code gref)
    (If (Guarded-Proxy-Huh gref)
        (interp-cast
         (Unguarded-Box-Ref (Guarded-Proxy-Ref gref))
         (Ref-Coercion-Read (Guarded-Proxy-Coercion gref)))
        (Unguarded-Box-Ref gref)))
  (: make-gbox-set!-code  PBox-Set-Type)
  (define (make-gbox-set!-code gref val)
    (If (Guarded-Proxy-Huh gref)
        (Unguarded-Box-Set!
         (Guarded-Proxy-Ref gref)
         (interp-cast val (Ref-Coercion-Write (Guarded-Proxy-Coercion gref))))
        (Unguarded-Box-Set! gref val)))
  (: make-gvect-ref-code  PVec-Ref-Type)
  (define (make-gvect-ref-code gref index)
    (If (Guarded-Proxy-Huh gref)
        (interp-cast (Unguarded-Vect-Ref (Guarded-Proxy-Ref gref) index)
                     (Ref-Coercion-Read (Guarded-Proxy-Coercion gref)))
        (Unguarded-Vect-Ref gref index)))
  (: make-gvect-length-code PVec-Len-Type)
  (define (make-gvect-length-code gvect)
    (If (Guarded-Proxy-Huh gvect)
        (Unguarded-Vect-length (Guarded-Proxy-Ref gvect))
        (Unguarded-Vect-length gvect)))
  (: make-gvect-set!-code PVec-Set-Type)
  (define (make-gvect-set!-code gref index val)
    (: index-exp CoC3-Expr)
    (define index-exp (if (integer? index) (Quote index) index))
    (If (Guarded-Proxy-Huh gref)
        (Unguarded-Vect-Set!
         (Guarded-Proxy-Ref gref)
         index-exp
         (interp-cast val (Ref-Coercion-Write (Guarded-Proxy-Coercion gref))))
        (Unguarded-Vect-Set! gref index-exp val)))
  (cond
    [(inline-guarded-branch?)
     ;; we just hand back the code generators to build
     ;; inline code everywhere.
     (values make-gbox-ref-code
             make-gbox-set!-code
             make-gvect-ref-code
             make-gvect-set!-code
             make-gvect-length-code
             '())]
        [else
         ;; If they are not inlined then the compiler we generate
         ;; the runtime binding and returns procedures that builds
         ;; invokations of this runtime code.
         (let ([gbr   (next-uid! "rt_gbox_ref")]
               [gbr-b (next-uid! "box")]
               [gbs   (next-uid! "rt_gbox_set")]
               [gbs-b (next-uid! "box")]
               [gbs-v (next-uid! "write_val")]
               [gvr   (next-uid! "rt_gvec_ref")]
               [gvr-r (next-uid! "vec")]
               [gvr-i (next-uid! "ind")]
               [gvs   (next-uid! "rt_gvec_set")]
               [gvs-r (next-uid! "vec")]
               [gvs-i (next-uid! "ind")]
               [gvs-v (next-uid! "val")]
               [gvl   (next-uid! "rt_gvec_len")]
               [gvl-r (next-uid! "vec")])
           (values
            (apply-code gbr)
            (apply-code gbs)
            (apply-code gvr)
            (apply-code gvs)
            (apply-code gvl)
            `([,gbr . ,(Code (list gbr-b)
                             (make-gbox-ref-code (Var gbr-b)))]
              [,gbs . ,(Code (list gbs-b gbs-v)
                             (make-gbox-set!-code (Var gbs-b) (Var gbs-v)))]
              [,gvr . ,(Code (list gvr-r gvr-i)
                             (make-gvect-ref-code (Var gvr-r) (Var gvr-i)))]
              [,gvs . ,(Code (list gvs-r gvs-i gvs-v)
                             (make-gvect-set!-code (Var gvs-r) (Var gvs-i)
                                        (Var gvs-v)))]
              [,gvl . ,(Code (list gvl-r)
                         (make-gvect-length-code (Var gvl-r)))])))]))


(define-type MBox-Ref-Type ((Var Uid) (Var Uid) -> CoC3-Expr))
(define-type MBox-Set-Type ((Var Uid) (Var Uid) (Var Uid) -> CoC3-Expr))
(define-type MVec-Ref-Type ((Var Uid) (Var Uid) (Var Uid) -> CoC3-Expr))
(define-type MVec-Set-Type ((Var Uid) (Var Uid) (Var Uid) (Var Uid) -> CoC3-Expr))

(define (make-mono-reference-helpers! [interp-cast : Interp-Cast-Type]
                                      [interp-make-coercion : Interp-Make-Coercion-Type])
  : (Values MBox-Ref-Type MBox-Set-Type MVec-Ref-Type MVec-Set-Type
            CoC3-Bnd-Code*)
  (: make-mbox-ref-code MBox-Ref-Type)
  (define (make-mbox-ref-code mref t2)
    (let ([t1 (next-uid! "t1")]
        [c  (next-uid! "crcn")])
    (Let (list (cons t1 (Mbox-rtti-ref mref)))
      (Let (list (cons c (interp-make-coercion (Var t1) t2 (Quote ""))))
        (interp-cast (Mbox-val-ref mref) (Var c) (Quote 0))))))
  (: make-mbox-set!-code  MBox-Set-Type)
  (define (make-mbox-set!-code mref val t1)
    (define-syntax-let$* let$* next-uid!)
    (let$* ([t2 (Mbox-rtti-ref mref)]
            [c (interp-make-coercion t1 t2 (Quote ""))]
            [cv (cond$
                 [(and$ (tupleT?$ t1) (tupleT?$ t2))
                  (let$* ([n (Type-Tuple-num t2)]
                          [ctv (Copy-Tuple n val)])
                    (Begin
                      (list
                       (Mbox-val-set! mref ctv))
                      ctv))]
                 [else val])]
            [ccv (interp-cast cv c mref)]
            [t2-new (Mbox-rtti-ref mref)])
      (begin
        (if$ (op=? t2 t2-new)
             (Mbox-val-set! mref ccv)
             (Quote 0)))))
  (: make-mvect-ref-code  MVec-Ref-Type)
  (define (make-mvect-ref-code mvect i t2)
    (let ([t1 (next-uid! "t1")]
          [c  (next-uid! "crcn")])
      (Let (list (cons t1 (Mvector-rtti-ref mvect)))
        (Let (list (cons c (interp-make-coercion (Var t1) t2 (Quote ""))))
          (interp-cast (Mvector-val-ref mvect i) (Var c) (Quote 0))))))
  (: make-mvect-set!-code MVec-Set-Type)
  (define (make-mvect-set!-code mvect i val t1)
    (define-syntax-let$* let$* next-uid!)
    (let$* ([t2 (Mvector-rtti-ref mvect)]
            [c (interp-make-coercion t1 t2 (Quote ""))])
      (cond$
       [(and$ (tupleT?$ t1) (tupleT?$ t2))
        (let$* ([n (Type-Tuple-num t2)]
                [cvi (Copy-Tuple n val)])
          (Begin
            (list
             (Mvector-val-set! mvect i cvi))
            (let$* ([ccvi (interp-cast cvi c mvect)]
                    [t2-new (Mvector-rtti-ref mvect)])
              (if$ (op=? t2 t2-new)
                   (Mvector-val-set! mvect i ccvi)
                   (Quote 0)))))]
       [else
        (let$* ([cvi (interp-cast val c mvect)]
                [t2-new (Mvector-rtti-ref mvect)])
          (if$ (op=? t2 t2-new)
               (Mvector-val-set! mvect i cvi)
               (Quote 0)))])))
  (cond
    [(inline-guarded-branch?)
     (values make-mbox-ref-code
             make-mbox-set!-code
             make-mvect-ref-code
             make-mvect-set!-code
             '())]
        [else
         ;; If they are not inlined then the compiler we generate
         ;; the runtime binding and returns procedures that builds
         ;; invokations of this runtime code.
         (let ([mbr   (next-uid! "rt_mbox_ref")]
               [mbr-b (next-uid! "box")]
               [mbr-rt (next-uid! "ref_type")]
               [mbs   (next-uid! "rt_mbox_set")]
               [mbs-b (next-uid! "box")]
               [mbs-v (next-uid! "write_val")]
               [mbs-rt (next-uid! "ref_type")]
               [mvr   (next-uid! "rt_mvec_ref")]
               [mvr-r (next-uid! "vec")]
               [mvr-i (next-uid! "ind")]
               [mvr-rt (next-uid! "ref_type")]
               [mvs   (next-uid! "rt_mvec_set")]
               [mvs-r (next-uid! "vec")]
               [mvs-i (next-uid! "ind")]
               [mvs-v (next-uid! "val")]
               [mvs-rt (next-uid! "ref_type")])
           (values
            (apply-code mbr) (apply-code mbs)
            (apply-code mvr) (apply-code mvs)
            `([,mbr
               . ,(Code (list mbr-b mbr-rt) (make-mbox-ref-code (Var mbr-b) (Var mbr-rt)))]
              [,mbs
               . ,(Code (list mbs-b mbs-v mbs-rt)
                    (make-mbox-set!-code (Var mbs-b) (Var mbs-v) (Var mbs-rt)))]
              [,mvr
               . ,(Code (list mvr-r mvr-i mvr-rt)
                    (make-mvect-ref-code (Var mvr-r) (Var mvr-i) (Var mvr-rt)))]
              [,mvs
               . ,(Code (list mvs-r mvs-i mvs-v mvs-rt)
                    (make-mvect-set!-code (Var mvs-r) (Var mvs-i)
                                        (Var mvs-v) (Var mvs-rt)))])))]))

(define-type Dyn-PBox-Ref-Type ((Var Uid) (Var Uid) -> CoC3-Expr))
(define-type Dyn-PBox-Set-Type
  ((Var Uid) (Var Uid) (U (Type Schml-Type) (Var Uid)) (Var Uid) -> CoC3-Expr))
(define-type Dyn-PVec-Ref-Type
  ((Var Uid) (Var Uid) (Var Uid) -> CoC3-Expr))
(define-type Dyn-PVec-Set-Type
  ((Var Uid) (Var Uid) (Var Uid) (U (Type Schml-Type) (Var Uid)) (Var Uid)
   -> CoC3-Expr))
(define-type Dyn-MBox-Ref-Type ((Var Uid) (Var Uid) -> CoC3-Expr))
(define-type Dyn-MBox-Set-Type
  ((Var Uid) (Var Uid) (U (Type Schml-Type) (Var Uid)) (Var Uid) -> CoC3-Expr))
(define-type Dyn-MVec-Ref-Type
  ((Var Uid) (Var Uid) (Var Uid) -> CoC3-Expr))
(define-type Dyn-MVec-Set-Type
  ((Var Uid) (Var Uid) (Var Uid) (U (Type Schml-Type) (Var Uid)) (Var Uid)
   -> CoC3-Expr))
(define-type Dyn-PVec-Len-Type ((Var Uid)  (Var Uid) -> CoC3-Expr))
(define-type Dyn-Fn-App-Type
  ((Var Uid) (Listof (Var Uid)) Schml-Type* (Var Uid) -> CoC3-Expr))
(define-type Dyn-Tup-Prj-Type ((Var Uid) (Var Uid) (Var Uid) -> CoC3-Expr))

;; Creates a set of functions that given the set of arguments to a dynamic
;; operation generates an expression that handles that operation.
;; Also returns a possibly empty set of binding for runtime functions
;; necisarry to make the generated expressions work.
(define (make-dynamic-operations-helpers!
         [interp-cast-uid : Uid]
         [interp-cast : Interp-Cast-Type]
         [interp-make-coercion : Interp-Make-Coercion-Type]
         [pbox-ref  : PBox-Ref-Type]
         [pbox-set! : PBox-Set-Type]
         [pvec-ref  : PVec-Ref-Type]
         [pvec-set! : PVec-Set-Type]
         [pvec-len  : PVec-Len-Type]
         [mbox-ref  : MBox-Ref-Type]
         [mbox-set! : MBox-Set-Type]
         [mvec-ref  : MVec-Ref-Type]
         [mvec-set! : MVec-Set-Type])
  : (Values Dyn-PBox-Ref-Type Dyn-PBox-Set-Type
            Dyn-PVec-Ref-Type Dyn-PVec-Set-Type Dyn-PVec-Len-Type
            Dyn-MBox-Ref-Type Dyn-MBox-Set-Type
            Dyn-MVec-Ref-Type Dyn-MVec-Set-Type
            Dyn-Fn-App-Type Dyn-Tup-Prj-Type
            CoC3-Bnd-Code*)
  ;; smart-cast generates calls to interp-cast only when it is non-obvious
  ;; what coercions is going to be the result of the cast.
  ;; This tends to be the common case in Dynamic operations.
  ;; Viewed from another light smart-casts encodes some, but not all of
  ;; the behavior of casting with coercions into the compiler. 
  (define dyn-type (Type DYN-TYPE))
  (define (smart-cast [val : CoC3-Expr]
                      [t1 : CoC3-Expr] [t2 : CoC3-Expr] [lbl : CoC3-Expr]
                      [mono_type : CoC3-Expr]
                      #:t1-not-dyn [t1-not-dyn : Boolean #f]
                      #:t2-not-dyn [t2-not-dyn : Boolean #f])
    (match* (val t1 t2)
      [(val (Type t) (Type t)) val]
      [(val (Type (Dyn)) t2) #:when t2-not-dyn
       (interp-cast val (HC (Quote #t) t2 lbl (Quote #f) t2
                            (Quote-Coercion IDENTITY))
                    mono_type)]
      [((or (Var _) (Quote _)) (Type (Dyn)) t2)
       (If (Type-Dyn-Huh t2)
           val
           (interp-cast val (HC (Quote #t) t2 (Quote #f) (Quote #f) t2
                                (Quote-Coercion IDENTITY))
                        mono_type))]
      [(val t1 (Type (Dyn))) #:when t1-not-dyn 
       (interp-cast val (HC (Quote #t) t2 lbl (Quote #f) t2
                            (Quote-Coercion IDENTITY))
                    mono_type)]
      [((Var _) t1 (Type (Dyn)))
       (If (Type-Dyn-Huh t1)
           val
           (interp-cast val (HC (Quote #f) t1 (Quote #f) (Quote #t) t1
                                (Quote-Coercion IDENTITY))
                        mono_type))]
      [(val t1 t2)
       (interp-cast val (interp-make-coercion t1 t2 lbl) mono_type)]))
  (: make-dyn-pbox-ref-code Dyn-PBox-Ref-Type)
  (define (make-dyn-pbox-ref-code dyn lbl)
    (define-values (val ty tyof read-val)
      (values (next-uid! "dyn_unbox_val")
              (next-uid! "dyn_unbox_ty")
              (next-uid! "dyn_unbox_tyof")
              (next-uid! "dyn_unbox_read_val")))
    (define-values (var-val var-ty var-tyof var-read-val)
      (values (Var val) (Var ty) (Var tyof) (Var read-val)))
    (Let `((,val . ,(Dyn-value dyn))
           (,ty . ,(Dyn-type dyn)))
         (If (Type-GRef-Huh var-ty)
             (Let `([,tyof . ,(Type-GRef-Of var-ty)]
                    [,read-val . ,(pbox-ref var-val)])
                  (smart-cast var-read-val var-tyof (Type DYN-TYPE) lbl (Quote 0)))
             (Blame lbl))))
  
  (: make-dyn-pbox-set!-code Dyn-PBox-Set-Type)
  (define (make-dyn-pbox-set!-code dyn-gbox wrt-val1 t2 lbl)
    (let$ ([gbox (Dyn-value dyn-gbox)]
           [ty  (Dyn-type dyn-gbox)])
      (If (Type-GRef-Huh ty)
          (let$ ([tyof (Type-GRef-Of ty)])
            (cond$
             [(op=? tyof t2)
              (pbox-set! gbox wrt-val1)]
             [else
              (let$ ([wrt-val2 (smart-cast wrt-val1 t2 tyof lbl (Quote 0))])
                (pbox-set! gbox wrt-val2))])
            (smart-cast (Quote '()) (Type UNIT-TYPE) dyn-type lbl (Quote 0)))
          (Blame lbl))))
  (: make-dyn-pvec-ref-code Dyn-PVec-Ref-Type)
  (define (make-dyn-pvec-ref-code dyn ind lbl)
    (let$ ([maybe-pvec-val (Dyn-value dyn)]
           [maybe-pvec-ty  (Dyn-type dyn)])
      (cond$
       [(not$ (Type-GVect-Huh maybe-pvec-ty))  (Blame lbl)]
       [else
        (let$ ([elem-ty (Type-GVect-Of maybe-pvec-ty)]
               [elem-val (pvec-ref maybe-pvec-val ind)])
          (smart-cast elem-val elem-ty dyn-type lbl (Quote 0)))])))
  (: make-dyn-pvec-set!-code Dyn-PVec-Set-Type)
  (define (make-dyn-pvec-set!-code dyn-gvec ind wrt-val1 t2 lbl) 
    (let$ ([maybe-vec      (Dyn-value dyn-gvec)]
           [maybe-vec-type (Dyn-type  dyn-gvec)])
      (cond$
       [(not$ (Type-GVect-Huh maybe-vec-type)) (Blame lbl)]
       [else
        (let*$ ([elem-type (Type-GVect-Of maybe-vec-type)]
                [new-elem (If (op=? elem-type t2)
                              wrt-val1
                              (smart-cast wrt-val1 t2 elem-type lbl (Quote 0)))])
          (pvec-set! maybe-vec ind new-elem)
          (smart-cast (Quote '()) (Type UNIT-TYPE) dyn-type lbl (Quote 0)))])))
  #| TODO delete this once this code passes the test-suite 
  (let$ [ (Type-GVect-Huh ty-v)
  (Let `([,tyof . ,(Type-GVect-Of ty-v)])
  (If (Type-Dyn-Huh tyof-v)
  (Let `([,wrt-val2 . ,(smart-cast wrt-val1 t2 dyn-type lbl)])
  (pvec-set! val-v ind wrt-val2-v))
  (Let `([,wrt-val3
  . ,(smart-cast wrt-val1 t2 tyof-v lbl #:t2-not-dyn #t)])
  (smart-cast (pvec-set! val-v ind wrt-val3-v) 
  tyof-v (Type DYN-TYPE) lbl
  #:t1-not-dyn #t))))
  (Blame lbl))))
  |#
  (: make-dyn-pvec-len-code Dyn-PVec-Len-Type)
  (define (make-dyn-pvec-len-code dyn lbl)
    (define-values (val ty)
      (values (next-uid! "dyn_pvec_ref_val")
              (next-uid! "dyn_pvec_ref_ty")))
    (define-values (var-val var-ty) (values (Var val) (Var ty)))
    (Let `((,val . ,(Dyn-value dyn))
           (,ty . ,(Dyn-type dyn)))
         (If (Type-GVect-Huh var-ty)
             (pvec-len var-val)
             (Blame lbl))))

  (: make-dyn-mbox-ref-code Dyn-MBox-Ref-Type)
  (define (make-dyn-mbox-ref-code dyn lbl)
    (define-values (val ty dynty)
      (values (next-uid! "dyn_unbox_val")
              (next-uid! "dyn_unbox_ty")
              (next-uid! "dyn_type")))
    (define-values (var-val var-ty var-dynty)
      (values (Var val) (Var ty) (Var dynty)))
    (Let `((,val . ,(Dyn-value dyn))
           (,ty . ,(Dyn-type dyn)))
      (If (Type-MRef-Huh var-ty)
          (Let `([,dynty . ,(Type DYN-TYPE)])
            (mbox-ref var-val var-dynty))
          (Blame lbl))))
  
  (: make-dyn-mbox-set!-code Dyn-MBox-Set-Type)
  (define (make-dyn-mbox-set!-code dyn-mbox wrt-val1 t2 lbl)
    (define-values (mbox ty tyof t2u)
      (values (next-uid! "dyn_setbox_mbox")
              (next-uid! "dyn_setbox_ty")
              (next-uid! "dyn_setbox_tyof")
              (next-uid! "t2_type")))
    (define-values (mbox-v ty-v tyof-v t2u-v)
      (values (Var mbox) (Var ty) (Var tyof) (Var t2u)))
    (Let `([,mbox . ,(Dyn-value dyn-mbox)]
           [,ty   . ,(Dyn-type dyn-mbox)])
      (If (Type-MRef-Huh ty-v)
          (Let `([,tyof . ,(Type-MRef-Of ty-v)]
                 [,t2u .  ,t2])
            (If (Type-Dyn-Huh tyof-v)
                (smart-cast (mbox-set! mbox-v wrt-val1 t2u-v)
                       (Type UNIT-TYPE) (Type DYN-TYPE) lbl
                       (Quote 0))
                (mbox-set! mbox-v wrt-val1 t2u-v)))
          (Blame lbl))))
  (: make-dyn-mvec-ref-code Dyn-MVec-Ref-Type)
  (define (make-dyn-mvec-ref-code dyn ind lbl)
    (define-values (val ty dynty)
      (values (next-uid! "dyn_mvec_ref_val")
              (next-uid! "dyn_mvec_ref_ty")
              (next-uid! "dyn_type")))
    (define-values (var-val var-ty var-dynty)
      (values (Var val) (Var ty) (Var dynty)))
    (Let `((,val . ,(Dyn-value dyn))
           (,ty . ,(Dyn-type dyn)))
      (If (Type-MVect-Huh var-ty)
          (Let `([,dynty . ,(Type DYN-TYPE)])
            (mvec-ref var-val ind var-dynty))
          (Blame lbl))))
  (: make-dyn-mvec-set!-code Dyn-MVec-Set-Type)
  (define (make-dyn-mvec-set!-code dyn-mvec ind wrt-val1 t2 lbl) 
    (define-values (val ty tyof t2u)
      (values (next-uid! "dyn_setbox_mvect_value")
              (next-uid! "dyn_setbox_ty")
              (next-uid! "dyn_setbox_tyof")
              (next-uid! "t2_type")))
    (define-values (val-v ty-v tyof-v t2u-v)
      (values (Var val) (Var ty) (Var tyof) (Var t2u)))
    (Let `((,val . ,(Dyn-value dyn-mvec))
           (,ty . ,(Dyn-type dyn-mvec)))
      (If (Type-MVect-Huh ty-v)
          (Let `([,tyof . ,(Type-MVect-Of ty-v)]
                 [,t2u .  ,t2])
            (If (Type-Dyn-Huh tyof-v)
                (smart-cast (mvec-set! val-v ind wrt-val1 t2u-v)
                       (Type UNIT-TYPE) (Type DYN-TYPE) lbl (Quote 0))
                (mvec-set! val-v ind wrt-val1 t2u-v)))
          (Blame lbl))))
  
  (: make-dyn-fn-app-code Dyn-Fn-App-Type)
  (define (make-dyn-fn-app-code v v* t* l)
    (define-values (val ty ret-val ret-ty)
      (values (next-uid! "dyn_fn_val")
              (next-uid! "dyn_fn_ty")
              (next-uid! "dyn_fn_ret_val")
              (next-uid! "dyn_fn_ret_ty")))
    (define arg-casts : CoC3-Expr*
      (for/list : (Listof CoC3-Expr)
                ([v : CoC3-Expr v*]
                 [t : Schml-Type t*]
                 [i (in-naturals)])
        (let$ ([dyn-fn-arg-type (Type-Fn-arg (Var ty) (Quote i))])
          (smart-cast v (Type t) dyn-fn-arg-type l (Quote 0)))))
    (define casts-apply : CoC3-Expr
      (case (function-cast-representation)
        [(Hybrid) (App-Fn-or-Proxy interp-cast-uid (Var val) arg-casts)]
        [(Data) (error 'todo "implement coercions data representation")]
        [(Functional) (error 'todo "incompatible with coercions")]
      [else (error 'interp-cast-with-coercions/dyn-fn-app "unexpected value")]))
    (Let `([,val . ,(Dyn-value v)]
           [,ty . ,(Dyn-type v)])
     (If (Type-Fn-Huh (Var ty))
         (Let `([,ret-val . ,casts-apply]
                [,ret-ty . ,(Type-Fn-return (Var ty))])
           (smart-cast (Var ret-val) (Var ret-ty) (Type DYN-TYPE) l (Quote 0)))
         (Blame l))))
  (: make-dyn-tup-prj-code Dyn-Tup-Prj-Type)
  (define (make-dyn-tup-prj-code v i l)
    (define-values (val ty prj-val prj-ty)
      (values (next-uid! "dyn_tup_val")
              (next-uid! "dyn_tup_ty")
              (next-uid! "dyn_tup_prj_val")
              (next-uid! "dyn_tup_prj_ty")))
    (let$ ([val (Dyn-value v)]
           [ty  (ann (Dyn-type v) CoC3-Expr)])
          (cond$
           [(ann (or$ (not$ (Type-Tuple-Huh ty))
                      (not$ (Op '> (list (Type-Tuple-num ty) i))))
                 CoC3-Expr)
            (Blame l)]
           [else
            (ann (let$ ([prj-val (Tuple-proj val i)]
                        [prj-ty  (Type-Tuple-item ty i)])
                   (smart-cast prj-val prj-ty (Type DYN-TYPE) l (Quote 0)))
                 CoC3-Expr)])))
  ;; Body of Make dynamic operations helpers!
  (case (dynamic-operations?)
    ;; In the case that dynamic operations specialization isn't
    ;; enabled we should raise an error if any of the helpers
    ;; are ever invoked.
    [(#f)
     (define ((th-error [sym : Symbol]) . a)
       (error sym "dynamic-operation? = #f but present in AST"))
     (values
      (th-error 'interpret-cast-with-coercions/dyn-gbox-ref)
      (th-error 'interpret-cast-with-coercions/dyn-gbox-set!)
      (th-error 'interpret-cast-with-coercions/dyn-gvec-ref)
      (th-error 'interpret-cast-with-coercions/dyn-gvec-set!)
      (th-error 'interpret-cast-with-coercions/dyn-gvec-len)
      (th-error 'interpret-cast-with-coercions/dyn-mbox-ref)
      (th-error 'interpret-cast-with-coercions/dyn-mbox-set!)
      (th-error 'interpret-cast-with-coercions/dyn-mvec-ref)
      (th-error 'interpret-cast-with-coercions/dyn-mvec-set!)
      (th-error 'interpret-cast-with-coercions/dyn-fn-app)
      (th-error 'interpret-cast-with-hyper-coercions/dyn-tup-prj)
      '())]
    ;; In the case that dynamic operations are inlined we should
    ;; generate the code in-place and therefore do not need any
    ;; runtime support for dynamic-operations.
    ;; no code bindings returned.
    [(inline)
     (values
      make-dyn-pbox-ref-code
      make-dyn-pbox-set!-code
      make-dyn-pvec-ref-code
      make-dyn-pvec-set!-code
      make-dyn-pvec-len-code
      make-dyn-mbox-ref-code
      make-dyn-mbox-set!-code
      make-dyn-mvec-ref-code
      make-dyn-mvec-set!-code
      make-dyn-fn-app-code
      make-dyn-tup-prj-code
      '())]
    [else
     (define dpbr   (next-uid! "rt_dyn_pbox_ref"))
     (define dpbs   (next-uid! "rt_dyn_pbox_set"))
     (define dpvr   (next-uid! "rt_dyn_pvec_ref"))
     (define dpvs   (next-uid! "rt_dyn_pvec_set"))
     (define dpvl   (next-uid! "rt_dyn_pvec_len"))
     (define dmbr   (next-uid! "rt_dyn_mbox_ref"))
     (define dmbs   (next-uid! "rt_dyn_mbox_set"))
     (define dmvr   (next-uid! "rt_dyn_mvec_ref"))
     (define dmvs   (next-uid! "rt_dyn_mvec_set"))
     (define dtp   (next-uid! "rt_dyn_tuple_project"))
     (values 
      (apply-code dpbr)
      (apply-code dpbs)
      (apply-code dpvr)
      (apply-code dpvs)
      (apply-code dpvl)
      (apply-code dmbr)
      (apply-code dmbs)
      (apply-code dmvr)
      (apply-code dmvs)
      ;; Since the code for dynamic function application is arity
      ;; dependent we are inlining the code.
      make-dyn-fn-app-code 
      (apply-code dtp)
      `([,dpbr . ,(code$ (ref lbl)
                    (make-dyn-pbox-ref-code ref lbl))]
        [,dpbs . ,(code$ (ref val ty lbl)
                    (make-dyn-pbox-set!-code ref val ty lbl))]
        [,dpvr . ,(code$ (vec ind lbl)
                    (make-dyn-pvec-ref-code vec ind lbl))]
        [,dpvs . ,(code$ (vec ind val ty lbl)
                    (make-dyn-pvec-set!-code vec ind val ty lbl))]
        [,dpvl . ,(code$ (vec lbl) (make-dyn-pvec-len-code vec lbl))]
        [,dmbr . ,(code$ (ref lbl)
                    (make-dyn-mbox-ref-code ref lbl))]
        [,dmbs . ,(code$ (ref val ty lbl)
                    (make-dyn-mbox-set!-code ref val ty lbl))]
        [,dmvr . ,(code$ (vec ind lbl)
                    (make-dyn-mvec-ref-code vec ind lbl))]
        [,dmvs . ,(code$ (vec ind val ty lbl)
                    (make-dyn-mvec-set!-code vec ind val ty lbl))]
        [,dtp . ,(code$ (tup ind lbl)
                   (make-dyn-tup-prj-code tup ind lbl))]))]))
