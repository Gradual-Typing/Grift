#lang typed/racket

;; This pass creates an ADT for boxes that will be followed until instruction selection.
;; This is a Specification for some of the forms in my Ast and the ways in wich they
;; may be used. As of yet the Asts are not actually type checked

#| Guarded Itermediate Representation ADT
;; Forgive the dependant type notation
;; {} denote implicit information that

Type (GRep A) where
  ;; Unguarded Boxes
  UGbox  A : {A : Set} -> A -> GRep A
  ;;xies for Guarded Representations
  Gproxy A : GRep B -> (B : Type) -> (A : Type) -> Blame-Label -> GRep A

;; The only two things that you can do with an GRep A are proxy it
;; and look to see if it is proxied.
GRep-proxied? : GRep A -> Bool

;; Once you are able to tell whether a GRep is Unguarded
UGbox-read (x : GRep A) -> {not (GRep-proxied? x)} -> A
UGbox-write (x : GRep A) -> {not (GRep-proxied? x)} -> A -> Unit

;; Or if you can tell the a GRep is a Proxy
Gproxy-for     : (x : GRep A) -> {GRep-proxied? x} -> GRep B
Gproxy-from    : (x : GRep A) -> {GRep-proxied? x} -> (: B Type)
Gproxy-to      : (x : GRep A) -> {GRep-proxied? x} -> (: A Type)
Gproxy-blames  : (x : GRep A) -> {GRep-proxied? x} -> Blame-Label

;; ;; interpretation is the best that you can do...? how to prove this?
;; ;; might be able to remove one load from the first cast.

;; gref-cast can do proxy collapsing via threesomes or by moving to coercions
|#

(require "../helpers.rkt"
         "../errors.rkt"
         "../language.rkt")

;; Only the pass is provided by this module
(provide introduce-castable-references)

;; The entry point for this pass it is called by impose-casting semantics
(: introduce-castable-references (Cast1-Lang Config . -> . Cast2-Lang))
(trace-define (introduce-castable-references prgm config)
  (match-let (((Prog (list name next type) expression) prgm))
    (let-values ([([e : C2-Expr] [n : Natural])
                  (run-state (icr-expr expression) next)])
      (Prog (list name n type) e))))

(: icr-expr (-> C1-Expr (State Natural C2-Expr)))
(define (icr-expr exp)
  (match exp
    [(Lambda f* (Castable fn e))
     (do (bind-state : (State Natural C2-Expr))
         (e : C2-Expr <- (icr-expr e))
         (return-state (Lambda f* (Castable fn e))))]
    [(Cast e t1 t2 l)
     (do (bind-state : (State Natural C2-Expr))
         (e : C2-Expr <- (icr-expr e))
         (if (and (GRef? t1) (GRef? t2))
             (let ([t1^ : Schml-Type (GRef-arg t1)]
                   [t2^ : Schml-Type (GRef-arg t2)])
               (return-state
                (Gproxy e (Type t1^) (Type t2^) (Quote l))))
             (return-state (Cast e t1 t2 l))))]
    [(Letrec b* e)
     (do (bind-state : (State Natural C2-Expr))
         (b* : C2-Bnd* <- (icr-bnd* b*))
         (e  : C2-Expr <- (icr-expr e))
         (return-state (Letrec b* e)))]
    [(Let b* e)
     (do (bind-state : (State Natural C2-Expr))
         (b* : C2-Bnd* <- (icr-bnd* b*))
         (e  : C2-Expr <- (icr-expr e))
         (return-state (Let b* e)))]
    [(App e e*)
     (do (bind-state : (State Natural C2-Expr))
         (e  : C2-Expr  <- (icr-expr e))
         (e* : C2-Expr* <- (icr-expr* e*))
         (return-state (App e e*)))]
    [(Op p e*)
     (do (bind-state : (State Natural C2-Expr))
         (e* : C2-Expr* <- (icr-expr* e*))
         (return-state (Op p e*)))]
    [(If t c a)
     (do (bind-state : (State Natural C2-Expr))
         (t : C2-Expr <- (icr-expr t))
         (c : C2-Expr <- (icr-expr c))
         (a : C2-Expr <- (icr-expr a))
         (return-state (If t c a)))]
    [(Begin e* e)
     (do (bind-state : (State Natural C2-Expr))
         (e* : C2-Expr* <- (icr-expr* e*))
       (e  : C2-Expr  <- (icr-expr  e))
       (return-state (Begin e* e)))]
    [(Gbox e)
     (do (bind-state : (State Natural C2-Expr))
         (e : C2-Expr <- (icr-expr e))
         (return-state (UGbox e)))]
    [(Gunbox e)
     ;; I decided to inline here because in this context letrec ; ; ; ;
     ;; can be made into a simple loop; ; ; ;
     (do (bind-state : (State Natural C2-Expr))
         (e : C2-Expr <- (icr-expr e))
         (loop : Uid <- (uid-state "loop"))
         (ref : Uid <- (uid-state "gref"))
         (let* ([l-var  (Var loop)]
                [r-var  (Var ref)]
                [r-var^ (Gproxy-for r-var)]
                [t1     (Gproxy-from r-var)]
                [t2     (Gproxy-to r-var)]
                [lbl    (Gproxy-blames r-var)]
                [recur  (App l-var (list r-var^))]
                [lexp   (Lambda (list ref)
                         (Castable #f
                          (If (GRep-proxied? r-var)
                              (Runtime-Cast recur t1 t2 lbl)
                              (UGbox-ref r-var))))])
           (return-state
            (Letrec (list (cons loop lexp))
              (App l-var (list e))))))]
    [(Gbox-set! e1 e2)
     (do (bind-state : (State Natural C2-Expr))
         (e1 : C2-Expr <- (icr-expr e1))
         (e2 : C2-Expr <- (icr-expr e2))
         (loop : Uid <- (uid-state "loop"))
         (ref : Uid <- (uid-state "gref"))
         (val  : Uid <- (uid-state "val"))
         (let* ([l-var : C2-Expr (Var loop)]
                [r-var : C2-Expr (Var ref)]
                [v-var : C2-Expr (Var val)]
                [t1  : C2-Expr (Gproxy-from r-var)]
                [t2  : C2-Expr (Gproxy-to r-var)]
                [lbl : C2-Expr (Gproxy-blames r-var)]
                [r-var^ : C2-Expr (Gproxy-for r-var)]
                ;; Switched t1 t2 for a check
                [cast-val : C2-Expr (Runtime-Cast v-var t2 t1 lbl)]
                [lexp : C2-Expr (Lambda (list ref val)
                                 (Castable #f
                                  (If (GRep-proxied? r-var)
                                      (App l-var (list r-var^ cast-val))
                                      (Begin
                                        (list (UGbox-set! r-var v-var))
                                        (Quote '())))))])
           (return-state
            (Letrec (list (cons loop lexp))
              (App l-var (list e1 e2))))))]
    [(Var i) (return-state (Var i))]
    [(Quote lit) (return-state (Quote lit))]
    [(Type-Fn-arity e)
     (do (bind-state : (State Nat C2-Expr))
         (e : C2-Expr <- (icr-expr e))
         (return-state (Type-Fn-arity e)))]
    [(Type-Fn-return e)
     (do (bind-state : (State Nat C2-Expr))
         (e : C2-Expr <- (icr-expr e))
         (return-state (Type-Fn-return e)))]
    [(Type-Fn-arg e i)
     (do (bind-state : (State Nat C2-Expr))
         (e : C2-Expr <- (icr-expr e))
         (i : C2-Expr <- (icr-expr i))
         (return-state (Type-Fn-arg e i)))]
    [(Fn-Cast e t1 t2 lbl)
     (do (bind-state : (State Nat C2-Expr))
         (e : C2-Expr <- (icr-expr e))
         (return-state (Fn-Cast e t1 t2 lbl)))]
    [(Runtime-Cast e1 e2 e3 e4)
     (do (bind-state : (State Nat C2-Expr))
         (e1 : C2-Expr <- (icr-expr e1))
         (e2 : C2-Expr <- (icr-expr e2))
         (e3 : C2-Expr <- (icr-expr e3))
         (e4 : C2-Expr <- (icr-expr e4))
         (return-state (Runtime-Cast e1 e2 e3 e4)))]
    [(Blame e)
     (do (bind-state : (State Nat C2-Expr))
         (e : C2-Expr <- (icr-expr e))
         (return-state (Blame e)))]))

(: icr-expr* (-> C1-Expr* (State Natural C2-Expr*)))
(define (icr-expr* e*) (map-state icr-expr e*))

(: icr-bnd* (-> C1-Bnd* (State Natural C2-Bnd*)))
(define (icr-bnd* b*) (map-state icr-bnd b*))

(: icr-bnd  (-> C1-Bnd (State Natural C2-Bnd)))
(define (icr-bnd b)
  (do (bind-state : (State Natural C2-Bnd))
      (match-let ([(cons i e) b])
        (e : C2-Expr <- (icr-expr e))
        (return-state (cons i e)))))
