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
;;proxies for Guarded Representations
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
(provide lower-reference-casts)

;; The entry point for this pass it is called by impose-casting semantics
(: lower-reference-casts (Cast1-Lang Config . -> . Cast2-Lang))
(trace-define (lower-reference-casts prgm config)
  (match-let (((Prog (list name next-unique type) body) prgm))
    (define-values (prgm+ref-runtime next-unique^)
      ;; Template Generation and Lowering onf operators is monadic
      (run-state
       (do (bind-state : (State Nat C2-Expr))
           ((and gunbox-bnd    (cons gunbox    _)) : C2-Bnd <- gunbox-template)
           ((and gbox-set!-bnd (cons gbox-set! _)) : C2-Bnd
            <- gbox-set!-template)
           (e : C2-Expr <- ((lower-reference-ops gunbox gbox-set!) body))
           (return-state (Letrec `(,gunbox-bnd ,gbox-set!-bnd) e)))
       next-unique))
    (Prog (list name next-unique^ type) prgm+ref-runtime)))


;; Template to Build the GRef Read Runtime Code
;; The current implementation recursively removes proxies until
;; it finds an unguarded box, reads the value from the box, and
;; returns through each proxying stack frame casting the value
;; as specified by the previously travered proxy.
;; TODO amk get rid of this closure -- use a code segment
;; TODO amk if inline-guarded-code? then I could do slightly
;;;;;;;;;;;;better code generation here.
(define gunbox-template
 (do (bind-state : (State Nat C2-Bnd))
     (gunbox : Uid <- (uid-state "rt_gunbox"))
     (gref   : Uid <- (uid-state "rt_gref"))
     (let* ([gunbox-e : C2-Expr (Var gunbox)]
            [gref-e   : C2-Expr (Var gref)]
            [gref-for : C2-Expr (Gproxy-for    gref-e)]
            [gref-t1  : C2-Expr (Gproxy-from   gref-e)]
            [gref-t2  : C2-Expr (Gproxy-to     gref-e)]
            [gref-lbl : C2-Expr (Gproxy-blames gref-e)]
            [recur    : C2-Expr (App gunbox-e (list gref-for))]
            [gunbox-rt-code : C2-Expr
             (Lambda (list gref)
              (Castable #f
                 (If (GRep-proxied? gref-e)
                     (Runtime-Cast recur gref-t1 gref-t2 gref-lbl)
                     (UGbox-ref gref-e))))])
       (return-state (cons gunbox gunbox-rt-code)))))

;; A template to build the GRef Write Runtime Code
;; The current implementation recursively removes proxies
;; casting the value to be written as specified by each traversed
;; proxy. When a unguarded proxy is found the value is written.
;; The invarient of guarded guarantees that the unguarded box will
;; always contain the same type as the value at this point.
;; Note: NO PROOF FOR THIS CLAIM
;; TODO amk get rid of this closure -- use a code segment
(define gbox-set!-template
  (do (bind-state : (State Nat C2-Bnd))
      (gbox-set! : Uid <- (uid-state "rt_gbox_set"))
      (gref      : Uid <- (uid-state "rt_gref"))
      (val       : Uid <- (uid-state "rt_write_val"))
      ;; All variables that will be used for
      (let* ([gbox-set-e (Var gbox-set!)]
             [gref-e     (Var gref)]
             [val-e      (Var val)]
             [gref-for (Gproxy-for    gref-e)]
             [gref-t1  (Gproxy-from   gref-e)]
             [gref-t2  (Gproxy-to     gref-e)]
             [gref-lbl (Gproxy-blames gref-e)]
             [cast-e   (Runtime-Cast val-e gref-t2 gref-t1 gref-lbl)]
             [gbox-set!-rt-code
              (Lambda (list gref val)
               (Castable #f
                (If (GRep-proxied? gref-e)
                    (App gbox-set-e (list gref-for cast-e))
                    (UGbox-set! gref-e val-e))))])
        (return-state (cons gbox-set! gbox-set!-rt-code)))))

;; Parameter determining if the code generated for gbox-set! and gunbox
;; performs the first check to see if the gref is a unguarded referenc
;; or delegates the entire operation to the runtime.
(define inline-guarded-branch?
  (make-parameter #f))

;; Map over the expression tree lowering reference operations into
;; more explicit operations on the GRef interface described at the
;; top of the file.
(: lower-reference-ops (Uid Uid -> (C1-Expr -> (State Nat C2-Expr))))
(define  (lower-reference-ops gunbox-id gbox-set!-id)
  (define gunbox    : C2-Expr (Var gunbox-id))
  (define gbox-set! : C2-Expr (Var gbox-set!-id))
  ;; recur is a recursive closure returned by lower-reference-ops
  (: recur (C1-Expr -> (State Nat C2-Expr)))
  (define (recur exp)
    ;; The following match is in the (State Nat A) monad to support unique
    ;; Identifiers expect match clauses to look like [pat Mexp ... Mexp0]
    (do (bind-state : (State Nat C2-Expr))
        (match exp
          ;; Interesting Cases -----------------------------------------------
          ;; Casts from Guarded Reference to Guarded References are represented
          ;; as a data structure proxying the Original Reference.
          [(Cast e t1 t2 l)
           (e : C2-Expr <- (recur e))
           (match* (t1 t2)
             [((GRef t1) (GRef t2))
              (return-state (Gproxy e (Type t1) (Type t2) (Quote l)))]
             [((GVect t1) (GVect t2))
              (return-state (Gproxy e (Type t1) (Type t2) (Quote l)))]
             [(_ _) (return-state (Cast e t1 t2 l))])]
          ;; Every Guarded Reference Starts As an Unguarded (Normal) box
          [(Gbox e)
           (e : C2-Expr <- (recur e))
           (return-state (UGbox e))]
          ;; Unboxing calls off to the runtime unbox operation
          [(Gunbox e)
           (e : C2-Expr <- (recur e))
           (if (inline-guarded-branch?)
               (doing
                (r : Uid <- (uid-state "gref_value"))
                (let ([gref : C2-Expr (Var r)])
                  (return-state
                   (Let `((,r . ,e))
                        (If (GRep-proxied? gref)
                            (UGbox-ref gref)
                            (App gunbox `(,gref))))))) 
               (return-state (App gunbox `(,e))))]
          ;; Setting a Gaurded refference results in iteratively applying
          ;; all the guarding casts to the value to be written.
          [(Gbox-set! e1 e2)
           (e1 : C2-Expr <- (recur e1))
           (e2 : C2-Expr <- (recur e2))
           (if (inline-guarded-branch?)
               (doing
                (r : Uid <- (uid-state "gref_value"))
                (v : Uid <- (uid-state "write_value"))
                (let ([gref : C2-Expr (Var r)]
                      [val  : C2-Expr (Var v)])
                  (return-state
                   (Let `((,r . ,e1) (,v . ,e2))
                        (If (GRep-proxied? gref)
                            (UGbox-set! gref val)
                            (App gbox-set! `(,gref ,val)))))))
               (return-state (App gbox-set! `(,e1 ,e2))))]
          [(Gvector n e)
           (e : C2-Expr <- (recur e))
           (n : C2-Expr <- (recur n))
           (return-state (UGvect n e))]
          [(Gvector-ref e i)
           (e : C2-Expr <- (recur e))
           (i : C2-Expr <- (recur i))
           (loop : Uid <- (uid-state "loop"))
           (vect : Uid <- (uid-state "gref"))
           (ind : Uid <- (uid-state "index"))
           (let* ([l-var  (Var loop)]
                  [v-var  (Var vect)]
                  [i-var  (Var ind)]
                  [v-var^ (Gproxy-for v-var)]
                  [t1     (Gproxy-from v-var)]
                  [t2     (Gproxy-to v-var)]
                  [lbl    (Gproxy-blames v-var)]
                  [recur  (App l-var (list v-var^ i-var))]
                  [lexp   (Lambda (list vect ind)
                           (Castable #f
                                     (If (GRep-proxied? v-var)
                                         (Runtime-Cast recur t1 t2 lbl)
                                         (UGvect-ref v-var i-var))))])
             (return-state
              (Letrec (list (cons loop lexp))
                      (App l-var (list e i)))))]
          [(Gvector-set! e1 i e2)
           (e1 : C2-Expr <- (recur e1))
           (i : C2-Expr <- (recur i))
           (e2 : C2-Expr <- (recur e2))
           (loop : Uid <- (uid-state "loop"))
           (vect : Uid <- (uid-state "gvect"))
           (val  : Uid <- (uid-state "val"))
           (ind : Uid <- (uid-state "index"))
           (let* ([l-var : C2-Expr (Var loop)]
                  [vt-var : C2-Expr (Var vect)]
                  [i-var : C2-Expr (Var ind)]
                  [v-var : C2-Expr (Var val)]
                  [t1  : C2-Expr (Gproxy-from vt-var)]
                  [t2  : C2-Expr (Gproxy-to vt-var)]
                  [lbl : C2-Expr (Gproxy-blames vt-var)]
                  [vt-var^ : C2-Expr (Gproxy-for vt-var)]
                  ;; Switched t1 t2 for a check
                  [cast-val : C2-Expr (Runtime-Cast v-var t2 t1 lbl)]
                  [lexp : C2-Expr (Lambda (list vect ind val)
                                          (Castable #f
                                                    (If (GRep-proxied? vt-var)
                                                        (App l-var (list vt-var^ i-var cast-val))
                                                        (Begin
                                                          (list (UGvect-set! vt-var i-var v-var))
                                                          (Quote '())))))])
             (return-state
              (Letrec (list (cons loop lexp))
                      (App l-var (list e1 i e2)))))]
          ;; Boring Recursion Cases --------------------------------
          [(Lambda f* (Castable fn e))
           (e : C2-Expr <- (recur e))
           (return-state (Lambda f* (Castable fn e)))]
          [(Letrec b* e)
           (b* : C2-Bnd* <- (recur-bnd* b*))
           (e  : C2-Expr <- (recur e))
           (return-state (Letrec b* e))]
          [(Let b* e)
           (b* : C2-Bnd* <- (recur-bnd* b*))
           (e  : C2-Expr <- (recur e))
           (return-state (Let b* e))]
          [(App e e*)
           (e  : C2-Expr  <- (recur e))
           (e* : C2-Expr* <- (recur* e*))
           (return-state (App e e*))]
          [(Op p e*)
           (e* : C2-Expr* <- (recur* e*))
           (return-state (Op p e*))]
          [(Var i) (return-state (Var i))]
          [(Quote lit) (return-state (Quote lit))]
          [(If t c a)
           (t  : C2-Expr  <- (recur t))
           (c  : C2-Expr  <- (recur c))
           (a  : C2-Expr  <- (recur a))
           (return-state (If t c a))]
          [(Begin e* e)
           (e* : C2-Expr* <- (recur* e*))
           (e  : C2-Expr  <- (recur e))
           (return-state (Begin e* e))]
          [(Repeat i s e b)
           (s  : C2-Expr  <- (recur s))
           (e  : C2-Expr  <- (recur e))
           (b  : C2-Expr  <- (recur b))
           (return-state (Repeat i s e b))]
          [(Type-Fn-arity e)
           (e : C2-Expr <- (recur e))
           (return-state (Type-Fn-arity e))]
          [(Type-Fn-return e)
           (e : C2-Expr <- (recur e))
           (return-state (Type-Fn-return e))]
          [(Type-Fn-arg t o)
           (t : C2-Expr <- (recur t))
           (o : C2-Expr <- (recur o))
           (return-state (Type-Fn-arg t o))]
          [(Fn-Cast e t1 t2 lbl)
           (e : C2-Expr <- (recur e))
           (return-state (Fn-Cast e t1 t2 lbl))]
          [(Runtime-Cast e t1 t2 l)
           (e  : C2-Expr <- (recur e))
           (t1 : C2-Expr <- (recur t1))
           (t2 : C2-Expr <- (recur t2))
           (l  : C2-Expr <- (recur l))
           (return-state (Runtime-Cast e t1 t2 l))]
          [(Blame e)
           (e : C2-Expr <- (recur e))
           (return-state (Blame e))])))
  ;; recur over a list of expressions
  (define (recur* [e* : C1-Expr*]) : (State Nat C2-Expr*)
    (map-state recur e*))
  ;; recur over bindings
  (define (recur-bnd* [b* : C1-Bnd*]) : (State Nat C2-Bnd*)
    (map-state
     (lambda ([b : C1-Bnd])
       (do (bind-state : (State Nat C2-Bnd))
           (match-let ([(cons i e) b])
             (e : C2-Expr <- (recur e))
             (return-state (cons i e)))))
     b*))
  ;; recur is a recursive closure that results from lower-reference-ops
  recur)
