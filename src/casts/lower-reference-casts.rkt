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
           ;; Instatiate Code Templates with the next unique names
           (gunbox-bnd     : C2-Bnd <- gunbox-template)
           (gbox-set!-bnd  : C2-Bnd <- gbox-set!-template)
           (gvect-ref-bnd  : C2-Bnd <- gvect-ref-template)
           (gvect-set!-bnd : C2-Bnd <- gvect-set!-template)
           (let* ([bnd* : C2-Bnd* (list gunbox-bnd
                                        gbox-set!-bnd
                                        gvect-ref-bnd
                                        gvect-set!-bnd)]
                  [gb-ref : Uid (car gunbox-bnd)]
                  [gb-set : Uid (car gbox-set!-bnd)]
                  [gv-ref : Uid (car gvect-ref-bnd)]
                  [gv-set : Uid (car gvect-set!-bnd)]
                  [lower (lower-reference-ops gb-ref gb-set gv-ref gv-set)])
             ;; Replace reads and writes with the calls to runtime
             (e : C2-Expr <- (lower body))
             (return-state (Letrec bnd* e))))
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

(define gvect-ref-template
  (do (bind-state : (State Nat C2-Bnd))
      (gvect-ref : Uid <- (uid-state "rt_gvect_ref"))
      (vect      : Uid <- (uid-state "rt_gvect"))
      (ind       : Uid <- (uid-state  "rt_index"))
      (let* ([gvect-ref-var  (Var gvect-ref)]
             [v-var          (Var vect)]
             [i-var          (Var ind)]
             [v-var^ (Gproxy-for v-var)]
             [t1     (Gproxy-from v-var)]
             [t2     (Gproxy-to v-var)]
             [lbl    (Gproxy-blames v-var)]
             [recur  (App gvect-ref-var (list v-var^ i-var))]
             [gbox-ref-rt-code : C2-Expr
              (Lambda (list vect ind)
               (Castable #f
                (If (GRep-proxied? v-var)
                    (Runtime-Cast recur t1 t2 lbl)
                    (UGvect-ref v-var i-var))))])
        (return-state (cons gvect-ref gbox-ref-rt-code)))))

(define gvect-set!-template
  (do (bind-state : (State Nat C2-Bnd))
      (gvect-set! : Uid <- (uid-state "rt_gvect_set"))
      (vect       : Uid <- (uid-state "rt_gvect"))
      (ind        : Uid <- (uid-state "rt_index"))
      (val        : Uid <- (uid-state "rt_write_val"))
      (let* ([gvect-set!-var : C2-Expr (Var gvect-set!)]
             [vt-var         : C2-Expr (Var vect)]
             [i-var          : C2-Expr (Var ind)]
             [v-var          : C2-Expr (Var val)]
             [t1      : C2-Expr (Gproxy-from vt-var)]
             [t2      : C2-Expr (Gproxy-to vt-var)]
             [lbl     : C2-Expr (Gproxy-blames vt-var)]
             [vt-var^ : C2-Expr (Gproxy-for vt-var)]
             ;; Switched t1 t2 for a check
             [cast-val : C2-Expr (Runtime-Cast v-var t2 t1 lbl)]
             [gvect-set!-rt-code : C2-Expr
              (Lambda (list vect ind val)
               (Castable #f
                (If (GRep-proxied? vt-var)
                    (App gvect-set!-var (list vt-var^ i-var cast-val))
                    (UGvect-set! vt-var i-var v-var))))])
        (return-state (cons gvect-set! gvect-set!-rt-code)))))

;; Parameter determining if the code generated for gbox-set! and gunbox
;; performs the first check to see if the gref is a unguarded referenc
;; or delegates the entire operation to the runtime.
(define inline-guarded-branch?
  (make-parameter #t))

;; Inlines the first branch for any guarded operation if the inline-guarded-branch?
;; flag is set.
;; The un-inlined version just calls the runtime procedure which will perform the
;; operation on the given variables.
;; The inline version checks to see if the value is proxied if not it does a
;; raw read, otherwise it calls off to the runtime proceedure.
;; TODO This could be achieved with regular inlining... Perhaps we should lookat that.

(define-syntax (guarded-branch stx)
  (syntax-case stx ()
    [(_ unguarded-op runtime-op-var (n . e) (n* . e*) ...)
     (with-syntax ([(u u* ...) (generate-temporaries #'(n n* ...))]
                   [(v v* ...) (generate-temporaries #'(n n* ...))])
       #'(if (inline-guarded-branch?)
             (do (bind-state : (State Nat C2-Expr))
              (u  : Uid <- (uid-state n))
              (u* : Uid <- (uid-state n*))
              ...
              (let ([v  : C2-Expr (Var u)]
                    [v* : C2-Expr (Var u*)]
                    ...)
                (return-state
                 (Let `((,u  . ,e )
                        (,u* . ,e*)
                        ...)
                      (If (GRep-proxied? v)
                          (App runtime-op-var `(,v ,v* ...))
                          (unguarded-op v v* ...))))))
             (return-state (App runtime-op-var `(,e ,e* ...)))))]))

;; Map over the expression tree lowering reference operations into
;; more explicit operations on the GRef interface described at the
;; top of the file.
(: lower-reference-ops (Uid Uid Uid Uid -> (C1-Expr -> (State Nat C2-Expr))))
(define  (lower-reference-ops gunbox-id gbox-set!-id gvect-ref-id gvect-set!-id)
  (define gunbox     : C2-Expr (Var gunbox-id))
  (define gbox-set!  : C2-Expr (Var gbox-set!-id))
  (define gvect-ref  : C2-Expr (Var gvect-ref-id))
  (define gvect-set! : C2-Expr (Var gvect-set!-id))
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
          [(Gunbox b)
           (b : C2-Expr <- (recur b))
           (guarded-branch UGbox-ref gunbox ("gbox" . b))
#;
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
          ;; Setting a Gaurded reference results in iteratively applying
          ;; all the guarding casts to the value to be written.
          [(Gbox-set! b w)
           (b : C2-Expr <- (recur b))
           (w : C2-Expr <- (recur w))
           (guarded-branch UGbox-set! gbox-set! ("gbox" . b) ("value" . w))]
          [(Gvector size init)
           (size : C2-Expr <- (recur size))
           (init : C2-Expr <- (recur init))
           (return-state (UGvect size init))]
          [(Gvector-ref v i)
           (v : C2-Expr <- (recur v))
           (i : C2-Expr <- (recur i))
           (guarded-branch UGvect-ref gvect-ref ("gvect" . v) ("index" . i))]
          [(Gvector-set! v i w)
           (v : C2-Expr <- (recur v))
           (i : C2-Expr <- (recur i))
           (w : C2-Expr <- (recur w))
           (guarded-branch UGvect-set! gvect-set!
                           ("gvect" . v) ("index" . i) ("value" . w))
           #;
           (if (inline-guarded-branch?)
           (doing
           (v : Uid <- (uid-state "gvect_value"))
           (u : Uid <- (uid-state "index_value"))
           (w : Uid <- (uid-state "write_value"))
           (let ([gvect : C2-Expr (Var v)]
                 [index : C2-Expr (Var u)]
                 [write : C2-Expr (Var w)])
             (return-state
              (Let `((,v . ,e1) (,u . ,i) (,w . ,e2))
                   (If (GRep-proxied? gvect)
                       (UGvect-set! gvect index write)
                       (App gvect-set! `(,gvect ,index ,write)))))))
          (return-state (App gvect-set! `(,e1 ,i ,e2))))]
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
  (: recur* (C1-Expr* -> (State Nat C2-Expr*)))
  (define (recur* e*) (map-state recur e*))
  ;; recur over bindings
  (: recur-bnd* (C1-Bnd* -> (State Nat C2-Bnd*)))
  (define (recur-bnd* b*) (map-state recur-bnd b*))
  ;; recur over a single binding
  (: recur-bnd (C1-Bnd -> (State Nat C2-Bnd)))
  (define (recur-bnd b)
    (do (bind-state : (State Nat C2-Bnd))
        (match-let ([(cons i e) b])
          (e : C2-Expr <- (recur e))
          (return-state (cons i e)))))
  ;; recur is a recursive closure that results from lower-reference-ops
  recur)
