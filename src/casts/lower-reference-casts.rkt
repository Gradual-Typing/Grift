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
Guarded-Proxy-Huh : GRep A -> Bool

;; Once you are able to tell whether a GRep is Unguarded
UGbox-read (x : GRep A) -> {not (Guarded-Proxy-Huh x)} -> A
UGbox-write (x : GRep A) -> {not (Guarded-Proxy-Huh x)} -> A -> Unit

;; Or if you can tell the a GRep is a Proxy
Guarded-Proxy-Ref     : (x : GRep A) -> {Guarded-Proxy-Huh x} -> GRep B
Guarded-Proxy-Source    : (x : GRep A) -> {Guarded-Proxy-Huh x} -> (: B Type)
Guarded-Proxy-Target      : (x : GRep A) -> {Guarded-Proxy-Huh x} -> (: A Type)
Guarded-Proxy-Blames  : (x : GRep A) -> {Guarded-Proxy-Huh x} -> Blame-Label

;; ;; interpretation is the best that you can do...? how to prove this?
;; ;; might be able to remove one load from the first cast.

;; gref-cast can do proxy collapsing via threesomes or by moving to coercions
|#

(require "../helpers.rkt"
         "../errors.rkt"
         "../configuration.rkt"
         "../language/cast-or-coerce1.rkt"
         "../language/cast-or-coerce2.rkt")

;; Only the pass is provided by this module
(provide
 lower-reference-casts
 (all-from-out
  "../language/cast-or-coerce1.rkt"
  "../language/cast-or-coerce2.rkt"))

;; Flags for this pass

;; inline-guarde-branch
;; Parameter determining if the code generated for gbox-set! and gunbox
;; performs the first check to see if the gref is a unguarded referenc
;; or delegates the entire operation to the runtime.
;; This is only used for the twosome representation
(define inline-guarded-branch?
  (make-parameter #t))

;; The entry point for this pass it is called by impose-casting semantics
(: lower-reference-casts (Cast-or-Coerce1-Lang Config . -> . Cast-or-Coerce2-Lang))
(trace-define (lower-reference-casts prgm config)
  (match-let (((Prog (list name next-unique type) body) prgm))
    (define-values (prgm+ref-runtime next-unique^)
      (run-state
       (do (bind-state : (State Nat CoC2-Expr))
           ((list gunbox gbox-set! gvect-ref gvect-set! build-runtime)
            : (List GunboxT Gbox-setT
                    Gvect-refT Gvect-setT
                    (CoC2-Expr -> CoC2-Expr))
            <- (get-ref-helpers (Config-cast-rep config)))
           (e : CoC2-Expr
              <- ((lrc-expr gunbox gbox-set! gvect-ref gvect-set!) body))
           (return-state (build-runtime e)))
       next-unique))
    (Prog (list name next-unique^ type) prgm+ref-runtime)))

;; Functions for use sites of guarded references with coersions
(define-type GunboxT ((Var Uid) -> CoC2-Expr))
(define-type Gbox-setT ((Var Uid) (U (Quote Cast-Literal) (Var Uid)) ->
                        CoC2-Expr))
(define-type Gvect-refT ((Var Uid) (U (Quote Integer) (Var Uid)) -> CoC2-Expr))
(define-type Gvect-setT ((Var Uid)
                         (U (Quote Integer) (Var Uid))
                         (U (Quote Cast-Literal) (Var Uid))
                         -> CoC2-Expr))

(: gunbox/coercion GunboxT)
(define (gunbox/coercion gref)
  (If (Guarded-Proxy-Huh gref)
      (Interpreted-Cast
       (Unguarded-Box-Ref (Guarded-Proxy-Ref gref))
       (Coercion (Ref-Coercion-Read (Guarded-Proxy-Coercion gref))))
      (Unguarded-Box-Ref gref)))

(: gbox-set!/coercion Gbox-setT)
(define (gbox-set!/coercion gref val)
  (If (Guarded-Proxy-Huh gref)
      (Unguarded-Box-Set!
       (Guarded-Proxy-Ref gref)
       (Interpreted-Cast
        val
        (Coercion (Ref-Coercion-Write (Guarded-Proxy-Coercion gref)))))
      (Unguarded-Box-Set! gref val)))

(: gvect-ref/coercion Gvect-refT)
(define (gvect-ref/coercion gref index)
  (If (Guarded-Proxy-Huh gref)
      (Interpreted-Cast
       (Unguarded-Vect-Ref (Guarded-Proxy-Ref gref) index)
       (Coercion (Ref-Coercion-Read (Guarded-Proxy-Coercion gref))))
      (Unguarded-Vect-Ref gref index)))

(: gvect-set!/coercion Gvect-setT)
(define (gvect-set!/coercion gref index val)
  (let ([index (if (integer? index) (Quote index) index)])
    (If (Guarded-Proxy-Huh gref)
        (Unguarded-Vect-Set!
         (Guarded-Proxy-Ref gref)
         index
         (Interpreted-Cast
          val
          (Coercion (Ref-Coercion-Write (Guarded-Proxy-Coercion gref)))))
        (Unguarded-Vect-Set! gref index val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; How we implement the guarded runtime routines
;; -- We used to build a template from scratch but this resulted
;;    in some pretty verbose and illegible code.
;; -- Now we use the helper functions that inline the first branch
;;    to build the body of the runtime routine itself.

;; gunbox/twosome - builds the code to perform an gunbox
;; if inline-guarded-branch is true the this also will perform
;; the check to see if the reference is guarded and pealing of
;; a layer if so.
(: gunbox/twosome ((Code-Label Uid)-> GunboxT))
(define ((gunbox/twosome lbl) gref)
  (if (inline-guarded-branch?)
      (If (Guarded-Proxy-Huh gref)
          (Interpreted-Cast
           (App-Code lbl (list (Guarded-Proxy-Ref gref)))
           (Twosome (Guarded-Proxy-Source gref)
                    (Guarded-Proxy-Target gref)
                    (Guarded-Proxy-Blames gref)))
          (Unguarded-Box-Ref gref))
      (App-Code lbl (list gref))))

;; Template to Build the GRef Read Runtime Code
;; The current implementation recursively removes proxies until
;; it finds an unguarded box, reads the value from the box, and
;; returns through each proxying stack frame casting the value
;; as specified by the previously travered proxy.

;; As noted above this definition just allocates the unique
;; names and calls off two the call site generator.
(define gunbox-template
 (do (bind-state : (State Nat CoC2-Bnd-Code))
     (gunbox : Uid <- (uid-state "rt_gunbox"))
     (gref   : Uid <- (uid-state "rt_gref"))
     ;;
     #|(let* ([gunbox-e : CoC2-Expr (Code-Label gunbox)]
            [gref-e   : CoC2-Expr (Var gref)]
            [gref-for : CoC2-Expr (Guarded-Proxy-Ref    gref-e)]
            [gref-t1  : CoC2-Expr (Guarded-Proxy-Source   gref-e)]
            [gref-t2  : CoC2-Expr (Guarded-Proxy-Target     gref-e)]
            [gref-lbl : CoC2-Expr (Guarded-Proxy-Blames gref-e)]
            [recur    : CoC2-Expr (App-Code gunbox-e (list gref-for))]
            [gunbox-rt-code : CoC2-Code
                            (Code (list gref)
                                  (If (Guarded-Proxy-Huh gref-e)
                                      (Interpreted-Cast recur (Twosome gref-t1 gref-t2 gref-lbl))
                                      (Unguarded-Box-Ref gref-e)))]))|#
     (return-state
      (parameterize ([inline-guarded-branch? #t])
        (cons
         gunbox
         (Code `(,gref)
          ((gunbox/twosome (Code-Label gunbox)) (Var gref))))))))

(: gbox-set!/twosome ((Code-Label Uid) -> Gbox-setT))
(define ((gbox-set!/twosome lbl) gref val)
  (if (inline-guarded-branch?)
      (If (Guarded-Proxy-Huh gref)
          (App-Code
           lbl
           (list (Guarded-Proxy-Ref gref)
                 (Interpreted-Cast
                  val
                  (Twosome (Guarded-Proxy-Target gref)
                           (Guarded-Proxy-Source gref)
                           (Guarded-Proxy-Blames gref)))))
          (Unguarded-Box-Set! gref val))
      (App-Code lbl (list gref val))))

;; A template to build the GRef Write Runtime Code
;; The current implementation recursively removes proxies
;; casting the value to be written as specified by each traversed
;; proxy. When a unguarded proxy is found the value is written.
;; The invarient of guarded guarantees that the unguarded box will
;; always contain the same type as thera value at this point.
(define gbox-set!-template
  (do (bind-state : (State Nat CoC2-Bnd-Code))
      (gbox-set! : Uid <- (uid-state "rt_gbox_set"))
      (gref      : Uid <- (uid-state "rt_gref"))
      (val       : Uid <- (uid-state "rt_write_val"))
      ;; All variables that will be used for
      #|(let* ([gbox-set-e (Code-Label gbox-set!)]
      [gref-e     (Var gref)]
      [val-e      (Var val)]
      [gref-for (Guarded-Proxy-Ref    gref-e)]
      [gref-t1  (Guarded-Proxy-Source   gref-e)]
      [gref-t2  (Guarded-Proxy-Target     gref-e)]
      [gref-lbl (Guarded-Proxy-Blames gref-e)]
      [cast-e
      (Interpreted-Cast val-e (Twosome gref-t2 gref-t1 gref-lbl))]
      [gbox-set!-rt-code : CoC2-Code
                         (Code (list gref val)
                               (If (Guarded-Proxy-Huh gref-e)
                                   (App-Code gbox-set-e (list gref-for cast-e))
                                   (Unguarded-Box-Set! gref-e val-e)))])
          )|#
      (return-state
       (parameterize ([inline-guarded-branch? #t])
         (cons
          gbox-set!
          (Code (list gref val)
           ((gbox-set!/twosome (Code-Label gbox-set!))
            (Var gref) (Var val))))))))

(: gvect-ref/twosome ((Code-Label Uid) -> Gvect-refT))
(define ((gvect-ref/twosome lbl) gref index)
  (if (inline-guarded-branch?)
      (If (Guarded-Proxy-Huh gref)
          (Interpreted-Cast
           (App-Code lbl (list (Guarded-Proxy-Ref gref) index))
           (Twosome (Guarded-Proxy-Source gref)
                    (Guarded-Proxy-Target gref)
                    (Guarded-Proxy-Blames gref)))
          (Unguarded-Vect-Ref gref index))
      (App-Code lbl (list gref index))))

(define gvect-ref-template
  (do (bind-state : (State Nat CoC2-Bnd-Code))
      (gvect-ref : Uid <- (uid-state "rt_gvect_ref"))
      (vect      : Uid <- (uid-state "rt_gvect"))
      (ind       : Uid <- (uid-state  "rt_index"))
      #|(let* ([gvect-ref-e  (Code-Label gvect-ref)]
             [v-var          (Var vect)]
             [i-var          (Var ind)]
             [v-var^ (Guarded-Proxy-Ref v-var)]
             [t1     (Guarded-Proxy-Source v-var)]
             [t2     (Guarded-Proxy-Target v-var)]
             [lbl    (Guarded-Proxy-Blames v-var)]
             [recur  (App-Code gvect-ref-e (list v-var^ i-var))]
             [gbox-ref-rt-code : CoC2-Code
              (Code (list vect ind)
                (If (Guarded-Proxy-Huh v-var)
                    (Interpreted-Cast recur (Twosome t1 t2 lbl))
                    (Unguarded-Vect-Ref v-var i-var)))])
          (return-state (cons gvect-ref gbox-ref-rt-code)))|#
      (return-state
       (parameterize ([inline-guarded-branch? #t])
         (cons
          gvect-ref
          (Code (list vect ind)
           ((gvect-ref/twosome (Code-Label gvect-ref))
            (Var vect) (Var ind))))))))

(: gvect-set!/twosome ((Code-Label Uid) -> Gvect-setT))
(define ((gvect-set!/twosome lbl) gref index val)
  (if (inline-guarded-branch?)
      (If (Guarded-Proxy-Huh gref)
          (App-Code
           lbl
           (list (Guarded-Proxy-Ref gref)
                 index
                 (Interpreted-Cast
                  val
                  (Twosome (Guarded-Proxy-Target gref)
                           (Guarded-Proxy-Source gref)
                           (Guarded-Proxy-Blames gref)))))
          (Unguarded-Vect-Set! gref index val))
      (App-Code lbl (list gref index val))))

(define gvect-set!-template
  (do (bind-state : (State Nat CoC2-Bnd-Code))
      (gvect-set! : Uid <- (uid-state "rt_gvect_set"))
      (vect       : Uid <- (uid-state "rt_gvect"))
      (ind        : Uid <- (uid-state "rt_index"))
      (val        : Uid <- (uid-state "rt_write_val"))
      #|
      (let* ([gvect-set!-e (Code-Label gvect-set!)]
      [vt-var         : CoC2-Expr (Var vect)]
      [i-var          : CoC2-Expr (Var ind)]
      [v-var          : CoC2-Expr (Var val)]
      [t1      : CoC2-Expr (Guarded-Proxy-Source vt-var)]
      [t2      : CoC2-Expr (Guarded-Proxy-Target vt-var)]
      [lbl     : CoC2-Expr (Guarded-Proxy-Blames vt-var)]
      [vt-var^ : CoC2-Expr (Guarded-Proxy-Ref vt-var)]
      ;; Switched t1 t2 for a check ;
      [cast-val : CoC2-Expr (Interpreted-Cast v-var (Twosome t2 t1 lbl))]
      [gvect-set!-rt-code : CoC2-Code
                          (Code (list vect ind val)
                                (If (Guarded-Proxy-Huh vt-var)
                                    (App-Code gvect-set!-e (list vt-var^ i-var cast-val))
                                    (Unguarded-Vect-Set! vt-var i-var v-var)))])
      (return-state (cons gvect-set! gvect-set!-rt-code)))|#
      (return-state
       (parameterize ([inline-guarded-branch? #t])
         (cons
          gvect-set!
          (Code `(,vect ,ind ,val)
           ((gvect-set!/twosome (Code-Label gvect-set!))
            (Var vect) (Var ind) (Var val))))))))

(: get-ref-helpers (Cast-Representation
                    -> (State Nat (List GunboxT Gbox-setT
                                        Gvect-refT Gvect-setT
                                        (CoC2-Expr -> CoC2-Expr)))))
(define (get-ref-helpers rep)
  (do (bind-state : (State Nat (List GunboxT Gbox-setT
                                     Gvect-refT Gvect-setT
                                     (CoC2-Expr -> CoC2-Expr))))
      (match rep
        ['Twosomes
         (`(,gunbox-u   . ,gunbox-c)   : CoC2-Bnd-Code <- gunbox-template)
         (`(,gbox-set-u . ,gbox-set-c) : CoC2-Bnd-Code <- gbox-set!-template)
         (`(,gvec-ref-u . ,gvec-ref-c) : CoC2-Bnd-Code <- gvect-ref-template)
         (`(,gvec-set-u . ,gvec-set-c) : CoC2-Bnd-Code <- gvect-set!-template)
         (return-state
          (list
           (gunbox/twosome (Code-Label gunbox-u))
           (gbox-set!/twosome (Code-Label gbox-set-u))
           (gvect-ref/twosome (Code-Label gvec-ref-u))
           (gvect-set!/twosome (Code-Label gvec-set-u))
           (lambda ([e : CoC2-Expr])
             (Labels `((,gunbox-u   . ,gunbox-c)  
                       (,gbox-set-u . ,gbox-set-c)
                       (,gvec-ref-u . ,gvec-ref-c)
                       (,gvec-set-u . ,gvec-set-c))
               e))))]
        ['Coercions
         (return-state
          (list gunbox/coercion gbox-set!/coercion
                gvect-ref/coercion gvect-set!/coercion
                (lambda ([e : CoC2-Expr]) e)))]
        [other (error 'lrc/get-ref-help "unmatched ~a" other)])))


;; Map over the expression tree lowering reference operations into
;; more explicit operations on the GRef interface described at the
;; top of the file.
(: lrc-expr (GunboxT Gbox-setT Gvect-refT Gvect-setT ->
                     (CoC1-Expr -> (State Nat CoC2-Expr))))
(define (lrc-expr gunbox gbox-set! gvect-ref gvect-set!)
  ;; Recur through code bindings
  (: lrc-bnd-code ((CoC1-Expr ->  (State Nat CoC2-Expr)) ->
                   (CoC1-Bnd-Code -> (State Nat CoC2-Bnd-Code))))
  (define ((lrc-bnd-code recur) b)
    (do (bind-state : (State Nat CoC2-Bnd-Code))
        (match b
          [(cons u (Code u* e))
           (e : CoC2-Expr <- (recur e))
           (return-state (cons u (Code u* e)))]
          [other (error 'lrc-bnd-code)])))

  ;; recur is a recursive closure returned by lower-reference-ops
  (: recur (CoC1-Expr -> (State Nat CoC2-Expr)))
  (define (recur exp)
    ;; The following match is in the (State Nat A) monad to support unique
    ;; Identifiers expect match clauses to look like [pat Mexp ... Mexp0]
    (do (bind-state : (State Nat CoC2-Expr))
        (match exp
          ;; Interesting Cases -----------------------------------------------
          ;; Casts from Guarded Reference to Guarded References are represented
          ;; as a data structure proxying the Original Reference.
          [(Cast e rep)
           (e : CoC2-Expr <- (recur e))
           (match rep
             [(Twosome t1 t2 l)
              (match* (t1 t2)
                [((GRef t1) (GRef t2))
                 (return-state
                  (Guarded-Proxy e (Twosome (Type t1) (Type t2) (Quote l))))]
                [((GVect t1) (GVect t2))
                 (return-state
                  (Guarded-Proxy e (Twosome (Type t1) (Type t2) (Quote l))))]
                [(_ _) (return-state (Cast e (Twosome t1 t2 l)))])]
             [(Coercion c)
              (match c
                [(Ref r w)
                 (u : Uid <- (uid-state "ref_coercion"))
                 (`(,b* . ,v) : (Pair CoC2-Bnd* (Var Uid))
                  <- (if (Var? e)
                         (return-state `(() . ,e))
                         (bind-state
                          (uid-state "guarded-ref")
                          (lambda ([u : Uid])
                            : (State Nat (Pair CoC2-Bnd* (Var Uid)))
                            (return-state `(((,u . ,e)) . ,(Var u)))))))
                 (let ([e (If (Guarded-Proxy-Huh v)
                              (Let `((,u . ,(Guarded-Proxy-Coercion v)))
                               (Guarded-Proxy
                                (Guarded-Proxy-Ref v)
                                ;; TODO Get Rid of the second coercion here
                                (Coercion
                                 (Ref-Coercion
                                  (Compose-Coercions
                                   (Ref-Coercion-Read (Var u))
                                   (Quote-Coercion r))
                                  (Compose-Coercions
                                   (Quote-Coercion w)
                                   (Ref-Coercion-Write (Var u)))))))
                              (Guarded-Proxy
                               v
                               (Coercion (Quote-Coercion (Ref r w)))))])
                   (if (null? b*)
                       (return-state e)
                       (return-state (Let b* e))))]
                [c (return-state (Cast e (Coercion c)))])])]
          ;; Every Guarded Reference Starts As an Unguarded box
          [(Gbox e)
           (e : CoC2-Expr <- (recur e))
           (return-state (Unguarded-Box e))]
          ;; Unboxing calls off to the helpers we have defined
          [(Gunbox b)
           (b : CoC2-Expr <- (recur b))
           (if (Var? b)
               (return-state (gunbox b))
               (bind-state
                (uid-state "gbox")
                (lambda ([u : Uid])
                  : (State Nat CoC2-Expr)
                  (return-state (Let `((,u . ,b)) (gunbox (Var u)))))))]
          ;; Setting a Gaurded reference results in iteratively applying
          ;; all the guarding casts to the value to be written.
          ;; Most of the work is already done but the code requires values
          ;; so there is a little repetative to let bind any values that
          ;; haven't been evaluated.
          [(Gbox-set! b w)
           (b : CoC2-Expr <- (recur b))
           (w : CoC2-Expr <- (recur w))
           ((cons b* b) : (Pair CoC2-Bnd* (Var Uid))
            <- (if (Var? b)
                   (return-state (cons '() b))
                   (bind-state
                    (uid-state "gbox")
                    (lambda ([u : Uid])
                      : (State Nat (Pair CoC2-Bnd* (Var Uid)))
                      (return-state `(((,u . ,b)) . ,(Var u)))))))
           ((cons b* w) : (Pair CoC2-Bnd* (U (Var Uid) (Quote Cast-Literal)))
            <- (if (or (Var? w) (Quote? w))
                   (return-state (cons b* w))
                   (bind-state
                    (uid-state "write_val")
                    (lambda ([u : Uid])
                      : (State Nat (Pair CoC2-Bnd* (Var Uid)))
                      (return-state
                       `(((,u . ,w) . ,b*) . ,(Var u)))))))
           (if (null? b*)
               (return-state (gbox-set! b w))
               (return-state (Let b* (gbox-set! b w))))]
          [(Gvector size init)
           (size : CoC2-Expr <- (recur size))
           (init : CoC2-Expr <- (recur init))
           (return-state (Unguarded-Vect size init))]
          [(Gvector-ref v i)
           (v : CoC2-Expr <- (recur v))
           (i : CoC2-Expr <- (recur i))
           ((cons b* v) : (Pair CoC2-Bnd* (Var Uid))
            <- (if (Var? v)
                   (return-state `(() . ,v))
                   (bind-state
                    (uid-state "gvect")
                    (lambda ([u : Uid])
                      : (State Nat (Pair CoC2-Bnd* (Var Uid)))
                      (return-state `(((,u . ,v)) . ,(Var u)))))))
           ((cons b* i) : (Pair CoC2-Bnd* (U (Var Uid) (Quote Integer)))
            <- (cond
                 [(Var? i) (return-state `(,b* . ,i))]
                 [(and (Quote? i) (exact-integer? (Quote-literal i)))
                  (return-state `(,b* . ,i))]
                 [else
                  (bind-state
                   (uid-state "index")
                   (lambda ([u : Uid])
                     : (State Nat (Pair CoC2-Bnd* (Var Uid)))
                     (return-state
                      `(((,u . ,i) . ,b*) . ,(Var u)))))]))
           (if (null? b*)
               (return-state (gvect-ref v i))
               (return-state (Let b* (gvect-ref v i))))]
          [(Gvector-set! v i w)
           (v : CoC2-Expr <- (recur v))
           (i : CoC2-Expr <- (recur i))
           (w : CoC2-Expr <- (recur w))
           ((cons b* v) : (Pair CoC2-Bnd* (Var Uid))
            <- (if (Var? v)
                   (return-state `(() . ,v))
                   (bind-state
                    (uid-state "gvect")
                    (lambda ([u : Uid])
                      : (State Nat (Pair CoC2-Bnd* (Var Uid)))
                      (return-state `(((,u . ,v)) . ,(Var u)))))))
           ((cons b* i) : (Pair CoC2-Bnd* (U (Var Uid) (Quote Integer)))
            <- (cond
                 [(Var? i) (return-state `(,b* . ,i))]
                 [(and (Quote? i) (exact-integer? (Quote-literal i)))
                  (return-state `(,b* . ,i))]
                 [else 
                  (bind-state
                   (uid-state "index")
                   (lambda ([u : Uid])
                     : (State Nat (Pair CoC2-Bnd* (Var Uid)))
                     (return-state
                      `(((,u . ,i) . ,b*) . ,(Var u)))))]))
           ((cons b* w) : (Pair CoC2-Bnd* (U (Var Uid) (Quote Cast-Literal)))
            <- (if (or (Var? w) (Quote? w))
                   (return-state (cons b* w))
                   (bind-state
                    (uid-state "write_val")
                    (lambda ([u : Uid])
                      : (State Nat (Pair CoC2-Bnd* (Var Uid)))
                      (return-state
                       `(((,u . ,w) . ,b*) . ,(Var u)))))))
           (if (null? b*)
               (return-state (gvect-set! v i w))
               (return-state (Let b* (gvect-set! v i w))))]
          ;; Boring Recursion Cases --------------------------------
          [(Lambda f* (Castable fn e))
           (e : CoC2-Expr <- (recur e))
           (return-state (Lambda f* (Castable fn e)))]
          [(Letrec b* e)
           (b* : CoC2-Bnd* <- (recur-bnd* b*))
           (e  : CoC2-Expr <- (recur e))
           (return-state (Letrec b* e))]
          [(Let b* e)
           (b* : CoC2-Bnd* <- (recur-bnd* b*))
           (e  : CoC2-Expr <- (recur e))
           (return-state (Let b* e))]
          [(App e e*)
           (e  : CoC2-Expr  <- (recur e))
           (e* : CoC2-Expr* <- (recur* e*))
           (return-state (App e e*))]
          [(Op p e*)
           (e* : CoC2-Expr* <- (recur* e*))
           (return-state (Op p e*))]
          [(Labels bc* e)
           (bc* : CoC2-Bnd-Code* <-
                (map-state (lrc-bnd-code recur) bc*))
           (e   : CoC2-Expr <- (recur e))
           (return-state (Labels bc* e))]
          ;; TODO this should be called App-Fn
          [(App-Fn e e*)
           (e  : CoC2-Expr  <- (recur e))
           (e* : CoC2-Expr* <- (recur* e*))
           (return-state (App-Fn e e*))]
          [(App-Code e e*)
           (e  : CoC2-Expr  <- (recur e))
           (e* : CoC2-Expr* <- (recur* e*))
           (return-state (App-Code e e*))]
          [(App/Fn-Proxy-Huh e e*)
           (e  : CoC2-Expr  <- (recur e))
           (e* : CoC2-Expr* <- (recur* e*))
           (return-state (App/Fn-Proxy-Huh e e*))]
          [(Var i) (return-state (Var i))]
          [(Quote lit) (return-state (Quote lit))]
          [(Quote-Coercion c) (return-state (Quote-Coercion c))]
          [(Type t) (return-state (Type t))]
          [(Code-Label l) (return-state (Code-Label l))]
          [(If t c a)
            (t  : CoC2-Expr  <- (recur t))
            (c  : CoC2-Expr  <- (recur c))
            (a  : CoC2-Expr  <- (recur a))
           (return-state (If t c a))]
          [(Fn-Coercion e* e)
           (e  : CoC2-Expr  <- (recur e))
           (e* : CoC2-Expr* <- (recur* e*))
           (return-state (Fn-Coercion e* e))]
          [(Fn-Coercion-Arg e1 e2)
           (e1  : CoC2-Expr  <- (recur e1))
           (e2  : CoC2-Expr  <- (recur e2))
           (return-state (Fn-Coercion-Arg e1 e2))]
          [(Fn-Coercion-Return e)
           (e  : CoC2-Expr  <- (recur e))
           (return-state (Fn-Coercion-Return e))]
          [(Interpreted-Cast e rep)
           (e  : CoC2-Expr  <- (recur e))
           (match rep
             [(Twosome t1 t2 lbl)
              (t1  : CoC2-Expr  <- (recur t1))
              (t2  : CoC2-Expr  <- (recur t2))
              (lbl  : CoC2-Expr  <- (recur lbl))
              (return-state (Interpreted-Cast e (Twosome t1 t2 lbl)))]
             [(Coercion c)
              (c  : CoC2-Expr  <- (recur c))
              (return-state (Interpreted-Cast e (Coercion c)))]
             [other (error 'lrc-expr/Interpreted-Cast "~a" other)])]
          [(Fn-Caster e)
           (e : CoC2-Expr <- (recur e))
           (return-state (Fn-Caster e))]
          [(Id-Coercion-Huh e)
           (e : CoC2-Expr <- (recur e))
           (return-state (Id-Coercion-Huh e))]
          [(Compose-Coercions e1 e2)
           (e1  : CoC2-Expr  <- (recur e1))
           (e2  : CoC2-Expr  <- (recur e2))
           (return-state (Compose-Coercions e1 e2))]
          [(Fn-Proxy i e1 e2)
           (e1  : CoC2-Expr  <- (recur e1))
           (e2  : CoC2-Expr  <- (recur e2))
           (return-state (Fn-Proxy i e1 e2))]
          [(Fn-Proxy-Huh e)
           (e  : CoC2-Expr  <- (recur e))
           (return-state (Fn-Proxy-Huh e))]
          ;; TODO rename this Fn-Proxy-Function
          [(Fn-Proxy-Closure e)
           (e  : CoC2-Expr  <- (recur e))
           (return-state (Fn-Proxy-Closure e))]
          [(Fn-Proxy-Coercion e)
           (e  : CoC2-Expr  <- (recur e))
           (return-state (Fn-Proxy-Coercion e))]
          [(Begin e* e)
           (e* : CoC2-Expr* <- (recur* e*))
           (e  : CoC2-Expr  <- (recur e))
           (return-state (Begin e* e))]
          [(Repeat i s e b)
           (s  : CoC2-Expr  <- (recur s))
           (e  : CoC2-Expr  <- (recur e))
           (b  : CoC2-Expr  <- (recur b))
           (return-state (Repeat i s e b))]
          [(Type-Fn-arity e)
           (e : CoC2-Expr <- (recur e))
           (return-state (Type-Fn-arity e))]
          [(Type-Fn-return e)
           (e : CoC2-Expr <- (recur e))
           (return-state (Type-Fn-return e))]
          [(Type-Fn-arg t o)
           (t : CoC2-Expr <- (recur t))
           (o : CoC2-Expr <- (recur o))
           (return-state (Type-Fn-arg t o))]
          [(Fn-Caster e)
           (e : CoC2-Expr <- (recur e))
           (return-state (Fn-Caster e))]
          [(Blame e)
           (e : CoC2-Expr <- (recur e))
           (return-state (Blame e))]
          [else (error 'lower-reference-casts "unmatched ~a" else)])))
  ;; recur over a list of expressions
  (: recur* (CoC1-Expr* -> (State Nat CoC2-Expr*)))
  (define (recur* e*) (map-state recur e*))
  ;; recur over bindings
  (: recur-bnd* (CoC1-Bnd* -> (State Nat CoC2-Bnd*)))
  (define (recur-bnd* b*) (map-state recur-bnd b*))
  ;; recur over a single binding
  (: recur-bnd (CoC1-Bnd -> (State Nat CoC2-Bnd)))
  (define (recur-bnd b)
    (do (bind-state : (State Nat CoC2-Bnd))
        (match-let ([(cons i e) b])
          (e : CoC2-Expr <- (recur e))
          (return-state (cons i e)))))
  ;; recur is a recursive closure that results from lower-reference-ops
  recur)
