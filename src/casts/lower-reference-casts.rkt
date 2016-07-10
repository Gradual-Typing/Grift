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

;; inline-guarded-branch
;; Parameter determining if the code generated for gbox-set! and gunbox
;; performs the first check to see if the gref is a unguarded reference
;; or delegates the entire operation to the runtime.
;; This is only used for the twosome representation
(define inline-guarded-branch?
  (make-parameter #t))

;; The entry point for this pass it is called by impose-casting semantics
(: lower-reference-casts (Cast-or-Coerce1-Lang Config -> Cast-or-Coerce2-Lang))
(define (lower-reference-casts prgm config)
  (match-let (((Prog (list name next type) body) prgm))
    (let ([next : (Boxof Nat) (box next)])
      (match-let ([(list gunbox gbox-set! gvect-ref gvect-set! build-runtime)
                   (get-ref-helpers next (Config-cast-rep config))])
        (let* ([e : CoC2-Expr ((lrc-expr next gunbox gbox-set! gvect-ref gvect-set!) body)]
               [next : Nat (unbox next)])
          (Prog (list name next type) (build-runtime e)))))))

;; Functions for use sites of guarded references with coercions
(define-type GunboxT ((Var Uid) -> CoC2-Expr))
(define-type Gbox-setT 
   ((Var Uid) (U (Quote Cast-Literal) (Var Uid)) -> CoC2-Expr))
(define-type Gvect-refT
  ((Var Uid) (U (Quote Integer) (Var Uid)) -> CoC2-Expr))
(define-type Gvect-setT
  ((Var Uid)
   (U (Quote Integer) (Var Uid))
   (U (Quote Cast-Literal) (Var Uid))
   -> CoC2-Expr))

(: gunbox/coercion ((Option Uid) -> GunboxT))
(define ((gunbox/coercion gunbox) gref)
  (cond
    [gunbox (App-Code (Code-Label gunbox) (list gref))]
    [else
     (If (Guarded-Proxy-Huh gref)
         (Interpreted-Cast
          (Unguarded-Box-Ref (Guarded-Proxy-Ref gref))
          (Coercion (Ref-Coercion-Read (Guarded-Proxy-Coercion gref))))
         (Unguarded-Box-Ref gref))]))

(: gbox-set!/coercion ((Option Uid) -> Gbox-setT))
(define ((gbox-set!/coercion gbox-set!) gref val)
  (cond
    [gbox-set! (App-Code (Code-Label gbox-set!) (list gref val))]
    [else
     (If (Guarded-Proxy-Huh gref)
      (Unguarded-Box-Set!
       (Guarded-Proxy-Ref gref)
       (Interpreted-Cast
        val
        (Coercion (Ref-Coercion-Write (Guarded-Proxy-Coercion gref)))))
      (Unguarded-Box-Set! gref val))]))

(: gvect-ref/coercion ((Option Uid) -> Gvect-refT))
(define ((gvect-ref/coercion gvect-ref) gref index)
  (cond
    [gvect-ref (App-Code (Code-Label gvect-ref) (list gref index))]
    [else
     (If (Guarded-Proxy-Huh gref)
         (Interpreted-Cast
          (Unguarded-Vect-Ref (Guarded-Proxy-Ref gref) index)
          (Coercion (Ref-Coercion-Read (Guarded-Proxy-Coercion gref))))
         (Unguarded-Vect-Ref gref index))]))

(: gvect-set!/coercion ((Option Uid) -> Gvect-setT))
(define ((gvect-set!/coercion gvect-set!) gref index val)
  (: index-exp CoC2-Expr)
  (define index-exp (if (integer? index) (Quote index) index))
  (cond
    [gvect-set!
     (App-Code (Code-Label gvect-set!) (list gref index val))]
    [else
     (If (Guarded-Proxy-Huh gref)
         (Unguarded-Vect-Set!
          (Guarded-Proxy-Ref gref)
          index-exp
          (Interpreted-Cast
          val
          (Coercion (Ref-Coercion-Write (Guarded-Proxy-Coercion gref)))))
         (Unguarded-Vect-Set! gref index-exp val))]))

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

(: get-ref-helpers ((Boxof Nat) Cast-Representation
                    -> (List GunboxT Gbox-setT
                             Gvect-refT Gvect-setT
                             (CoC2-Expr -> CoC2-Expr))))
(define (get-ref-helpers next rep)
  ;; create a new unique identifier
  (: next-uid! (String -> Uid))
  (define (next-uid! s)
    (let ([n (unbox next)])
      (set-box! next (+ n 1))
      (Uid s n)))
  ;; body of get-ref-helpers
  (match rep
    ['Twosomes
     ;; Template to Build the GRef Read Runtime Code
     ;; The current implementation recursively removes proxies until
     ;; it finds an unguarded box, reads the value from the box, and
     ;; returns through each proxying stack frame casting the value
     ;; as specified by the previously traversed proxy.
     
     ;; As noted above this definition just allocates the unique
     ;; names and calls off to the call site generator.
     (: gunbox-u Uid)
     (: gunbox-c CoC2-Code)
     (define-values (gunbox-u gunbox-c)
       (let ([gunbox (next-uid! "rt_gunbox")]
             [gref (next-uid! "rt_gref")])
         (parameterize ([inline-guarded-branch? #t])
           (values
            gunbox
            (Code (list gref)
                  ((gunbox/twosome (Code-Label gunbox)) (Var gref)))))))
     ;; A template to build the GRef Write Runtime Code
     ;; The current implementation recursively removes proxies
     ;; casting the value to be written as specified by each traversed
     ;; proxy. When a unguarded proxy is found the value is written.
     ;; The invarient of guarded guarantees that the unguarded box will
     ;; always contain the same type as thera value at this point.
     (: gbox-set-u Uid)
     (: gbox-set-c CoC2-Code)
     (define-values (gbox-set-u gbox-set-c)
       (let ([gbox-set! (next-uid! "rt_gbox_set")]
             [gref (next-uid! "rt_gref")]
             [val (next-uid! "rt_write_val")])
         (parameterize ([inline-guarded-branch? #t])
           (values
            gbox-set!
            (Code (list gref val)
                  ((gbox-set!/twosome (Code-Label gbox-set!))
                   (Var gref) (Var val)))))))

     (: gvec-ref-u Uid)
     (: gvec-ref-c CoC2-Code)
     (define-values (gvec-ref-u gvec-ref-c)
       (let ([gvect-ref (next-uid! "rt_gvect_ref")]
             [vect (next-uid! "rt_gvect")]
             [ind (next-uid!  "rt_index")])
         (parameterize ([inline-guarded-branch? #t])
           (values
            gvect-ref
            (Code (list vect ind)
                  ((gvect-ref/twosome (Code-Label gvect-ref))
                   (Var vect) (Var ind)))))))

     (: gvec-set-u Uid)
     (: gvec-set-c CoC2-Code)
     (define-values (gvec-set-u gvec-set-c)
       (let ([gvect-set! (next-uid! "rt_gvect_set")]
             [vect (next-uid! "rt_gvect")]
             [ind (next-uid! "rt_index")]
             [val (next-uid! "rt_write_val")])
         (parameterize ([inline-guarded-branch? #t])
           (values
            gvect-set!
            (Code (list vect ind val)
                  ((gvect-set!/twosome (Code-Label gvect-set!))
                   (Var vect) (Var ind) (Var val)))))))
     (list
      (gunbox/twosome (Code-Label gunbox-u))
      (gbox-set!/twosome (Code-Label gbox-set-u))
      (gvect-ref/twosome (Code-Label gvec-ref-u))
      (gvect-set!/twosome (Code-Label gvec-set-u))
      (lambda ([e : CoC2-Expr])
        (Labels (list (cons gunbox-u gunbox-c)
                      (cons gbox-set-u gbox-set-c)
                      (cons gvec-ref-u gvec-ref-c)
                      (cons gvec-set-u gvec-set-c))
                e)))

     #;
     (match-let ([(cons gunbox-u gunbox-c) gunbox-template]
                 [(cons gbox-set-u gbox-set-c) gbox-set!-template]
                 [(cons gvec-ref-u gvec-ref-c) gvect-ref-template]
                 [(cons gvec-set-u gvec-set-c) gvect-set!-template])
       )]
    ['Coercions
     (if (inline-guarded-branch?)
         (list (gunbox/coercion #f)
               (gbox-set!/coercion #f)
               (gvect-ref/coercion #f)
               (gvect-set!/coercion #f)
               (lambda ([e : CoC2-Expr]) e))
         (let* ([gunbox-u   (next-uid! "rt_gunbox")]
                [gbox-set-u  (next-uid! "rt_gbox_set")]
                [gvect-ref-u (next-uid! "rt_gvect_ref")]
                [gvect-set-u (next-uid! "rt_gvect_set")]
                [bindings
                 (parameterize ([inline-guarded-branch? #f]) 
                   (list
                    (cons
                     gunbox-u
                     (let ([gbox (next-uid! "gbox")])
                       (Code (list gbox)
                             ((gunbox/coercion #f)
                              (Var gbox)))))
                    (cons
                     gbox-set-u
                     (let ([gbox (next-uid! "gbox")]
                           [val  (next-uid! "val")])
                       (Code (list gbox val)
                             ((gbox-set!/coercion #f)
                              (Var gbox) (Var val)))))
                    (cons
                     gvect-ref-u
                     (let ([vect (next-uid! "vect")]
                           [ind  (next-uid! "ind")])
                       (Code (list vect ind)
                             ((gvect-ref/coercion #f)
                              (Var vect) (Var ind)))))
                    (cons
                     gvect-set-u
                     (let ([vect (next-uid! "vect")]
                           [ind  (next-uid! "ind")]
                           [val  (next-uid! "val")])
                       (Code (list vect ind val)
                             ((gvect-set!/coercion #f)
                              (Var vect) (Var ind) (Var val)))))))])
           (list
            (gunbox/coercion gunbox-u)
            (gbox-set!/coercion gbox-set-u)
            (gvect-ref/coercion gvect-ref-u)
            (gvect-set!/coercion gvect-set-u)
            (lambda ([e : CoC2-Expr])
              (Labels bindings e)))))]
    [other (error 'lrc/get-ref-help "unmatched ~a" other)]))


;; Map over the expression tree lowering reference operations into
;; more explicit operations on the GRef interface described at the
;; top of the file.
(: lrc-expr ((Boxof Nat) GunboxT Gbox-setT Gvect-refT Gvect-setT ->
                     (CoC1-Expr -> CoC2-Expr)))
(define (lrc-expr next gunbox gbox-set! gvect-ref gvect-set!)
  ;; create a new unique identifier
  (: next-uid! (String -> Uid))
  (define (next-uid! s)
    (let ([n (unbox next)])
      (set-box! next (+ n 1))
      (Uid s n)))
  
  ;; Recur through code bindings
  (: lrc-bnd-code (CoC1-Bnd-Code -> CoC2-Bnd-Code))
  (define (lrc-bnd-code b)
    (match b
      [(cons u (Code u* e)) (cons u (Code u* (recur e)))]
      [other (error 'lrc-bnd-code)]))

  (: recur (CoC1-Expr -> CoC2-Expr))
  (define (recur exp)
    (match exp
      ;; Interesting Cases -----------------------------------------------
      ;; Casts from Guarded Reference to Guarded References are represented
      ;; as a data structure proxying the Original Reference.
      [(Cast (app recur e) rep)
       (match rep
         [(Twosome t1 t2 l)
          (match* (t1 t2)
            [((GRef t1) (GRef t2))
             (Guarded-Proxy e (Twosome (Type t1) (Type t2) (Quote l)))]
            [((GVect t1) (GVect t2))
             (Guarded-Proxy e (Twosome (Type t1) (Type t2) (Quote l)))]
            [(_ _) (Cast e (Twosome t1 t2 l))])]
         [(Coercion c)
          (match c
            [(Ref r w)
             (let ([u (next-uid! "ref_coercion")]
                   [r^ (next-uid! "ref_read")]
                   [w^ (next-uid! "ref_write")])
               (match-let ([(cons b* v)
                            (if (Var? e)
                                (cons '() e)
                                (let ([u (next-uid! "guarded-ref")])
                                  (cons (list (cons u e)) (Var u))))])
                 (let ([e (If (Guarded-Proxy-Huh v)
                              (Let (list (cons u (Guarded-Proxy-Coercion v)))
                                   (Let (list (cons r^ (Compose-Coercions
                                                        (Ref-Coercion-Read (Var u))
                                                        (Quote-Coercion r)))
                                              (cons w^ (Compose-Coercions
                                                        (Quote-Coercion w)
                                                        (Ref-Coercion-Write (Var u)))))
                                        (If (If (Id-Coercion-Huh (Var r^))
                                                (Id-Coercion-Huh (Var w^))
                                                (Quote #f))
                                            (Guarded-Proxy-Ref v)
                                            (Guarded-Proxy
                                             (Guarded-Proxy-Ref v)
                                             (Coercion (Ref-Coercion (Var r^) (Var w^)))))))
                              (Guarded-Proxy v (Coercion (Quote-Coercion (Ref r w)))))])
                   (if (null? b*)
                       e
                       (Let b* e)))))]
            [c (Cast e (Coercion c))])])]
      ;; Every Guarded Reference Starts As an Unguarded box
      [(Gbox (app recur e)) (Unguarded-Box e)]
      ;; Unboxing calls off to the helpers we have defined
      [(Gunbox (app recur b))
       (if (Var? b)
           (gunbox b)
           (let ([u (next-uid! "gbox")])
             (Let (list (cons u b)) (gunbox (Var u)))))]
      ;; Setting a Gaurded reference results in iteratively applying
      ;; all the guarding casts to the value to be written.
      ;; Most of the work is already done but the code requires values
      ;; so there is a little repetative to let bind any values that
      ;; haven't been evaluated.
      [(Gbox-set! (app recur b) (app recur w))
       (match-let* ([(cons b* b)
                     (if (Var? b)
                         (cons '() b)
                         (let ([u (next-uid! "gbox")])
                           (cons (list (cons u b)) (Var u))))]
                    [(cons b* w)
                     (if (or (Var? w) (Quote? w))
                         (cons b* w)
                         (let ([u (next-uid! "write_val")])
                           (cons (cons (cons u w) b*) (Var u))))])
         (if (null? b*)
             (gbox-set! b w)
             (Let b* (gbox-set! b w))))]
      [(Gvector (app recur size) (app recur init)) (Unguarded-Vect size init)]
      [(Gvector-ref (app recur v) (app recur i))
       (match-let* ([(cons b* v)
                     (if (Var? v)
                         (cons '() v)
                         (let ([u (next-uid! "gvect")])
                           (cons (list (cons u v)) (Var u))))]
                    [(cons b* i)
                     (cond
                       [(Var? i) (cons b* i)]
                       [(and (Quote? i) (exact-integer? (Quote-literal i)))
                        (cons b* i)]
                       [else
                        (let ([u (next-uid! "index")])
                          (cons (cons (cons u i) b*) (Var u)))])])
         (if (null? b*)
             (gvect-ref v i)
             (Let b* (gvect-ref v i))))]
      [(Gvector-set! (app recur v) (app recur i) (app recur w))
       (match-let* ([(cons b* v)
                     (if (Var? v)
                         (cons '() v)
                         (let ([u (next-uid! "gvect")])
                           (cons (list (cons u v)) (Var u))))]
                    [(cons b* i)
                     (cond
                       [(Var? i) (cons b* i)]
                       [(and (Quote? i) (exact-integer? (Quote-literal i)))
                        (cons b* i)]
                       [else 
                        (let ([u (next-uid! "index")])
                          (cons (cons (cons u i) b*) (Var u)))])]
                    [(cons b* w)
                     (if (or (Var? w) (Quote? w))
                         (cons b* w)
                         (let ([u (next-uid! "write_val")])
                           (cons (cons (cons u w) b*) (Var u))))])
         (if (null? b*)
             (gvect-set! v i w)
             (Let b* (gvect-set! v i w))))]
      ;; Boring Recursion Cases --------------------------------
      [(Lambda f* (Castable fn (app recur e)))
       (Lambda f* (Castable fn e))]
      [(Letrec (app recur-bnd* b*) (app recur e)) (Letrec b* e)]
      [(Let (app recur-bnd* b*) (app recur e)) (Let b* e)]
      [(App (app recur e) (app recur* e*)) (App e e*)]
      [(Op p (app recur* e*)) (Op p e*)]
      [(Labels bc* (app recur e)) (Labels (map lrc-bnd-code bc*) e)]
      [(App-Fn (app recur e) (app recur* e*)) (App-Fn e e*)]
      [(App-Code (app recur e) (app recur* e*)) (App-Code e e*)]
      [(App/Fn-Proxy-Huh (app recur e) (app recur* e*)) (App/Fn-Proxy-Huh e e*)]
      [(Var i) (Var i)]
      [(Quote lit) (Quote lit)]
      [(Quote-Coercion c) (Quote-Coercion c)]
      [(Type t) (Type t)]
      [(Code-Label l) (Code-Label l)]
      [(If (app recur t) (app recur c) (app recur a)) (If t c a)]
      [(Fn-Coercion (app recur* e*) (app recur e)) (Fn-Coercion e* e)]
      [(Fn-Coercion-Arg (app recur e1) (app recur e2)) (Fn-Coercion-Arg e1 e2)]
      [(Fn-Coercion-Return (app recur e)) (Fn-Coercion-Return e)]
      [(Interpreted-Cast (app recur e) rep)
       (match rep
         [(Twosome (app recur t1) (app recur t2) (app recur lbl))
          (Interpreted-Cast e (Twosome t1 t2 lbl))]
         [(Coercion (app recur c)) (Interpreted-Cast e (Coercion c))]
         [other (error 'lrc-expr/Interpreted-Cast "~a" other)])]
      [(Fn-Caster (app recur e)) (Fn-Caster e)]
      [(Id-Coercion-Huh (app recur e)) (Id-Coercion-Huh e)]
      [(Compose-Coercions (app recur e1) (app recur e2))
       (Compose-Coercions e1 e2)]
      [(Fn-Proxy i (app recur e1) (app recur e2)) (Fn-Proxy i e1 e2)]
      [(Fn-Proxy-Huh (app recur e)) (Fn-Proxy-Huh e)]
      ;; TODO rename this Fn-Proxy-Function
      [(Fn-Proxy-Closure (app recur e)) (Fn-Proxy-Closure e)]
      [(Fn-Proxy-Coercion (app recur e)) (Fn-Proxy-Coercion e)]
      [(Begin (app recur* e*) (app recur e)) (Begin e* e)]
      [(Repeat i (app recur s) (app recur e) (app recur b)) (Repeat i s e b)]
      [(Type-Fn-arity (app recur e)) (Type-Fn-arity e)]
      [(Type-Fn-return (app recur e)) (Type-Fn-return e)]
      [(Type-Fn-arg (app recur t) (app recur o)) (Type-Fn-arg t o)]
      [(Fn-Caster (app recur e)) (Fn-Caster e)]
      [(Blame (app recur e)) (Blame e)]
      [(Create-tuple (app recur* e*)) (Create-tuple e*)]
      [(Tuple-proj (app recur e) i) (Tuple-proj e i)]
      [else (error 'lower-reference-casts "unmatched ~a" else)]))
  
  ;; recur over a list of expressions
  (: recur* (CoC1-Expr* -> CoC2-Expr*))
  (define (recur* e*) (map recur e*))
  ;; recur over bindings
  (: recur-bnd* (CoC1-Bnd* -> CoC2-Bnd*))
  (define (recur-bnd* b*) (map recur-bnd b*))
  ;; recur over a single binding
  (: recur-bnd (CoC1-Bnd -> CoC2-Bnd))
  (define (recur-bnd b)
    (match-let ([(cons i e) b])
      (cons i (recur e))))
  
  ;; recur is a recursive closure that results from lower-reference-ops
  recur)
