#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: src/casts/specify-representation
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
Description: This pass exposes the memory layout of aspects of the program.
After this pass language complexity decreases greatly! But all operations are
exposed as the effects that they truelly are. Lets bindings become assignments,
but a static single assignment is implicitly maintained.
+-------------------------------------------------------------------------------+
| Source Grammar : Cast6
| Target Grammar : Data0
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require (for-syntax racket/syntax syntax/parse racket/list
                     "../language/forms.rkt")
         "../errors.rkt"
         (submod "../logging.rkt" typed)
         "../configuration.rkt"
         "../language/data-representation.rkt"
         "../language/cast-or-coerce6.rkt"
         "../language/data0.rkt"
         "../language/syntax.rkt"
         "../unique-counter.rkt")

;; Only the pass is provided by this module
(provide
 (all-from-out
  "../language/cast-or-coerce6.rkt"
  "../language/data0.rkt")
 specify-representation)

(: specify-representation (Cast-or-Coerce6-Lang -> Data0-Lang))
(define (specify-representation prgm)
  (match-define
    (Prog (list name next type) (Let-Static* type-bnd* crcn-bnd* exp))
    prgm)
  (define unique (make-unique-counter next))
  (parameterize ([current-unique-counter unique])
    (define new-exp    : D0-Expr
      (begin
        (set-box! boxed-bnd-code* '())
        (set-box! mk-fn-coercion-code-label? #f)
        (set-box! mk-tuple-coercion-code-label? #f)
        (set-box! comp-fn-coercion-code-label? #f)
        (set-box! comp-tuple-coercion-code-label? #f)
        (set-box! coerce-tuple-code-label? #f)
        (set-box! coerce-tuple-in-place-code-label? #f)
        (set-box! cast-tuple-code-label? #f)
        (set-box! cast-tuple-in-place-code-label? #f)
        (set-box! hashcons-types-code-label? #f)
        (sr-expr (hash) empty-index-map exp)))
    (define init-type* : D0-Expr* (map allocate-bound-type type-bnd*))
    (define type-id*   : Uid*     (map (inst car Uid Any) type-bnd*))
    (define init-crcn* : D0-Expr* (map allocate-bound-coercion crcn-bnd*))
    (define crcn-id*   : Uid*     (map (inst car Uid Any) crcn-bnd*))
    (define new-next (unique-counter-next! unique))
    (define bnd-code*  : D0-Bnd-Code* (unbox boxed-bnd-code*))
    (debug
     (Prog
         (list name new-next type)
       (GlobDecs (append type-id* crcn-id*)
         (Labels bnd-code*
           (Begin (append init-type* init-crcn*) new-exp)))))))

;; Env must be maintained as a mapping from uids to how to access those
;; values. This is important because uid references to variable inside a
;; closure must turn into memory loads.

(define-type IndexMap (Uid Uid -> Nat))

(define boxed-bnd-code* : (Boxof D0-Bnd-Code*) (box '()))

(: add-new-code! (D0-Bnd-Code -> Void))
(define (add-new-code! b)
  (set-box! boxed-bnd-code* (cons b (unbox boxed-bnd-code*))))

(: mk-fn-coercion-code-label? (Boxof (Option (Code-Label Uid))))
(define mk-fn-coercion-code-label? (box #f))

(: comp-fn-coercion-code-label? (Boxof (Option (Code-Label Uid))))
(define comp-fn-coercion-code-label? (box #f))

(: coerce-tuple-code-label? (Boxof (Option (Code-Label Uid))))
(define coerce-tuple-code-label? (box #f))

(: coerce-tuple-in-place-code-label? (Boxof (Option (Code-Label Uid))))
(define coerce-tuple-in-place-code-label? (box #f))

(: cast-tuple-code-label? (Boxof (Option (Code-Label Uid))))
(define cast-tuple-code-label? (box #f))

(: cast-tuple-in-place-code-label? (Boxof (Option (Code-Label Uid))))
(define cast-tuple-in-place-code-label? (box #f))

(: mk-tuple-coercion-code-label? (Boxof (Option (Code-Label Uid))))
(define mk-tuple-coercion-code-label? (box #f))

(: comp-tuple-coercion-code-label? (Boxof (Option (Code-Label Uid))))
(define comp-tuple-coercion-code-label? (box #f))

(: hashcons-types-code-label? (Boxof (Option (Code-Label Uid))))
(define hashcons-types-code-label? (box #f))

;; The way that boxed immediate work currently bothers me.)    
;; Since we have access to unboxed static ints should we just
;; abandon the unboxed dyn integers another a mixture of static
;; allocation and and constant lifting could be used to make all
(: sr-dyn-make ((CoC6-Expr -> D0-Expr) D0-Expr CoC6-Expr -> D0-Expr))
(define (sr-dyn-make sr-expr e1 e2)
  (cond
    [(Type? e2)
     (match e2
       [(Type (Int)) (op$ + (op$ %<< e1 DYN-IMDT-SHIFT) DYN-INT-TAG)]
       [(Type (Bool)) (op$ + (op$ %<< e1 DYN-IMDT-SHIFT) DYN-BOOL-TAG)]
       [(Type (Unit)) (op$ + (op$ %<< e1 DYN-IMDT-SHIFT) DYN-UNIT-TAG)]
       [(Type (Character)) (op$ + (op$ %<< e1 DYN-IMDT-SHIFT) DYN-CHAR-TAG)]
       [else (sr-alloc "dynamic_boxed" DYN-BOXED-TAG
                       `(("value" . ,e1) ("type" . ,(sr-expr e2))))])]
    [else
     (begin$
       (assign$ val e1)
       (assign$ type (sr-expr e2))
       (assign$ tag (sr-get-tag type TYPE-TAG-MASK))
       (Switch
           type
         (let* ([shifted-imm (op$ %<< val DYN-IMDT-SHIFT)]
                [tag-shifted-imm
                 (lambda ([tag : D0-Expr])
                   : D0-Expr
                   (op$ binary-or shifted-imm tag))])
           `([(,data:TYPE-INT-RT-VALUE)  . ,(tag-shifted-imm DYN-INT-TAG)]
             [(,data:TYPE-BOOL-RT-VALUE) . ,(tag-shifted-imm DYN-BOOL-TAG)]
             [(,data:TYPE-UNIT-RT-VALUE) . ,(tag-shifted-imm DYN-UNIT-TAG)]
             [(,data:TYPE-CHAR-RT-VALUE) . ,(tag-shifted-imm DYN-CHAR-TAG)]))
         ;; Notice that float types fall into this case also
         (sr-alloc "dynamic_boxed" DYN-BOXED-TAG `(("" . ,val) ("" . ,type)))))]))

(: tuple-type-glb ((Code-Label Uid) -> ((Var Uid) (Var Uid) -> D0-Expr)))
(define ((tuple-type-glb tglb-label) t1 t2)
  (define-track-next-uid!$ hrt)
  (begin$
    (assign$ t1-count
      (sr-tagged-array-ref t1 TYPE-TUPLE-TAG TYPE-TUPLE-COUNT-INDEX))
    (assign$ t2-count
      (sr-tagged-array-ref t2 TYPE-TUPLE-TAG TYPE-TUPLE-COUNT-INDEX))
    (assign$ smaller-count (If (op$ < t1-count t2-count) t1-count t2-count))
    (assign$ bigger-count (If (op$ > t1-count t2-count) t1-count t2-count))
    (assign$ rt (op$ Alloc (op$ + bigger-count TYPE-TUPLE-ELEMENTS-OFFSET)))
    (assign$ iters (op$ + TYPE-TUPLE-ELEMENTS-OFFSET smaller-count))
    (assign$ tagged-rt (sr-tag-value rt TYPE-TUPLE-TAG))
    (repeat$ (i TYPE-TUPLE-ELEMENTS-OFFSET iters) (_ UNIT-IMDT)
      (assign$ t1a (sr-tagged-array-ref t1 TYPE-TUPLE-TAG i))
      (assign$ t2a (sr-tagged-array-ref t2 TYPE-TUPLE-TAG i))
      (assign$ t-glb (app-code$ tglb-label t1a t2a))
      (sr-array-set! rt i t-glb))
    (cond$
     [(op$ > t1-count t2-count)
      (assign$ iters (op$ + t1-count TYPE-TUPLE-ELEMENTS-OFFSET))
      (assign$ i-init (op$ + t2-count TYPE-TUPLE-ELEMENTS-OFFSET))
      (repeat$ (i i-init iters) (_ UNIT-IMDT)
        (assign$ t1a (sr-tagged-array-ref t1 TYPE-TUPLE-TAG i))
        (sr-array-set! rt i t1a))]
     [(op$ < t1-count t2-count)
      (assign$ iters (op$ + t2-count TYPE-TUPLE-ELEMENTS-OFFSET))
      (assign$ i-init (op$ + t1-count TYPE-TUPLE-ELEMENTS-OFFSET))
      (repeat$ (i i-init iters) (_ UNIT-IMDT)
        (assign$ t2a (sr-tagged-array-ref t1 TYPE-TUPLE-TAG i))
        (sr-array-set! rt i t2a))]
     [else UNIT-IMDT])
    (sr-array-set! rt TYPE-TUPLE-COUNT-INDEX bigger-count)
    (Assign hrt (app-code$ (get-hashcons-types!) tagged-rt))
    (Var hrt)))

(: fn-type-glb ((Code-Label Uid) -> ((Var Uid) (Var Uid) -> D0-Expr)))
(define ((fn-type-glb tglb-label) t1 t2)
  (define-track-next-uid!$ hrt)
  (begin$
    (assign$ arity (sr-tagged-array-ref t1 TYPE-FN-TAG TYPE-FN-ARITY-INDEX))
    (assign$ rt (op$ Alloc (op$ + arity TYPE-FN-FMLS-OFFSET)))
    (assign$ iters (op$ + TYPE-FN-FMLS-OFFSET arity))
    (assign$ tagged-rt (sr-tag-value rt TYPE-FN-TAG))
    (assign$ t1-rt (sr-tagged-array-ref t1 TYPE-FN-TAG TYPE-FN-RETURN-INDEX))
    (assign$ t2-rt (sr-tagged-array-ref t2 TYPE-FN-TAG TYPE-FN-RETURN-INDEX))
    (assign$ t-rt (app-code$ tglb-label t1-rt t2-rt))
    (repeat$ (i TYPE-FN-FMLS-OFFSET iters) (_ UNIT-IMDT)
      (assign$ t1a (sr-tagged-array-ref t1 TYPE-FN-TAG i))
      (assign$ t2a (sr-tagged-array-ref t2 TYPE-FN-TAG i))
      (assign$ t-glb (app-code$ tglb-label t1a t2a))
      (sr-array-set! rt i t-glb))
    (sr-array-set! rt TYPE-FN-ARITY-INDEX arity)
    (sr-array-set! rt TYPE-FN-RETURN-INDEX t-rt)
    (Assign hrt (app-code$ (get-hashcons-types!) tagged-rt))
    (Var hrt)))

(: get-mk-fn-crcn! (Uid -> (Code-Label Uid)))
(define (get-mk-fn-crcn! mk-crcn)
  (: make-code! (Uid -> (Code-Label Uid)))
  (define (make-code! mk-crcn)
    (define-track-next-uid!$ mk-fn-crcn)
    (define mk-fn-crcn-label (Code-Label mk-fn-crcn))
    (define mk-crcn-label (Code-Label mk-crcn))
    (define mk-fn-crcn-c : D0-Code
      (code$ (t1 t2 l)
        (assign$ arity (sr-tagged-array-ref t2 TYPE-FN-TAG TYPE-FN-ARITY-INDEX))
        (assign$ tagged-arity
          (op$ + (op$ %<< arity COERCION-SECOND-TAG-SHIFT)
               COERCION-FN-SECOND-TAG))
        (assign$ t1r (sr-tagged-array-ref t1 TYPE-FN-TAG TYPE-FN-RETURN-INDEX))
        (assign$ t2r (sr-tagged-array-ref t2 TYPE-FN-TAG TYPE-FN-RETURN-INDEX))
        (assign$ cr (app-code$ mk-crcn-label t1r t2r l))
        (assign$ crcn (op$ Alloc (op$ + arity COERCION-FN-FMLS-OFFSET)))
        (assign$ iters (op$ + TYPE-FN-FMLS-OFFSET arity))
        (sr-array-set! crcn COERCION-FN-ARITY-INDEX tagged-arity)
        (sr-array-set! crcn COERCION-FN-RETURN-INDEX cr)
        (repeat$ (i TYPE-FN-FMLS-OFFSET iters) (_ UNIT-IMDT)
          (assign$ t1a (sr-tagged-array-ref t1 TYPE-FN-TAG i))
          (assign$ t2a (sr-tagged-array-ref t2 TYPE-FN-TAG i))
          (assign$ val-crcn (app-code$ mk-crcn-label t2a t1a l))
          (assign$ c-i (op$ + (op$ - i TYPE-FN-FMLS-OFFSET)
                            COERCION-FN-FMLS-OFFSET))
          (sr-array-set! crcn c-i val-crcn))
        (sr-tag-value crcn COERCION-MEDIATING-TAG)))
    (add-new-code! (cons mk-fn-crcn mk-fn-crcn-c))
    (set-box! mk-fn-coercion-code-label? mk-fn-crcn-label)
    mk-fn-crcn-label)
  (let ([cl? (unbox mk-fn-coercion-code-label?)])
    (or cl? (make-code! mk-crcn))))

(: get-comp-fn-crcn! (Uid -> (Code-Label Uid)))
(define (get-comp-fn-crcn! comp-crcn)
  (: make-code! (Uid -> (Code-Label Uid)))
  (define (make-code! comp-crcn)
    (define-track-next-uid!$ comp-fn-crcn)
    (define comp-fn-crcn-label (Code-Label comp-fn-crcn))
    (define comp-crcn-label (Code-Label comp-crcn))
    (define comp-fn-crcn-c : D0-Code
      (code$ (crcn1 crcn2)
        (assign$ tagged-arity
          (sr-tagged-array-ref
           crcn1 COERCION-MEDIATING-TAG COERCION-FN-ARITY-INDEX))
        (assign$ arity (op$ %>> tagged-arity COERCION-SECOND-TAG-SHIFT))
        (assign$ c1r
          (sr-tagged-array-ref
           crcn1 COERCION-MEDIATING-TAG COERCION-FN-RETURN-INDEX))
        (assign$ c2r
          (sr-tagged-array-ref
           crcn2 COERCION-MEDIATING-TAG COERCION-FN-RETURN-INDEX))
        (assign$ cr (app-code$ comp-crcn-label c1r c2r))
        (assign$ new-crcn UNIT-IMDT)
        (assign$ iters (op$ + COERCION-FN-FMLS-OFFSET arity))
        (assign$ id? (sr-check-tag=? cr COERCION-TAG-MASK COERCION-IDENTITY-TAG))
        (repeat$ (i COERCION-FN-FMLS-OFFSET iters) (_ UNIT-IMDT)
          (assign$ c1a (sr-tagged-array-ref crcn1 COERCION-MEDIATING-TAG i))
          (assign$ c2a (sr-tagged-array-ref crcn2 COERCION-MEDIATING-TAG i))
          (assign$ composed-crcn (app-code$ comp-crcn-label c1a c2a))
          (cond$
           [id?
            (assign$ tmp-id?
              (sr-check-tag=?
               composed-crcn COERCION-TAG-MASK COERCION-IDENTITY-TAG))
            (cond$
             [(op$ not tmp-id?)
              (Assign (Var-id new-crcn) (op$ Alloc (op$ + arity COERCION-FN-FMLS-OFFSET)))
              (sr-array-set! new-crcn COERCION-FN-ARITY-INDEX tagged-arity)
              (sr-array-set! new-crcn COERCION-FN-RETURN-INDEX cr)
              (repeat$ (j COERCION-FN-FMLS-OFFSET i) (_ UNIT-IMDT)
                (sr-array-set! new-crcn j COERCION-IDENTITY-IMDT))
              (sr-array-set! new-crcn i composed-crcn)
              (Assign (Var-id id?) FALSE-IMDT)]
             [else UNIT-IMDT])]
           [else (sr-array-set! new-crcn i composed-crcn)]))
        (cond$
         [id? COERCION-IDENTITY-IMDT]
         [else (sr-tag-value new-crcn COERCION-MEDIATING-TAG)])))
    (add-new-code! (cons comp-fn-crcn comp-fn-crcn-c))
    (set-box! comp-fn-coercion-code-label? comp-fn-crcn-label)
    comp-fn-crcn-label)
  (let ([cl? (unbox comp-fn-coercion-code-label?)])
    (or cl? (make-code! comp-crcn))))

(: get-coerce-tuple! (Uid -> (Code-Label Uid)))
(define (get-coerce-tuple! coerce)
  (: make-code! (Uid -> (Code-Label Uid)))
  (define (make-code! coerce)
    (define-track-next-uid!$ coerce-tuple)
    (define coerce-tuple-label (Code-Label coerce-tuple))
    (define coerce-label (Code-Label coerce))
    (define coerce-tuple-c : D0-Code
      (code$ (val crcn)
        (assign$ tagged-count
          (sr-tagged-array-ref
           crcn COERCION-MEDIATING-TAG COERCION-TUPLE-COUNT-INDEX))
        (assign$ count (op$ %>> tagged-count COERCION-SECOND-TAG-SHIFT))
        (assign$ iters (op$ + COERCION-TUPLE-ELEMENTS-OFFSET count))
        (assign$ new-val (op$ Alloc count))
        (repeat$ (i COERCION-TUPLE-ELEMENTS-OFFSET iters) (_ UNIT-IMDT)
          (assign$ val-i (op$ - i COERCION-TUPLE-ELEMENTS-OFFSET))
          (assign$ vala (op$ Array-ref val val-i))
          (assign$ crcna (sr-tagged-array-ref crcn COERCION-MEDIATING-TAG i))
          (assign$ casted-vala (app-code$ coerce-label vala crcna ZERO-IMDT))
          (op$ Array-set! new-val val-i casted-vala))
        new-val))
    (add-new-code! (cons coerce-tuple coerce-tuple-c))
    (set-box! coerce-tuple-code-label? coerce-tuple-label)
    coerce-tuple-label)
  (let ([cl? (unbox coerce-tuple-code-label?)])
    (or cl? (make-code! coerce))))

(: get-coerce-tuple-in-place! (Uid -> (Code-Label Uid)))
(define (get-coerce-tuple-in-place! cast)
  (: make-code! (Uid -> (Code-Label Uid)))
  (define (make-code! cast)
    (define-track-next-uid!$ coerce-tuple-in-place)
    (define cast-label (Code-Label cast))
    (define coerce-tuple-in-place-label (Code-Label coerce-tuple-in-place))
    (define coerce-tuple-in-place-c : D0-Code
      (code$ (tpl-val tpl-crcn mono-addr)
        (begin$
          (assign$ tagged-count
            (sr-tagged-array-ref
             tpl-crcn COERCION-MEDIATING-TAG COERCION-TUPLE-COUNT-INDEX))
          (assign$ count (op$ %>> tagged-count COERCION-SECOND-TAG-SHIFT))
          (repeat$ (i ZERO-IMDT count) (_ UNIT-IMDT)
            (assign$ val (op$ Array-ref tpl-val i))
            (assign$ crcn
              (sr-tagged-array-ref
               tpl-crcn
               COERCION-MEDIATING-TAG
               (sr-plus COERCION-TUPLE-ELEMENTS-OFFSET i)))
            (assign$ rtti1 (op$ Array-ref mono-addr MONO-RTTI-INDEX))
            (assign$ new-val (app-code$ cast-label val crcn mono-addr))
            (assign$ rtti2 (op$ Array-ref mono-addr MONO-RTTI-INDEX))
            (If (op$ = rtti1 rtti2)
                (op$ Array-set! tpl-val i new-val)
                ZERO-IMDT))
          tpl-val)))
    (add-new-code! (cons coerce-tuple-in-place coerce-tuple-in-place-c))
    (set-box! coerce-tuple-in-place-code-label? coerce-tuple-in-place-label)
    coerce-tuple-in-place-label)
  (let ([cl? (unbox coerce-tuple-in-place-code-label?)])
    (or cl? (make-code! cast))))

(: get-cast-tuple-in-place! (Uid -> (Code-Label Uid)))
(define (get-cast-tuple-in-place! cast)
  (: make-code! (Uid -> (Code-Label Uid)))
  (define (make-code! cast)
    (define-track-next-uid!$ cast-tuple-in-place)
    (define cast-tuple-in-place-label (Code-Label cast-tuple-in-place))
    (define cast-label (Code-Label cast))
    (define cast-tuple-in-place-c : D0-Code
      (code$ (tpl-val t1 t2 l mono-addr)
        (begin$
          (assign$ count
            (sr-tagged-array-ref t2 TYPE-TUPLE-TAG TYPE-TUPLE-COUNT-INDEX))
          (repeat$ (i ZERO-IMDT count) (_ UNIT-IMDT)
            (begin$
              (assign$ val (op$ Array-ref tpl-val i))
              (assign$ t1a
                (sr-tagged-array-ref
                 t1 TYPE-TUPLE-TAG (sr-plus TYPE-TUPLE-ELEMENTS-OFFSET i)))
              (assign$ t2a
                (sr-tagged-array-ref
                 t2 TYPE-TUPLE-TAG (sr-plus TYPE-TUPLE-ELEMENTS-OFFSET i)))
              (assign$ rtti1 (op$ Array-ref mono-addr MONO-RTTI-INDEX))
              (assign$ new-val (app-code$ cast-label val t1a t2a l mono-addr))
              (assign$ rtti2 (op$ Array-ref mono-addr MONO-RTTI-INDEX))
              (If (op$ = rtti1 rtti2)
                  (op$ Array-set! tpl-val i new-val)
                  ZERO-IMDT)))
          tpl-val)))
    (add-new-code! (cons cast-tuple-in-place cast-tuple-in-place-c))
    (set-box! cast-tuple-in-place-code-label? cast-tuple-in-place-label)
    cast-tuple-in-place-label)
  (let ([cl? (unbox cast-tuple-in-place-code-label?)])
    (or cl? (make-code! cast))))

(: get-cast-tuple! (Uid -> (Code-Label Uid)))
(define (get-cast-tuple! cast)
  (: make-code! (Uid -> (Code-Label Uid)))
  (define (make-code! cast)
    (define-track-next-uid!$ cast-tuple)
    (define cast-tuple-label (Code-Label cast-tuple))
    (define cast-label (Code-Label cast))
    (define cast-tuple-c : D0-Code
      (code$ (val t1 t2 l)
        (assign$ count
          (sr-tagged-array-ref t2 TYPE-TUPLE-TAG TYPE-TUPLE-COUNT-INDEX))
        (assign$ new-val (op$ Alloc count))
        (assign$ iters (op$ + TYPE-TUPLE-ELEMENTS-OFFSET count))
        (repeat$ (i TYPE-TUPLE-ELEMENTS-OFFSET iters) (_ UNIT-IMDT)
          (assign$ val-i (op$ - i TYPE-TUPLE-ELEMENTS-OFFSET))
          (assign$ vala (op$ Array-ref val val-i))
          (assign$ t1a (sr-tagged-array-ref t1 TYPE-TUPLE-TAG i))
          (assign$ t2a (sr-tagged-array-ref t2 TYPE-TUPLE-TAG i))
          (assign$ casted-vala (app-code$ cast-label vala t1a t2a l ZERO-IMDT))
          (op$ Array-set! new-val val-i casted-vala))
        new-val))
    (add-new-code! (cons cast-tuple cast-tuple-c))
    (set-box! cast-tuple-code-label? cast-tuple-label)
    cast-tuple-label)
  (let ([cl? (unbox cast-tuple-code-label?)])
    (or cl? (make-code! cast))))

(: get-mk-tuple-crcn! (Uid -> (Code-Label Uid)))
(define (get-mk-tuple-crcn! mk-crcn)
  (: make-code! (Uid -> (Code-Label Uid)))
  (define (make-code! mk-crcn)
    (define-track-next-uid!$ mk-tuple-crcn)
    (define mk-tuple-crcn-label (Code-Label mk-tuple-crcn))
    (define mk-crcn-label (Code-Label mk-crcn))
    (define mk-tuple-crcn-c : D0-Code
      ;; mk-tuple-crcn creates a coercion out of two tuple types, it also checks
      ;; if the two types are identical, so that it can generate a simple
      ;; identity coercion without allocating unnecessary tuple coercion of
      ;; identities. It expects the length of the first tuple to be greater than
      ;; or equal to the length of the second.
      (code$ (t1 t2 l)
        (assign$ t1-count
          (sr-tagged-array-ref t1 TYPE-TUPLE-TAG TYPE-TUPLE-COUNT-INDEX))
        (assign$ t2-count
          (sr-tagged-array-ref t2 TYPE-TUPLE-TAG TYPE-TUPLE-COUNT-INDEX))
        (assign$ crcn UNIT-IMDT)
        (assign$ id? TRUE-IMDT)
        (assign$ iters (op$ + TYPE-TUPLE-ELEMENTS-OFFSET t2-count))
        (repeat$ (i TYPE-TUPLE-ELEMENTS-OFFSET iters) (_ UNIT-IMDT)
          (assign$ t1a (sr-tagged-array-ref t1 TYPE-TUPLE-TAG i))
          (assign$ t2a (sr-tagged-array-ref t2 TYPE-TUPLE-TAG i))
          (assign$ val-crcn (app-code$ mk-crcn-label t1a t2a l))
          (cond$
           [id?
            (assign$ tmp-id?
              (sr-check-tag=?
               val-crcn COERCION-TAG-MASK COERCION-IDENTITY-TAG))
            (cond$
             [(op$ not tmp-id?)
              (assign$ tagged-count
                (op$ + (op$ %<< t1-count COERCION-SECOND-TAG-SHIFT)
                     COERCION-TUPLE-SECOND-TAG))
              (assign$ c-i (op$ + (op$ - i TYPE-TUPLE-ELEMENTS-OFFSET)
                                COERCION-TUPLE-ELEMENTS-OFFSET))
              (Assign (Var-id crcn)
                (op$ Alloc (op$ + t1-count COERCION-TUPLE-ELEMENTS-OFFSET)))
              (sr-array-set! crcn COERCION-TUPLE-COUNT-INDEX tagged-count)
              (repeat$ (j COERCION-TUPLE-ELEMENTS-OFFSET c-i) (_ UNIT-IMDT)
                (sr-array-set! crcn j COERCION-IDENTITY-IMDT))
              (sr-array-set! crcn c-i val-crcn)
              (Assign (Var-id id?) FALSE-IMDT)]
             [else UNIT-IMDT])]
           [else
            (assign$ c-i (op$ + (op$ - i TYPE-TUPLE-ELEMENTS-OFFSET)
                              COERCION-TUPLE-ELEMENTS-OFFSET))
            (sr-array-set! crcn c-i val-crcn)]))
        (cond$
         [id? COERCION-IDENTITY-IMDT]
         [else
          (assign$ iters (op$ + t1-count TYPE-TUPLE-ELEMENTS-OFFSET))
          (assign$ i-init (op$ + t2-count TYPE-TUPLE-ELEMENTS-OFFSET))
          (repeat$ (i i-init iters) (_ UNIT-IMDT)
            (assign$ t1a (sr-tagged-array-ref t1 TYPE-TUPLE-TAG i))
            (assign$ c-i (op$ + (op$ - i TYPE-TUPLE-ELEMENTS-OFFSET)
                              COERCION-TUPLE-ELEMENTS-OFFSET))
            (sr-array-set! crcn c-i COERCION-IDENTITY-IMDT))
          (sr-tag-value crcn COERCION-MEDIATING-TAG)])))
    (add-new-code! (cons mk-tuple-crcn mk-tuple-crcn-c))
    (set-box! mk-tuple-coercion-code-label? mk-tuple-crcn-label)
    mk-tuple-crcn-label)
  (let ([cl? (unbox mk-tuple-coercion-code-label?)])
    (or cl? (make-code! mk-crcn))))

(: get-comp-tuple-crcn! (Uid -> (Code-Label Uid)))
(define (get-comp-tuple-crcn! comp-crcn)
  (: make-code! (Uid -> (Code-Label Uid)))
  (define (make-code! comp-crcn)
    (define-track-next-uid!$ comp-tuple-crcn)
    (define comp-tuple-crcn-label (Code-Label comp-tuple-crcn))
    (define comp-crcn-label (Code-Label comp-crcn))
    (define comp-tuple-crcn-c : D0-Code
      ;; comp-tuple-crcn expects to compose two tuple coercions of the same
      ;; length. This might change later when I formalize our tuple semantics.
      ;; It also make sure to return an identity coercion if the two coercions
      ;; are identical.
      (code$ (crcn1 crcn2)
        (assign$ tagged-count
          (sr-tagged-array-ref
           crcn1 COERCION-MEDIATING-TAG COERCION-TUPLE-COUNT-INDEX))
        (assign$ count (op$ %>> tagged-count COERCION-SECOND-TAG-SHIFT))
        (assign$ new-crcn UNIT-IMDT)
        (assign$ iters (op$ + COERCION-TUPLE-ELEMENTS-OFFSET count))
        (assign$ id? TRUE-IMDT)
        (repeat$ (i COERCION-TUPLE-ELEMENTS-OFFSET iters) (_ UNIT-IMDT)
          (assign$ c1a (sr-tagged-array-ref crcn1 COERCION-MEDIATING-TAG i))
          (assign$ c2a (sr-tagged-array-ref crcn2 COERCION-MEDIATING-TAG i))
          (assign$ composed-crcn (app-code$ comp-crcn-label c1a c2a))
          (cond$
           [id?
            (assign$ tmp-id?
              (sr-check-tag=?
               composed-crcn COERCION-TAG-MASK COERCION-IDENTITY-TAG))
            (cond$
             [(op$ not tmp-id?)
              (Assign (Var-id new-crcn)
                (op$ Alloc (op$ + count COERCION-TUPLE-ELEMENTS-OFFSET)))
              (sr-array-set! new-crcn COERCION-TUPLE-COUNT-INDEX tagged-count)
              (repeat$ (j COERCION-TUPLE-ELEMENTS-OFFSET i) (_ UNIT-IMDT)
                (sr-array-set! new-crcn j COERCION-IDENTITY-IMDT))
              (sr-array-set! new-crcn i composed-crcn)
              (Assign (Var-id id?) FALSE-IMDT)]
             [else UNIT-IMDT])]
           [else (sr-array-set! new-crcn i composed-crcn)]))
        (cond$
         [id? COERCION-IDENTITY-IMDT]
         [else (sr-tag-value new-crcn COERCION-MEDIATING-TAG)])))
    (add-new-code! (cons comp-tuple-crcn comp-tuple-crcn-c))
    (set-box! comp-tuple-coercion-code-label? comp-tuple-crcn-label)
    comp-tuple-crcn-label)
  (let ([cl? (unbox comp-tuple-coercion-code-label?)])
    (or cl? (make-code! comp-crcn))))

(: hash-type ((Var Uid) -> D0-Expr))
(define (hash-type ty)
  (define err-msg1
    (Quote "specify-representation/hash-type: switch failure in access"))
  (define err-msg2
    (Quote "specify-representation/hash-type: switch failure in hashing"))
  (: type-hash-access ((Var Uid) -> D0-Expr))
  (define (type-hash-access ty)
    (case$ ty
      ;; The hash value for primitive types are their runtime values.
      [(data:TYPE-DYN-RT-VALUE) TYPE-DYN-RT-VALUE]
      [(data:TYPE-INT-RT-VALUE) TYPE-INT-RT-VALUE]
      [(data:TYPE-BOOL-RT-VALUE) TYPE-BOOL-RT-VALUE]
      [(data:TYPE-UNIT-RT-VALUE) TYPE-UNIT-RT-VALUE]
      [(data:TYPE-FLOAT-RT-VALUE) TYPE-FLOAT-RT-VALUE]
      [(data:TYPE-CHAR-RT-VALUE) TYPE-CHAR-RT-VALUE]
      [else
       (assign$ tag (sr-get-tag ty TYPE-TAG-MASK))
       (case$ tag
         [(data:TYPE-GREF-TAG)
          (sr-tagged-array-ref ty TYPE-GREF-TAG TYPE-GREF-HASH-INDEX)]
         [(data:TYPE-GVECT-TAG)
          (sr-tagged-array-ref ty TYPE-GVECT-TAG TYPE-GVECT-HASH-INDEX)]
         [(data:TYPE-MREF-TAG)
          (sr-tagged-array-ref ty TYPE-MREF-TAG TYPE-MREF-HASH-INDEX)]
         [(data:TYPE-MVECT-TAG)
          (sr-tagged-array-ref ty TYPE-MVECT-TAG TYPE-MVECT-HASH-INDEX)]
         [(data:TYPE-TUPLE-TAG)
          (sr-tagged-array-ref ty TYPE-TUPLE-TAG TYPE-TUPLE-HASH-INDEX)]
         [(data:TYPE-FN-TAG)
          (sr-tagged-array-ref ty TYPE-FN-TAG TYPE-FN-HASH-INDEX)]
         [else (op$ Print err-msg1) (op$ Exit (Quote 1)) UNDEF-IMDT])]))
  (begin$
    (assign$ tag (sr-get-tag ty TYPE-TAG-MASK))
    (case$ tag
      [(data:TYPE-GREF-TAG)
       (assign$ arg-ty
         (sr-tagged-array-ref ty TYPE-GREF-TAG TYPE-GREF-TYPE-INDEX))
       (assign$ arg-type-hash (type-hash-access arg-ty))
       (op$ + (op$ * (Quote 19) arg-type-hash) (Quote 1))]
      [(data:TYPE-GVECT-TAG)
       (assign$ arg-ty
         (sr-tagged-array-ref ty TYPE-GVECT-TAG TYPE-GVECT-TYPE-INDEX))
       (assign$ arg-type-hash (type-hash-access arg-ty))
       (op$ + (op$ * (Quote 19) arg-type-hash) (Quote 2))]
      [(data:TYPE-MREF-TAG)
       (assign$ arg-ty
         (sr-tagged-array-ref ty TYPE-MREF-TAG TYPE-MREF-TYPE-INDEX))
       (assign$ arg-type-hash (type-hash-access arg-ty))
       (op$ + (op$ * (Quote 19) arg-type-hash) (Quote 3))]
      [(data:TYPE-MVECT-TAG)
       (assign$ arg-ty
         (sr-tagged-array-ref ty TYPE-MVECT-TAG TYPE-MVECT-TYPE-INDEX))
       (assign$ arg-type-hash (type-hash-access arg-ty))
       (op$ + (op$ * (Quote 19) arg-type-hash) (Quote 4))]
      [(data:TYPE-FN-TAG)
       (assign$ return-ty
         (sr-tagged-array-ref ty TYPE-FN-TAG TYPE-FN-RETURN-INDEX))
       (assign$ init-hash-code (type-hash-access return-ty))
       (assign$ args-count
         (sr-tagged-array-ref ty TYPE-FN-TAG TYPE-FN-ARITY-INDEX))
       (assign$ iters-count (op$ + TYPE-FN-FMLS-OFFSET args-count))
       (op$ +
         (repeat$
             (i TYPE-FN-FMLS-OFFSET iters-count)
             (hash-code init-hash-code)
           (assign$ arg-type (sr-tagged-array-ref ty TYPE-FN-TAG i))
           (assign$ arg-type-hash (type-hash-access arg-type))
           (op$ * (Quote 19) (op$ + hash-code arg-type-hash)))
         (Quote 5))]
      [(data:TYPE-TUPLE-TAG)
       (assign$ elms-count
         (sr-tagged-array-ref ty TYPE-TUPLE-TAG TYPE-TUPLE-COUNT-INDEX))
       (assign$ iters-count
         (op$ + TYPE-TUPLE-ELEMENTS-OFFSET elms-count))
       (op$ +
         (repeat$
             (i TYPE-TUPLE-ELEMENTS-OFFSET iters-count)
             (hash-code (Quote 0))
           (assign$ elem-type (sr-tagged-array-ref ty TYPE-TUPLE-TAG i))
           (assign$ elem-type-hash (type-hash-access elem-type))
           (op$ * (Quote 19) (op$ + hash-code elem-type-hash)))
         (Quote 6))]
      [else (op$ Print err-msg2) (op$ Exit (Quote 1)) UNDEF-IMDT])))

(: get-hashcons-types! (-> (Code-Label Uid)))
(define (get-hashcons-types!)
  (: make-code! (-> (Code-Label Uid)))
  (define (make-code!)
    (define-track-next-uid!$ hashcons-types)
    (define hashcons-types-label (Code-Label hashcons-types))
    (define err-msg (Quote "specify-representation/hashcons-types: switch failure"))
    (define hashcons-types-c : D0-Code
      (code$ (ty)
        (cond$
         [(op$ <= ty TYPE-MAX-ATOMIC-RT-VALUE) ty]
         [else
          (begin$
            (assign$ hcode (hash-type ty))
            (assign$ hty (op$ Types-hashcons! ty hcode))
            (cond$
             [(op$ = ty hty)
              (begin$
                (assign$ index (op$ Types-gen-index!))
                (assign$ tag (sr-get-tag ty TYPE-TAG-MASK))
                (case$ tag
                  [(data:TYPE-GREF-TAG)
                   (sr-tagged-array-set!
                    hty TYPE-GREF-TAG TYPE-GREF-INDEX-INDEX index)
                   (sr-tagged-array-set!
                    hty TYPE-GREF-TAG TYPE-GREF-HASH-INDEX hcode)]
                  [(data:TYPE-GVECT-TAG)
                   (sr-tagged-array-set!
                    hty TYPE-GVECT-TAG TYPE-GVECT-INDEX-INDEX index)
                   (sr-tagged-array-set!
                    hty TYPE-GVECT-TAG TYPE-GVECT-HASH-INDEX hcode)]
                  [(data:TYPE-MREF-TAG)
                   (sr-tagged-array-set!
                    hty TYPE-MREF-TAG TYPE-MREF-INDEX-INDEX index)
                   (sr-tagged-array-set!
                    hty TYPE-MREF-TAG TYPE-MREF-HASH-INDEX hcode)]
                  [(data:TYPE-MVECT-TAG)
                   (sr-tagged-array-set!
                    hty TYPE-MVECT-TAG TYPE-MVECT-INDEX-INDEX index)
                   (sr-tagged-array-set!
                    hty TYPE-MVECT-TAG TYPE-MVECT-HASH-INDEX hcode)]
                  [(data:TYPE-TUPLE-TAG)
                   (sr-tagged-array-set!
                    hty TYPE-TUPLE-TAG TYPE-TUPLE-INDEX-INDEX index)
                   (sr-tagged-array-set!
                    hty TYPE-TUPLE-TAG TYPE-TUPLE-HASH-INDEX hcode)]
                  [(data:TYPE-FN-TAG)
                   (sr-tagged-array-set!
                    hty TYPE-FN-TAG TYPE-FN-INDEX-INDEX index)
                   (sr-tagged-array-set!
                    hty TYPE-FN-TAG TYPE-FN-HASH-INDEX hcode)]
                  [else (op$ Print err-msg) (op$ Exit (Quote 1)) UNDEF-IMDT])
                hty)]
             [else hty]))])))
    (add-new-code! (cons hashcons-types hashcons-types-c))
    (set-box! hashcons-types-code-label? hashcons-types-label)
    hashcons-types-label)
  (let ([cl? (unbox hashcons-types-code-label?)])
    (or cl? (make-code!))))

(: sr-expr (Env IndexMap CoC6-Expr -> D0-Expr))
(define (sr-expr env cenv exp)

  (: recur-curry-env (Env IndexMap -> (CoC6-Expr -> D0-Expr)))
  (define ((recur-curry-env env cenv) exp)
    (recur/env exp env cenv))

  (: recur/env (CoC6-Expr Env IndexMap -> D0-Expr))
  (define (recur/env exp env cenv)
    (: recur* (CoC6-Expr* -> D0-Expr*))
    (define (recur* e*) (map recur e*))
    (: recur (CoC6-Expr -> D0-Expr))
    (define (recur e)
      (match e
        [(Let (app (sr-bnd* recur) b*) e)
         (let* ([id* (map (inst car Uid Any) b*)]
                [rhs* (map (inst cdr Any D0-Expr) b*)]
                [a* (map (inst Assign Uid D0-Expr) id* rhs*)]
                [env (extend* env id* (map var id*))])
           (Begin a* (recur/env e env cenv)))]
        [(If (app recur t) (app recur c) (app recur a))
         (If t c a)]
        [(Switch e c* d)
         (Switch (recur e) (map-switch-case* recur c*) (recur d))]
        [(Op p (app recur* e*))
         (cond
           [(uil-prim-value? p) (Op p e*)]
           [(uil-prim-effect? p) (Op p e*)]
           [else (error 'specify-representation/Op "~a" p)])]
        [(and (No-Op) nop) nop]
        [(Quote k)
         (cond
           [(null? k)    UNIT-IMDT]
           [(boolean? k) (if k TRUE-IMDT FALSE-IMDT)]
           [(data-literal? k) (Quote k)]
           [else
            (error 'specify-representation/quote "invalid: ~a" k)])]
        ;; Closure Representation
        [(App-Closure (app recur e) (app recur e^) (app recur* e*))
         (App-Code e (cons e^ e*))]
        [(LetP (app (sr-bndp* recur/env) p*) b)
         (let* ([l*  : Uid* (map (inst car Uid D0-Code) p*)]
                [env : Env  (extend* env l* (map label l*))])
           (if (LetC? b)
               (match-let ([(LetC c* e) b])
                 (let* ([u*  : Uid* (map (inst car Uid Any) c*)]
                        [env : Env  (extend* env u* (map var u*))]
                        [recur      (recur-curry-env env cenv)])
                   (let*-values ([(a*) (sr-bndc* recur c*)])                     
                     (Labels p* (Begin a* (recur e)))))) 
               (Labels p* (recur/env b env cenv))))]
        [(Closure-caster (app recur e))
         (op$ Array-ref (op$ binary-and CLOSURE-VALUE-MASK e) CLOS-CSTR-INDEX)]
        [(Closure-code (app recur e))
         (op$ Array-ref (op$ binary-and CLOSURE-VALUE-MASK e) CLOS-CODE-INDEX)]
        [(Closure-ref clos fvar)
         (op$ Array-ref (op$ binary-and (Var clos) CLOSURE-VALUE-MASK)
              (Quote (cenv clos fvar)))]
        [(Var i) (lookup env i)]
        [(Labels (app (sr-bnd-code* recur/env) b*) e)
         (let* ([u* (map (inst car Uid Any) b*)]
                [l* (map label u*)])
           (Labels b* (recur/env e (extend* env u* l*) cenv)))]
        [(App-Code e e*) (App-Code (recur e) (recur* e*))]
        [(Code-Label u) (Code-Label u)]
        ;; Type Representation
        [(Type t) (sr-prim-type t)]
        [(Type-Fn-Huh (app recur e)) (type-fn? e)]
        [(Type-Fn-arity (app recur e)) (type-fn-arity-access e)]
        [(Type-Fn-return (app recur e)) (type-fn-return-access e)]
        [(Type-Fn-arg (app recur e1) (app recur e2)) (type-fn-fmls-access e1 e2)]
        [(Type-GRef-Huh (app recur e)) (type-gref? e)]
        [(Type-GRef-Of (app recur e)) (type-gref-type-access e)]
        [(Type-GVect-Huh (app recur e)) (type-gvect? e)]
        [(Type-GVect-Of (app recur e)) (type-gvect-type-access e)]
        [(Type-Dyn-Huh (app recur e))
         (Op '= (list TYPE-DYN-RT-VALUE e))]
        
        [(Type-Tag (app recur e))
         (Op 'binary-and (list e TYPE-TAG-MASK))]
        ;; Coercions
        ;; Projection Coercions
        [(Quote-Coercion c) (sr-immediate-coercion c)]
        [(Project-Coercion (app recur t) (app recur l))
         (sr-alloc "project_coercion" COERCION-PROJECT-TAG
                   `(("type" . ,t) ("label" . ,l)))]
        [(Project-Coercion-Huh (app recur e))
         (sr-check-tag=? e COERCION-TAG-MASK COERCION-PROJECT-TAG)]
        [(Project-Coercion-Type (app recur e))
         (sr-tagged-array-ref e COERCION-PROJECT-TAG COERCION-PROJECT-TYPE-INDEX)]
        [(Project-Coercion-Label (app recur e))
         (sr-tagged-array-ref e COERCION-PROJECT-TAG COERCION-PROJECT-LABEL-INDEX)]
        ;; Injection Coercions
        [(Inject-Coercion (app recur t))
         (sr-alloc "inject_coercion" COERCION-INJECT-TAG `(("type" . ,t)))]
        [(Inject-Coercion-Huh (app recur e))
         (sr-check-tag=? e COERCION-TAG-MASK COERCION-INJECT-TAG)]
        [(Inject-Coercion-Type (app recur e))
         (sr-tagged-array-ref e COERCION-INJECT-TAG COERCION-INJECT-TYPE-INDEX)]
        ;; Sequence Coercions
        [(Sequence-Coercion (app recur f) (app recur s))
         (sr-alloc "sequence_coercion" COERCION-SEQUENCE-TAG
                   `(("first" . ,f) (,"second" . ,s)))]
        [(Sequence-Coercion-Huh (app recur e))
         (sr-check-tag=? e COERCION-TAG-MASK COERCION-SEQUENCE-TAG)]
        [(Sequence-Coercion-Fst (app recur e))
         (sr-tagged-array-ref e COERCION-SEQUENCE-TAG COERCION-SEQUENCE-FST-INDEX)]
        [(Sequence-Coercion-Snd (app recur e))
         (sr-tagged-array-ref e COERCION-SEQUENCE-TAG COERCION-SEQUENCE-SND-INDEX)]
        ;; Identity Coercions can only be created by coercion quoting
        ;; But  their representation is just (Quote ID-COERCION-TAG)
        [(Id-Coercion-Huh (app recur e)) (build-id-coercion-huh e)]
        [(HC (app recur p?) (app recur t1) (app recur lbl)
             (app recur i?) (app recur t2)
             (app recur m))
         (define tag-expr : D0-Expr
           (match* (p? i?)
             [((Quote (? boolean? p?)) (Quote (? boolean? i?)))
              (define p (if p? data:HC-PRJ-TAG-MASK 0))
              (define i (if i? data:HC-INJ-TAG-MASK 0))
              (Quote (bitwise-ior p i))]
             [(p? i?)
              (begin$
                (assign$ prj-bit (If p? HC-PRJ-TAG-MASK ZERO-IMDT))
                (assign$ inj-bit (If i? HC-INJ-TAG-MASK ZERO-IMDT))
                (op$ binary-or prj-bit inj-bit))])) 
         (sr-alloc "hyper_Coercion" (ann tag-expr D0-Expr)
                   `(("t1" . ,t1)
                     ("label" . ,lbl)
                     ("t2" . ,t2)
                     ("med-coercion" . ,m)))]
        [(HC-Inject-Huh (app recur h)) (check-tag? h HC-INJ-TAG-MASK)]
        [(HC-Project-Huh (app recur h)) (check-tag? h HC-PRJ-TAG-MASK)]
        [(HC-Identity-Huh (app recur h))
         (begin$
           (assign$ hc h)
           (If (check-tag? hc HC-TAG-MASK)
               FALSE-IMDT
               (build-id-coercion-huh (build-hc-med hc))))]
        [(HC-Label (app recur h)) 
         (tagged-array-ref h HC-TAG-MASK HC-LABEL-INDEX)]
        [(HC-T1 (app recur h))
         (tagged-array-ref h HC-TAG-MASK HC-T1-INDEX)]
        [(HC-T2 (app recur h))
         (tagged-array-ref h HC-TAG-MASK HC-T2-INDEX)]
        [(HC-Med (app recur h)) (build-hc-med h)]
        ;; Function Coercions
        [(Fn-Coercion-Huh (app recur e))
         (begin$
           (assign$ tmp-crcn (sr-tagged-array-ref
                              e COERCION-MEDIATING-TAG COERCION-FN-ARITY-INDEX))
           (assign$ crcn-tag (sr-get-tag tmp-crcn COERCION-TAG-MASK))
           (op$ = crcn-tag COERCION-FN-SECOND-TAG))]
        [(Fn-Coercion-Arity (app recur e))
         (begin$
           (assign$ tagged-arity
             (sr-tagged-array-ref
              e COERCION-MEDIATING-TAG COERCION-FN-ARITY-INDEX))
           (op$ %>> tagged-arity COERCION-SECOND-TAG-SHIFT))]
        [(Fn-Coercion-Arg (app recur e) (app recur i))
         (sr-tagged-array-ref
          e COERCION-MEDIATING-TAG (sr-plus COERCION-FN-FMLS-OFFSET i))]
        [(Fn-Coercion-Return (app recur e))
         (sr-tagged-array-ref e COERCION-MEDIATING-TAG COERCION-FN-RETURN-INDEX)]
        ;; TODO either repurpose or get rid of the arity field
        ;; One could immagine that we use it to dynamically dispatch on compose
        [(Fn-Coercion (app recur* e*) (app recur e))
         (begin$
           (assign$ tag_and_arity
             (op$ + (op$ %<< (Quote (length e*)) COERCION-SECOND-TAG-SHIFT)
                  COERCION-FN-SECOND-TAG))
           (sr-alloc "fn_coercion" COERCION-MEDIATING-TAG
                     `(("arity" . ,tag_and_arity)
                       ("return" . ,e) .
                       ,(map (lambda ([e : D0-Expr])
                               (cons "argument" e))
                             e*))))]
        [(Make-Fn-Coercion mk-crcn (app recur t1) (app recur t2) (app recur l))
         (app-code$ (get-mk-fn-crcn! mk-crcn) t1 t2 l)]
        [(Compose-Fn-Coercion comp-crcn (app recur c1) (app recur c2))
         (app-code$ (get-comp-fn-crcn! comp-crcn) c1 c2)]
        [(Id-Fn-Coercion (app recur a))
         (begin$
           (assign$ size a)
           (assign$ fn-c (op$ Alloc (sr-plus size (Quote 2))))
           (assign$ info (op$ + (op$ %<< size COERCION-SECOND-TAG-SHIFT)
                              COERCION-FN-SECOND-TAG))
           (sr-array-set! fn-c COERCION-FN-ARITY-INDEX info)
           (sr-array-set! fn-c COERCION-FN-RETURN-INDEX COERCION-IDENTITY-IMDT)
           (repeat$ (i (Quote 0) size) (_ (Quote 0))
             (sr-array-set!
              fn-c (op$ + i COERCION-FN-FMLS-OFFSET) COERCION-IDENTITY-IMDT))
           (sr-tag-value fn-c COERCION-MEDIATING-TAG))]
        [(Id-Tuple-Coercion (app recur a))
         (begin$
           (assign$ size a)
           (assign$ tup-c (ann (op$ Alloc (sr-plus size (Quote 1))) D0-Expr))
           (assign$ info
             (ann (op$ + (op$ %<< size COERCION-SECOND-TAG-SHIFT)
                       COERCION-TUPLE-SECOND-TAG)
                  D0-Expr))
           (sr-array-set! tup-c COERCION-TUPLE-COUNT-INDEX info)
           (repeat$ (i (Quote 0) size) (_ (Quote 0))
             (sr-array-set!
              tup-c
              (op$ + i COERCION-TUPLE-ELEMENTS-OFFSET)
              COERCION-IDENTITY-IMDT))
           (sr-tag-value tup-c COERCION-MEDIATING-TAG))]
        [(Fn-Coercion-Arg-Set! (app recur f) (app recur i) (app recur a))
         (sr-tagged-array-set!
          f COERCION-MEDIATING-TAG (op$ + i COERCION-FN-FMLS-OFFSET) a)]
        [(Fn-Coercion-Return-Set! (app recur f) (app recur r))
         (sr-tagged-array-set!
          f COERCION-MEDIATING-TAG COERCION-FN-RETURN-INDEX r)]
        [(Tuple-Coercion-Item-Set! (app recur t) (app recur i) (app recur e))
         (sr-tagged-array-set!
          t COERCION-MEDIATING-TAG (op$ + i COERCION-TUPLE-ELEMENTS-OFFSET) e)]
        [(Ref-Coercion-Huh (app recur e))
         (begin$
           (assign$ tmp-crcn
             (sr-tagged-array-ref e COERCION-MEDIATING-TAG COERCION-REF-TAG-INDEX))
           (assign$ crcn-tag (sr-get-tag tmp-crcn COERCION-TAG-MASK))
           (op$ = crcn-tag COERCION-REF-SECOND-TAG))]
        [(Ref-Coercion (app recur r) (app recur w))
         (begin$
           (assign$ second-tag
             (op$ + (op$ %<< ZERO-IMDT COERCION-SECOND-TAG-SHIFT)
                  COERCION-REF-SECOND-TAG))
           (sr-alloc "ref-coercion" COERCION-MEDIATING-TAG
                     `(("tag" . ,second-tag)
                       ("read-coercion" . ,r)
                       ("write-coercion" . ,w))))]        
        [(Ref-Coercion-Read (app recur e))
         (sr-tagged-array-ref e COERCION-MEDIATING-TAG COERCION-REF-READ-INDEX)]
        [(Ref-Coercion-Write (app recur e))
         (sr-tagged-array-ref e COERCION-MEDIATING-TAG COERCION-REF-WRITE-INDEX)]
        [(Failed-Coercion-Huh (app recur e))
         (sr-check-tag=? e COERCION-TAG-MASK COERCION-FAILED-TAG)]
        ;; For now I am allocating the blame label in a box.
        ;; Make this cheaper by ensuring that the string pointer is alligned and
        ;; tagging it.
        [(Failed-Coercion (app recur l))
         (sr-alloc "failed-coercion" COERCION-FAILED-TAG `(("label" . ,l)))]
        [(Failed-Coercion-Label (app recur e))
         (sr-tagged-array-ref e COERCION-FAILED-TAG COERCION-FAILED-LABEL-INDEX)]
        ;; FN-Proxy Stuff
        [(Fn-Proxy i (app recur clos) (app recur crcn))
         (sr-alloc "fn-proxy" FN-PROXY-TAG
                   `(("closure" . ,clos) ("coercion" . ,crcn)))]
        [(Fn-Proxy-Huh (app recur e)) (sr-check-tag=? e FN-TAG-MASK FN-PROXY-TAG)]
        [(Fn-Proxy-Closure (app recur e))
         (sr-tagged-array-ref e FN-PROXY-TAG FN-PROXY-CLOS-INDEX)]
        [(Fn-Proxy-Coercion (app recur e))
         (sr-tagged-array-ref e FN-PROXY-TAG FN-PROXY-CRCN-INDEX)]
        ;; Hybrid Proxy Stuff
        [(Hybrid-Proxy apply (app recur clos) (app recur crcn))
         (sr-alloc "hybrid-proxy" FN-PROXY-TAG
                   `(("apply-hybrid-proxy" . ,(Code-Label apply))
                     ("closure" . ,clos)
                     ("coercion" . ,crcn)))]
        [(Hybrid-Proxy-Huh (app recur e))
         (sr-check-tag=? e FN-TAG-MASK HYBRID-PROXY-TAG)]
        [(Hybrid-Proxy-Closure (app recur e))
         (sr-tagged-array-ref e HYBRID-PROXY-TAG HYBRID-PROXY-CLOS-INDEX)]
        [(Hybrid-Proxy-Coercion (app recur e))
         (sr-tagged-array-ref e HYBRID-PROXY-TAG HYBRID-PROXY-CRCN-INDEX)]
        ;; TODO Fix me TAG should never have been exposed
        [(Tag t) (sr-tag t)]
        ;; Dynamic Values Representation
        [(Construct (Dyn) 'make (list e t)) (sr-dyn-make recur (recur e) t)]
        [(Access (Dyn) 'value (app recur e) #f)
         (begin$
           (assign$ tmp e)
           (assign$ tag (sr-get-tag tmp DYN-TAG-MASK))
           (If (op$ = tag DYN-BOXED-TAG)
               (op$ Array-ref tmp DYN-VALUE-INDEX)
               (op$ %>> tmp DYN-IMDT-SHIFT)))]
        [(Access (Dyn) 'type (app recur e) #f)
         (define err-msg (Quote "specify-representation/Dyn-type: switch failure"))
         (begin$
           (assign$ tmp e)
           (assign$ tag (sr-get-tag tmp DYN-TAG-MASK))
           (Switch tag
             `([(,data:DYN-BOXED-TAG) . ,(op$ Array-ref tmp DYN-TYPE-INDEX)]
               [(,data:DYN-INT-TAG) . ,TYPE-INT-RT-VALUE]
               [(,data:DYN-BOOL-TAG) . ,TYPE-BOOL-RT-VALUE]
               [(,data:DYN-UNIT-TAG) . ,TYPE-UNIT-RT-VALUE]
               [(,data:DYN-CHAR-TAG) . ,TYPE-CHAR-RT-VALUE])
             (begin$ (op$ Print err-msg) (op$ Exit  (Quote 1)) UNDEF-IMDT)))]
        [(Access (Dyn) 'immediate-value (app recur e) #f)
         (op$ %>> e DYN-IMDT-SHIFT)]
        [(Access (Dyn) 'immediate-tag (app recur e) #f)
         (sr-get-tag e DYN-TAG-MASK)]
        [(Access (Dyn) 'box-value (app recur e) #f)
         (op$ Array-ref e DYN-VALUE-INDEX)]
        [(Access (Dyn) 'box-type (app recur e) #f)
         (op$ Array-ref e DYN-TYPE-INDEX)]
        [(Check (Dyn) 'immediate-tag=? (app recur e) `(,t))
         (match t
           [(Type t)
            (define tag
              (match t
                [(Int)  DYN-INT-TAG]
                [(Bool) DYN-BOOL-TAG]
                [(Unit) DYN-UNIT-TAG]
                [(Character) DYN-CHAR-TAG]
                [other  DYN-BOXED-TAG]))
            (op$ = tag (sr-get-tag e DYN-TAG-MASK))]
           [other (error 'dyn-immediate-tag=? "expected type literal: ~a" t)])]
        ;; Observable Results Representation
        [(Blame (app recur e))
         (begin$ (op$ Print e) (op$ Exit (Quote -1)) UNDEF-IMDT)]
        [(Observe (app recur e) t) (sr-observe next-uid! e t)]
        [(and nop (No-Op)) nop]
        ;; References Representation
        [(Begin (app recur* e*) (app recur e)) (Begin e* e)]
        [(Repeat i (app recur e1) (app recur e2) u (app recur e3) e4)
         (Repeat i e1 e2 u e3
           (recur/env e4 (extend (extend env u (Var u)) i (Var i)) cenv))]
        [(Break-Repeat) (Break-Repeat)]
        ;; Guarded
        [(Unguarded-Box (app recur e))
         (sr-alloc "unguarded_box" UGBOX-TAG (list (cons "init_value" e)))]
        [(Unguarded-Box-Ref (app recur e)) (op$ Array-ref e UGBOX-VALUE-INDEX)]
        [(Unguarded-Box-Set! (app recur e1) (app recur e2))
         (op$ Array-set! e1 UGBOX-VALUE-INDEX e2)]
        [(Unguarded-Vect (app recur e1) (app recur e2))
         (begin$
           (assign$ size e1)
           (assign$ init-val e2)
           (assign$ repr-size (op$ + size UGVECT-OFFSET))
           (assign$ vect (op$ Alloc repr-size))
           (op$ Array-set! vect UGVECT-SIZE-INDEX size)
           (repeat$ (i UGVECT-OFFSET repr-size) (_ UNIT-IMDT)
             (op$ Array-set! vect i init-val))
           vect)]
        [(Unguarded-Vect-Ref (app recur e1) (app recur e2))
         (begin$
           (assign$ ind e2)
           (assign$ vect e1)
           ;; TODO This duplicates the error exit code (fix this so it doesn't)
           (if (bounds-checks?)
               (If (op$ >= ind ZERO-IMDT) ;; vectors indices starts from 0
                   (If (op$ < ind (op$ Array-ref vect ZERO-IMDT))
                       (op$ Array-ref vect (op$ + ind UGVECT-OFFSET))
                       (begin$
                         (op$ Printf (Quote "index out of bound %ld\n") ind)
                         (op$ Exit (Quote -1))))
                   (begin$
                     (op$ Printf (Quote "index out of bound %ld\n") ind)
                     (op$ Exit (Quote -1))))
               (op$ Array-ref vect (op$ + ind UGVECT-OFFSET))))]
        [(Unguarded-Vect-Set! (app recur e1) (app recur e2) (app recur e3))
         (begin$
           (assign$ ind e2)
           (assign$ vect e1)
           (if (bounds-checks?)
               (If (op$ >= ind ZERO-IMDT) ;; vectors indices starts from 0
                   (If (op$ < ind (op$ Array-ref vect ZERO-IMDT))
                       (op$ Array-set! vect (op$ + ind UGVECT-OFFSET) e3)
                       (begin$
                         (op$ Printf (Quote "index out of bound %ld\n") ind)
                         (op$ Exit (Quote -1))))
                   (begin$
                     (op$ Printf (Quote "index out of bound %ld\n") ind)
                     (op$ Exit (Quote -1))))
               (op$ Array-set! vect (op$ + ind UGVECT-OFFSET) e3)))]
        [(Guarded-Proxy-Huh (app recur e))
         (op$ = (sr-get-tag e GREP-TAG-MASK) GPROXY-TAG)]
        [(Unguarded-Vect-length (app recur e))
         (op$ Array-ref e UGVECT-SIZE-INDEX)]
        [(Guarded-Proxy (app recur e) r)
         (match r
           [(Twosome (app recur t1) (app recur t2) (app recur l))
            (sr-alloc "proxied-ref" GPROXY-TAG
                      `(("ref" . ,e) ("from" . ,t1) ("to" . ,t2) ("label" . ,l)))]
           [(Coercion (app recur c))
            (sr-alloc "proxied-ref" GPROXY-TAG `(("ref" . ,e) ("crcn" . ,c)))])]
        [(Guarded-Proxy-Ref (app recur e))
         ((untag-deref-gproxy GPROXY-FOR-INDEX) e)]
        [(Guarded-Proxy-Source (app recur e))
         ((untag-deref-gproxy GPROXY-FROM-INDEX) e)]
        [(Guarded-Proxy-Target (app recur e))
         ((untag-deref-gproxy GPROXY-TO-INDEX) e)]
        [(Guarded-Proxy-Blames (app recur e))
         ((untag-deref-gproxy GPROXY-BLAMES-INDEX) e)]
        [(Guarded-Proxy-Coercion (app recur e))
         ((untag-deref-gproxy GPROXY-COERCION-INDEX) e)]
        [(Mbox (app recur e) (app sr-prim-type t))
         (sr-alloc "mbox" MBOX-TAG (list (cons "init_type" t) (cons "init_value" e)))]
        [(Mbox-val-set! (app recur e1) (app recur e2))
         (op$ Array-set! e1 MBOX-VALUE-INDEX e2)]
        [(Mbox-val-ref (app recur e)) (op$ Array-ref e MBOX-VALUE-INDEX)]
        [(Mbox-rtti-set! (app recur addr) (app recur e))
         (op$ Array-set! addr MONO-RTTI-INDEX e)]
        [(Mbox-rtti-ref (app recur addr)) (op$ Array-ref addr MONO-RTTI-INDEX)]
        [(Mvector (app recur e1) (app recur e2) (app sr-prim-type t))
         (begin$
           (assign$ size e1)
           (assign$ init-val e2)
           (assign$ rtti t)
           (assign$ repr-size (op$ + size MVECT-OFFSET))
           (assign$ vect (op$ Alloc repr-size))
           (op$ Array-set! vect MVECT-SIZE-INDEX size)
           (op$ Array-set! vect MONO-RTTI-INDEX rtti)
           (repeat$ (i MVECT-OFFSET repr-size) (_ UNIT-IMDT)
             (op$ Array-set! vect i init-val))
           vect)]
        [(Mvector-length (app recur e)) (op$ Array-ref e MVECT-SIZE-INDEX)]
        [(Mvector-rtti-set! (app recur addr) (app recur e))
         (op$ Array-set! addr MONO-RTTI-INDEX e)]
        [(Mvector-rtti-ref (app recur addr)) (op$ Array-ref addr MONO-RTTI-INDEX)]
        [(Mvector-val-ref (app recur e1) (app recur e2))
         (begin$
           (assign$ ind e2)
           (assign$ mvect e1)
           (if (bounds-checks?)
               (If (op$ >= ind ZERO-IMDT) ;; vectors indices starts from 0
                   (If (op$ < ind (op$ Array-ref mvect MVECT-SIZE-INDEX))
                       (op$ Array-ref mvect (op$ + ind MVECT-OFFSET))
                       (begin$
                         (op$ Printf (Quote "index out of bound %ld\n") ind)
                         (op$ Exit (Quote -1))))
                   (begin$
                     (op$ Printf (Quote "index out of bound %ld\n") ind)
                     (op$ Exit (Quote -1))))
               (op$ Array-ref mvect (op$ + ind MVECT-OFFSET))))]
        [(Mvector-val-set! (app recur e1) (app recur e2) (app recur e3))
         (begin$
           (assign$ ind e2)
           (assign$ mvect e1)
           (if (bounds-checks?)
               (If (op$ >= ind ZERO-IMDT) ;; vectors indices starts from 0
                   (If (op$ < ind (op$ Array-ref mvect MVECT-SIZE-INDEX))
                       (op$ Array-set! mvect (op$ + ind MVECT-OFFSET) e3)
                       (begin$
                         (op$ Printf (Quote "index out of bound %ld\n") ind)
                         (op$ Exit (Quote -1))))
                   (begin$
                     (op$ Printf (Quote "index out of bound %ld\n") ind)
                     (op$ Exit (Quote -1))))
               (begin$
                 (op$ Printf (Quote "index out of bound %ld\n") ind)
                 (op$ Exit (Quote -1)))))]
        [(Type-MVect (app recur e)) (sr-type-mvect e)]
        [(Type-MVect-Huh (app recur e)) (type-mvect? e)]
        [(Type-MVect-Of (app recur e)) (type-mvect-type-access e)]
        [(MVect-Coercion-Huh (app recur e))
         (begin$
           (assign$ tmp-crcn
             (sr-tagged-array-ref
              e COERCION-MEDIATING-TAG COERCION-MVECT-TAG-INDEX))
           (assign$ crcn-tag (sr-get-tag tmp-crcn COERCION-TAG-MASK))
           (op$ = crcn-tag COERCION-MVECT-SECOND-TAG))]
        [(MVect-Coercion-Type (app recur e))
         (sr-tagged-array-ref e COERCION-MEDIATING-TAG COERCION-MVECT-TYPE-INDEX)]
        [(MVect-Coercion (app recur t))
         (begin$
           (assign$ second-tag
             (op$ + (op$ %<< ZERO-IMDT COERCION-SECOND-TAG-SHIFT)
                  COERCION-MVECT-SECOND-TAG))
           (sr-alloc "mvect-coercion" COERCION-MEDIATING-TAG
                     `(("tag" . ,second-tag) ("type" . ,t))))]
        [(MRef-Coercion-Huh (app recur e))
         (begin$
           (assign$ tmp-crcn
             (sr-tagged-array-ref e COERCION-MEDIATING-TAG COERCION-MREF-TAG-INDEX))
           (assign$ crcn-tag (sr-get-tag tmp-crcn COERCION-TAG-MASK))
           (op$ = crcn-tag COERCION-MREF-SECOND-TAG))]
        [(MRef-Coercion-Type (app recur e))
         (sr-tagged-array-ref e COERCION-MEDIATING-TAG COERCION-MREF-TYPE-INDEX)]
        [(MRef-Coercion (app recur t))
         (begin$
           (assign$ second-tag
             (op$ + (op$ %<< ZERO-IMDT COERCION-SECOND-TAG-SHIFT)
                  COERCION-MREF-SECOND-TAG))
           (sr-alloc "ref-coercion" COERCION-MEDIATING-TAG
                     `(("tag" . ,second-tag) ("type" . ,t))))]
        [(Make-GLB-Two-Fn-Types mk-glb (app recur t1) (app recur t2))
         (let ([mk-fn-type-glb (fn-type-glb (Code-Label mk-glb))])
           (cond
             [(and (Var? t1) (Var? t2)) (mk-fn-type-glb t1 t2)]
             [(Var? t1) (begin$ (assign$ a-t2 t2) (mk-fn-type-glb t1 a-t2))]
             [(Var? t2) (begin$ (assign$ a-t1 t1) (mk-fn-type-glb a-t1 t2))]
             [else (begin$
                     (assign$ a-t1 t1)
                     (assign$ a-t2 t2)
                     (mk-fn-type-glb a-t1 a-t2))]))]
        [(Make-GLB-Two-Tuple-Types mk-glb (app recur t1) (app recur t2))
         (let ([mk-tuple-type-glb (tuple-type-glb (Code-Label mk-glb))])
           (cond
             [(and (Var? t1) (Var? t2)) (mk-tuple-type-glb t1 t2)]
             [(Var? t1) (begin$ (assign$ a-t2 t2) (mk-tuple-type-glb t1 a-t2))]
             [(Var? t2) (begin$ (assign$ a-t1 t1) (mk-tuple-type-glb a-t1 t2))]
             [else (begin$
                     (assign$ a-t1 t1)
                     (assign$ a-t2 t2)
                     (mk-tuple-type-glb a-t1 a-t2))]))]
        [(Type-GRef (app recur e)) (sr-type-gref e)]
        [(Type-GVect (app recur e)) (sr-type-gvect e)]
        [(Type-MRef (app recur e)) (sr-type-mref e)]
        [(Type-MRef-Huh (app recur e)) (type-mref? e)]
        [(Type-MRef-Of (app recur e)) (type-mref-type-access e)]
        [(Error (app recur e))
         (begin$ (op$ Print e) (op$ Exit (Quote -1)) UNDEF-IMDT)]
        [(Create-tuple (app recur* e*))
         (sr-alloc "tuple" #f (map (lambda ([e : D0-Expr]) (cons "element" e)) e*))]
        [(Copy-Tuple (app recur n) (app recur v))
         (begin$
           (assign$ new-tpl (op$ Alloc n))
           (repeat$ (i ZERO-IMDT n) (_ UNIT-IMDT)
             (begin$
               (assign$ val (op$ Array-ref v i))
               (op$ Array-set! new-tpl i val)))
           new-tpl)]
        [(Tuple-proj (app recur e) (app recur i)) (op$ Array-ref e i)]
        [(Tuple-Coercion-Huh (app recur e))
         (begin$
           (assign$ tmp-crcn
             (sr-tagged-array-ref
              e COERCION-MEDIATING-TAG COERCION-TUPLE-COUNT-INDEX))
           (assign$ crcn-tag (sr-get-tag tmp-crcn COERCION-TAG-MASK))
           (op$ = crcn-tag COERCION-TUPLE-SECOND-TAG))]
        [(Tuple-Coercion-Num (app recur e))
         (begin$
           (assign$ tagged-count
             (sr-tagged-array-ref
              e COERCION-MEDIATING-TAG COERCION-TUPLE-COUNT-INDEX))
           (op$ %>> tagged-count COERCION-SECOND-TAG-SHIFT))]
        [(Tuple-Coercion-Item (app recur e) (app recur i))
         (sr-tagged-array-ref
          e COERCION-MEDIATING-TAG (sr-plus COERCION-TUPLE-ELEMENTS-OFFSET i))]
        [(Type-Tuple-Huh (app recur e)) (type-tuple? e)]
        [(Type-Tuple-num (app recur e)) (type-tuple-count-access e)]
        [(Type-Tuple-item (app recur e) (app recur i))
         (type-tuple-elements-access e i)]
        [(Coerce-Tuple coerce (app recur v) (app recur c))
         (app-code$ (get-coerce-tuple! coerce) v c)]
        [(Coerce-Tuple-In-Place coerce (app recur v) (app recur c) (app recur mono-type))
         (app-code$ (get-coerce-tuple-in-place! coerce) v c mono-type)]
        [(Cast-Tuple-In-Place
          cast (app recur v) (app recur t1) (app recur t2) (app recur l)
          (app recur mono-address))
         (app-code$ (get-cast-tuple-in-place! cast) v t1 t2 l mono-address)]
        [(Cast-Tuple cast (app recur v) (app recur t1) (app recur t2) (app recur l))
         (app-code$ (get-cast-tuple! cast) v t1 t2 l)]
        [(Make-Tuple-Coercion mk-crcn (app recur t1) (app recur t2) (app recur l))
         (app-code$ (get-mk-tuple-crcn! mk-crcn) t1 t2 l)]
        [(Compose-Tuple-Coercion comp-crcn (app recur c1) (app recur c2))
         (app-code$ (get-comp-tuple-crcn! comp-crcn) c1 c2)]
        [(Mediating-Coercion-Huh (app recur e))
         (sr-check-tag=? e COERCION-TAG-MASK COERCION-MEDIATING-TAG)]
        [other (error 'specify-representation "unmatched ~a" other)]))

    (recur exp))

  (recur/env exp env cenv))

(define (build-id-coercion-huh [e : D0-Expr]) : D0-Expr
  (sr-check-tag=? e COERCION-TAG-MASK COERCION-IDENTITY-TAG))

(define (build-hc-med [e : D0-Expr]) : D0-Expr
  (tagged-array-ref e HC-TAG-MASK HC-MED-INDEX))

;; Allocate without forgetting to lift evaluating subterms first
;; this prevents evaluating terms which may cause more allocation
;; will initializing the values of an allocation
;; it essentially produces expression of the form:
;; (let ((t* e*) ...) (let ((tmp (alloc ...))) (begin (set tmp i t*) ... (binary-or tmp tag))))
;; though it does eliminate any form that it can based on it's input
(: sr-alloc (String (Option D0-Expr) (Listof (Pair String D0-Expr)) -> D0-Expr))
(define (sr-alloc name tag? slots)
  ;; As long as this is used to initialize all the data I have
  ;; a faily stong guarentee that no other allocation could possibly occur.
  (: sr-alloc-init ((Var Uid) -> (Index (Var Uid) -> D0-Expr)))
  (define ((sr-alloc-init mem) offset value)
    (op$ Array-set! mem (Quote offset) value))
  ;; Take a list of variables and expressions paired with their names
  ;; make variables that are bound to the expressions and the bindings
  (: get-assignments/vars ((Listof (Pair String D0-Expr)) -> (Values D0-Expr* (Listof (Var Uid)))))
  (define (get-assignments/vars b*)
    (cond
      [(null? b*) (values '() '())]
      [else
       (match-define (cons (cons n e) d) b*)
       (define-values (a* v*) (get-assignments/vars d))
       (cond
         [(Var? e) (values a* (cons e v*))]
         [else
          (define u (track-next-uid!$ n))
          (values (cons (Assign u e) a*) (cons (Var u) v*))])]))
  (define size (length slots))
  (when (= size 0)
    (error 'specify-representation "Empty objects can not be allocated"))
  (define-values (ass* var*) (get-assignments/vars slots))
  (define ind* (range 0 size))
  (define-track-next-uid!$ alloc-id)
  (define alloc-var (Var alloc-id))
  (define alloc-ass (Assign alloc-id (op$ Alloc (Quote size))))
  (define set* (map (sr-alloc-init alloc-var) ind* var*)) 
  (define tag-return : D0-Expr
    (cond
      [(not tag?) alloc-var]
      [else (sr-tag-value alloc-var tag?)]))
  (Begin (append ass* (cons alloc-ass set*)) tag-return))

(: sr-hashcons-types (String (Option D0-Expr) (Listof (Pair String D0-Expr)) -> D0-Expr))
(define (sr-hashcons-types name tag? slots)
  (: sr-alloc-init ((Var Uid) -> (Nonnegative-Fixnum D0-Expr -> D0-Expr)))
  (define ((sr-alloc-init mem) offset value)
    (op$ Array-set! mem (Quote offset) value))
  (: get-assignments/vars ((Listof (Pair String D0-Expr)) -> (Values D0-Expr* (Listof (Var Uid)))))
  (define (get-assignments/vars b*)
    (cond
      [(null? b*) (values '() '())]
      [else
       (match-define (cons (cons n e) d) b*)
       (define-values (a* v*) (get-assignments/vars d))
       (cond
         [(Var? e) (values a* (cons e v*))]
         [else
          (define u (track-next-uid!$ n))
          (values (cons (Assign u e) a*) (cons (Var u) v*))])]))
  (define size (length slots))
  (when (= size 0)
    (error 'specify-representation "Empty objects can not be allocated"))
  (define-values (ass* var*) (get-assignments/vars slots))
  (define allocation-size (+ 2 size))
  (define ind* (range 2 allocation-size))
  (define-track-next-uid!$ alloc-id)
  (define alloc-var (Var alloc-id))
  (define alloc-ass (Assign alloc-id (op$ Alloc (Quote allocation-size))))
  (define init* (map (sr-alloc-init alloc-var) ind* var*))
  (define tagged-ptr : D0-Expr
    (cond
      [(not tag?) alloc-var]
      [else (sr-tag-value alloc-var tag?)]))
  (Begin `(,alloc-ass ,@ass* ,@init*)
         (app-code$ (get-hashcons-types!) tagged-ptr)))

(: sr-prim-type (Immediate-Type -> D0-Expr))
(define (sr-prim-type t)
  (match t
    [(Int)  TYPE-INT-RT-VALUE]
    [(Bool) TYPE-BOOL-RT-VALUE]
    [(Dyn)  TYPE-DYN-RT-VALUE]
    [(Unit) TYPE-UNIT-RT-VALUE]
    [(Float) TYPE-FLOAT-RT-VALUE]
    [(Character) TYPE-CHAR-RT-VALUE]
    [(Static-Id u) (Var u)]
    [other (error 'specify-representation/primitive-type "unmatched ~a" other)]))

;; define-types-memory-layout-helpers generates constants and functions for
;; creating, accessing, and checking for equality for the specified
;; heap-allocated object.
;; For example:
;; (define-types-memory-layout-helpers "type" "tuple" #b101
;;                               ("count" single) ("elements" many))
;; generates the following for a type called tuple:
;; (define data:TYPE-TUPLE-COUNT-INDEX 0)
;; (define TYPE-TUPLE-COUNT-INDEX data:TYPE-TUPLE-COUNT-INDEX)
;; (define data:TYPE-TUPLE-ELEMENTS-OFFSET 1)
;; (define TYPE-TUPLE-ELEMENTS-OFFSET data:TYPE-TUPLE-ELEMENTS-OFFSET)
;; (define data:TYPE-TUPLE-TAG #b101)
;; (define TYPE-TUPLE-TAG data:TYPE-TUPLE-TAG)
;; (define (sr-type-tuple [tmp1 : D0-Expr] [tmp2 : (Listof D0-Expr)]) : D0-Expr
;;   (sr-alloc "tuple" TYPE-TUPLE-TAG (("count" . tmp1) .
;;                                    (map (lambda ([tmp3 : D0-Expr])
;;                                           (cons "elements" tmp3)) tmp2))))
;; (define (type-tuple? [tmp4 : D0-Expr]) : D0-Expr
;;   (sr-check-tag=? tmp4 TYPE-TAG-MASK TYPE-TUPLE-TAG))
;; (define (type-tuple-count-access [tmp5 : D0-Expr]) : D0-Expr
;;   (sr-tagged-array-ref tmp5 TYPE-TUPLE-TAG TYPE-TUPLE-COUNT-INDEX))
;; (define (type-tuple-elements-access [tmp6 : D0-Expr] [tmp7 : D0-Expr]) : D0-Expr
;;   (sr-tagged-array-ref tmp6 TYPE-TUPLE-TAG
;;                        (match tmp7
;;                         [(Quote (? fixnum? k))
;;                           (Quote (+ data:TYPE-TUPLE-ELEMENTS-OFFSET k))]
;;                         [otherwise
;;                           (Op '+ (list TYPE-TUPLE-ELEMENTS-OFFSET tmp7))])))
(define-syntax (define-types-memory-layout-helpers stx)
  (syntax-parse stx
    #:datum-literals (single many)
    [(_ namespace:str name:str tag:exact-integer
        (field*:str (~optional cval* #:defaults ([cval* #'#f])) (~literal single)) ...
        (~optional (many-field:str many) #:defaults ([many-field #'#f])))
     (define namespace-string (syntax->datum #'namespace))
     (define namespace-string-caps (string-upcase namespace-string))
     (define name-string (syntax->datum #'name))
     (define qualified-upcase-name
       (format "~a-~a" namespace-string-caps (string-upcase name-string)))
     (define ((gen-index/offset-id postfix) field)
       (format-id stx (string-append "~a-~a-" postfix)
                  qualified-upcase-name (string-upcase field)))
     (define (gen-data-id x) (format-id stx "data:~a" x))
     (define many-field-dtm (syntax->datum #'many-field))
     (define field-string* (map syntax->datum (syntax->list #'(field* ...))))
     (define index-def* (map (gen-index/offset-id "INDEX") field-string*))
     (define offset-def
       (if many-field-dtm ((gen-index/offset-id "OFFSET") many-field-dtm) #f))
     (define index-data-def* (map gen-data-id index-def*))
     (define offset-data-def (if many-field-dtm (gen-data-id offset-def) #f))
     (define-values (field-val* temp*)
       (for/fold ([p* '()] [t* '()]) ([c (syntax->list #'(cval* ...))])
         (let ([cdtm (syntax->datum c)])
           (if cdtm
               (values (cons (Quote cdtm) p*) t*)
               (with-syntax ([a (generate-temporary)])
                 (values (cons #'a p*) (cons #'a t*)))))))
     (define/with-syntax func-alloc-name
       (format-id stx "sr-~a-~a" namespace-string name-string))
     (define/with-syntax func-huh-name
       (format-id stx "~a-~a?" namespace-string name-string))
     (define/with-syntax namespace-mask-def
       (format-id stx "~a-TAG-MASK" namespace-string-caps))
     (define/with-syntax index-def
       (format-id stx "~a-INDEX-INDEX" qualified-upcase-name))
     (define/with-syntax hash-def
       (format-id stx "~a-HASH-INDEX" qualified-upcase-name))
     (define/with-syntax tag-def (format-id stx "~a-TAG" qualified-upcase-name))
     (define/with-syntax (sindex/offset-def* ...)
       (if many-field-dtm (append index-def* (list offset-def)) index-def*))
     (define/with-syntax (sindex/offset-data-def* ...)
       (if many-field-dtm
           (append index-data-def* (list offset-data-def))
           index-data-def*))
     (define/with-syntax tag-data-def (gen-data-id #'tag-def))
     (define/with-syntax index-data-def (gen-data-id #'index-def))
     (define/with-syntax hash-data-def (gen-data-id #'hash-def))
     (define/with-syntax (sindex/offset-val* ...)
       (let ([n (+ 2 (length field-string*))])
         (datum->syntax stx (range 2 (if many-field-dtm (add1 n) n)))))
     (define/with-syntax (alloc-arg* ...) temp*)
     (define/with-syntax (alloc-val* ...) field-val*)
     (define/with-syntax func-alloc
       (if many-field-dtm
           (with-syntax ([arg (generate-temporary)]
                         [last-alloc-arg (generate-temporary)])
             #`(define (func-alloc-name
                        [alloc-arg* : D0-Expr] ...
                        [last-alloc-arg : (Listof D0-Expr)])
                 : D0-Expr
                 (sr-hashcons-types
                  #,name-string tag-def
                  `((field*
                     . ,alloc-val*) ... .
                    ,(map (lambda ([arg : D0-Expr]) (cons many-field arg))
                          last-alloc-arg)))))
           #`(define (func-alloc-name [alloc-arg* : D0-Expr] ...) : D0-Expr
               (sr-hashcons-types #,name-string tag-def `((field* . ,alloc-val*) ...)))))
     (define/with-syntax equal-arg (generate-temporary))
     (define (gen-func-access*)
       (define (gen-access-func-name field-string)
         (format-id stx "~a-~a-~a-access"
                    namespace-string name-string field-string))
       (define (gen-index-func-access field-string index-def)
         (define/with-syntax func-name (gen-access-func-name field-string))
         (define/with-syntax access-arg (generate-temporary))
         #`(define (func-name [access-arg : D0-Expr]) : D0-Expr
             (sr-tagged-array-ref access-arg tag-def #,index-def)))
       (define (gen-offset-func-access)
         (define/with-syntax func-name (gen-access-func-name many-field-dtm))
         (define/with-syntax access-arg (generate-temporary))
         (define/with-syntax ind-arg (generate-temporary))
         #`(define (func-name [access-arg : D0-Expr] [ind-arg : D0-Expr]) : D0-Expr
             (sr-tagged-array-ref
              access-arg tag-def
              (match ind-arg
                [(Quote (? fixnum? k)) (Quote (+ #,offset-data-def k))]
                [otherwise (Op '+ (list #,offset-def ind-arg))]))))
       (let ([l (map gen-index-func-access field-string* index-def*)])
         (if many-field-dtm (append l (list (gen-offset-func-access))) l)))
     #`(begin
         (define tag-data-def tag)
         (define tag-def (Quote tag-data-def))
         (define index-data-def 0)
         (define index-def (Quote index-data-def))
         (define hash-data-def 1)
         (define hash-def (Quote hash-data-def))
         (define sindex/offset-data-def* sindex/offset-val*) ...
         (define sindex/offset-def* (Quote sindex/offset-data-def*)) ...
         func-alloc
         (define (func-huh-name [equal-arg : D0-Expr]) : D0-Expr
           (sr-check-tag=? equal-arg namespace-mask-def tag-def))
         #,@(gen-func-access*))]))

(define-types-memory-layout-helpers "type" "gref" #b001 ("type" single))
(define-types-memory-layout-helpers "type" "gvect" #b010 ("type" single))
(define-types-memory-layout-helpers "type" "mref" #b011 ("type" single))
(define-types-memory-layout-helpers "type" "mvect" #b100 ("type" single))
(define-types-memory-layout-helpers "type" "fn" #b000
  ("arity" single) ("return" single) ("fmls" many))
(define-types-memory-layout-helpers "type" "tuple" #b101
  ("count" single) ("elements" many))

(: allocate-bound-type (CoC6-Bnd-Type -> D0-Expr))
(define (allocate-bound-type bnd)
  (: sr-type (Compact-Type -> D0-Expr))
  (define (sr-type t)
    (match t
      [(GRef t) (sr-type-gref (sr-prim-type t))]
      [(GVect t) (sr-type-gvect (sr-prim-type t))]
      [(MRef t) (sr-type-mref (sr-prim-type t))]
      [(MVect t) (sr-type-mvect (sr-prim-type t))]
      [(Fn a f* r)
       (sr-type-fn (Quote a) (sr-prim-type r) (map sr-prim-type f*))]
      [(STuple n a*) (sr-type-tuple (Quote n) (map sr-prim-type a*))]
      [other (error 'specify-representation/type "unmatched ~a" other)]))
  (match-let ([(cons u t) bnd])
    (Assign u (sr-type t))))

(: sr-immediate-coercion (Immediate-Coercion -> D0-Expr))
(define (sr-immediate-coercion c)
  (match c
    [(Identity) COERCION-IDENTITY-IMDT]
    [(Static-Id id) (Var id)]
    [else (error 'sr-immediate-coercion "unhandled case in match")]))

(: sr-coercion (Compact-Coercion -> D0-Expr))
(define (sr-coercion t)
  (match t
    [(Identity) COERCION-IDENTITY-IMDT]
    [(Project t l)
     ;; TODO Make it possible to turn off type hoisting
     (define t^ (sr-prim-type t))
     (sr-alloc "project_coercion" COERCION-PROJECT-TAG
               `(("type" . ,t^) ("label" . ,(Quote l))))]
    [(Inject (app sr-prim-type t))
     (sr-alloc "inject-coercion" COERCION-INJECT-TAG `(("type" . ,t)))]
    [(HC p? (app sr-prim-type t1) l? 
         i? (app sr-prim-type t2)
         (app sr-immediate-coercion m))
     (define p (if p? data:HC-PRJ-TAG-MASK 0))
     (define i (if i? data:HC-INJ-TAG-MASK 0))
     (define t (bitwise-ior p i))
     (debug 'specify-representation/hc-hoisted
            p? p i? i t
            (sr-alloc "hyper_coercion" (Quote t)
                      `(("t1"    . ,t1)
                        ("label" . ,(if l? (Quote l?) FALSE-IMDT))
                        ("t2"    . ,t2)
                        ("Med-coercion" . ,m))))]
    [(Sequence (app sr-immediate-coercion f) (app sr-immediate-coercion s))
     (sr-alloc "sequence_coecion" COERCION-SEQUENCE-TAG
               `(("first" . ,f) (,"second" . ,s)))]
    [(Fn l a* (app sr-immediate-coercion r))
     (define len : Index (length a*))
     (unless (= l len)
       (error 'sr-coercion "length mismatch"))
     (begin$
       (assign$ second-tag (op$ + (op$ %<< (Quote l) COERCION-SECOND-TAG-SHIFT)
                                COERCION-FN-SECOND-TAG))
       (sr-alloc "fn_coercion" COERCION-MEDIATING-TAG
                 `(("arity"  . ,second-tag)
                   ("return" . ,r) .
                   ,(map (lambda ([a : Immediate-Coercion])
                           (cons "argument" (sr-immediate-coercion a)))
                         a*))))]
    [(Ref (app sr-immediate-coercion r) (app sr-immediate-coercion w))
     (begin$
       (assign$ second-tag (op$ + (op$ %<< ZERO-IMDT COERCION-SECOND-TAG-SHIFT)
                                COERCION-REF-SECOND-TAG))
       (sr-alloc "ref-coercion" COERCION-MEDIATING-TAG
                 `(("tag" . ,second-tag)
                   ("read-coercion" . ,r)
                   ("write-coercion" . ,w))))]
    [(MonoRef (app sr-prim-type t))
     (begin$
       (assign$ second-tag (op$ + (op$ %<< ZERO-IMDT COERCION-SECOND-TAG-SHIFT)
                                COERCION-MREF-SECOND-TAG))
       (sr-alloc "mref-coercion" COERCION-MEDIATING-TAG
                 `(("tag" . ,second-tag) ("type" . ,t))))]
    [(MonoVect (app sr-prim-type t))
     (begin$
       (assign$ second-tag (op$ + (op$ %<< ZERO-IMDT COERCION-SECOND-TAG-SHIFT)
                                COERCION-MVECT-SECOND-TAG))
       (sr-alloc "mvect-coercion" COERCION-MEDIATING-TAG
                 `(("tag" . ,second-tag) ("type" . ,t))))]
    [(CTuple l a*)
     (define len : Index (length a*))
     (unless (= l len)
       (error 'sr-coercion "length mismatch"))
     (begin$
       (assign$ second-tag
         (op$ + (op$ %<< (Quote (length a*)) COERCION-SECOND-TAG-SHIFT)
              COERCION-TUPLE-SECOND-TAG))
       (sr-alloc "tuple_coercion" COERCION-MEDIATING-TAG
                 `(("num"  . ,second-tag)
                   .
                   ,(map (lambda ([a : Immediate-Coercion])
                           (cons "item" (sr-immediate-coercion a)))
                         a*))))]
    [(Failed l)
     (sr-alloc "failed-coercion" COERCION-FAILED-TAG `(("label" . ,(Quote l))))]
    [other (error 'specify-representation/type "unmatched ~a" other)]))

(: allocate-bound-coercion : CoC6-Bnd-Crcn -> D0-Expr)
(define (allocate-bound-coercion bnd)
  (match-define (cons u c) bnd) (Assign u (sr-coercion c)))

(: untag-deref-gproxy (-> D0-Expr (-> D0-Expr D0-Expr)))
(define ((untag-deref-gproxy index) proxy)
  (op$ Array-ref (op$ binary-xor proxy GPROXY-TAG) index))

;; fold map through bindings
(: sr-bnd* ((CoC6-Expr -> D0-Expr) -> (CoC6-Bnd-Data* -> D0-Bnd*)))
(define ((sr-bnd* sr-expr) b*)
  (: sr-bnd (CoC6-Bnd-Data -> D0-Bnd))
  (define (sr-bnd b)
    (cons (car b) (sr-expr (cdr b))))
  (map sr-bnd b*))

(: sr-bnd-code* ((CoC6-Expr Env IndexMap -> D0-Expr)
                 -> (CoC6-Bnd-Code* -> D0-Bnd-Code*)))
(define ((sr-bnd-code* sr-expr/env) b*)
  (: sr-bnd (CoC6-Bnd-Code -> D0-Bnd-Code))
  (define (sr-bnd b)
    (match-let ([(cons u (Code u* e)) b])
      (let ([env (extend* (hash) u* (map var u*))])
        (cons u (Code u* (sr-expr/env e env empty-index-map))))))
  (map sr-bnd b*))

(: sr-observe ((String -> Uid) D0-Expr Schml-Type -> D0-Expr))
(define (sr-observe next-uid! e t)
  (: generate-print ((Var Uid) Schml-Type -> D0-Expr))
  (define (generate-print id ty)
    (cond
      [(Int? t) (op$ Printf (Quote "Int : %ld\n") id)]
      ;; This is a fragile hack
      ;; switch to typed ASTs
      [(Float? t) (begin$ (op$ Print (Quote "Float : "))
                          (op$ print-float id (Quote 9))
                          (op$ Print (Quote "\n")))]
      [(Character? t)
       (begin$ (op$ Print (Quote "Char : "))
               (op$ print-char id)
               (op$ Print (Quote "\n")))]
      [(Bool? t)
       (If id
           (op$ Print (Quote "Bool : #t\n"))
           (op$ Print (Quote "Bool : #f\n")))]
      [(Unit? t) (op$ Print (Quote "Unit : ()\n"))]
      [(Fn? t) (op$ Print (Quote "Function : ?\n"))]
      [(GRef? t) (op$ Print (Quote "GReference : ?\n"))]
      [(GVect? t) (op$ Print (Quote "GVector : ?\n"))]
      [(MRef? t) (op$ Print (Quote "MReference : ?\n"))]
      [(MVect? t) (op$ Print (Quote "MVector : ?\n"))]
      [(STuple? t) (op$ Print (Quote "Tuple : ?\n"))]
      [(Dyn? t) (op$ Print (Quote "Dynamic : ?\n"))]
      [else (error 'sr-observe "printing other things")]))
  (begin$ (assign$ res e) (generate-print res t)))

#;(TODO GET RID OF TAGS IN THE COMPILER)
(: sr-tag (Tag-Symbol -> (Quote Integer)))
(define (sr-tag t)
  (case t
    [(Int)    DYN-INT-TAG]
    [(Char)   DYN-CHAR-TAG]
    [(Bool)   DYN-BOOL-TAG]
    [(Unit)   DYN-UNIT-TAG]
    [(Atomic) TYPE-ATOMIC-TAG]
    [(Fn)     TYPE-FN-TAG]
    [(GRef)   TYPE-GREF-TAG]
    [(GVect)  TYPE-GVECT-TAG]
    [(Boxed)  DYN-BOXED-TAG]
    [(MRef)   TYPE-MREF-TAG]
    [(MVect)  TYPE-MVECT-TAG]
    [(STuple) TYPE-TUPLE-TAG]
    [else (error 'sr-tag "invalid: ~a" t)]))

(: sr-bndp* ((CoC6-Expr Env IndexMap -> D0-Expr)
             -> (CoC6-Bnd-Procedure* -> D0-Bnd-Code*)))
(define ((sr-bndp* sr-expr) b*)
  (: sr-bndp (CoC6-Bnd-Procedure -> D0-Bnd-Code))
  (define (sr-bndp bnd)
    (match-let ([(cons u (Procedure cp param* code ctr? fvar* exp)) bnd])
      (let* ([offset (if ctr? 2 1)]
             [closv  (Var cp)]
             [env (for/hash : Env ([fvar fvar*]
                                   [i (in-naturals offset)])
                    (values fvar (op$ Array-ref closv (Quote i))))]
             [env (extend* env param* (map var param*))]
             [cenv (index-closure offset cp fvar*)])
        (cons u (Code (cons cp param*) (sr-expr exp env cenv))))))
  (map sr-bndp b*))

(: index-closure (Nat Uid Uid* -> IndexMap))
(define (index-closure offset clos fvar*)
  (define ((fvar-err f))
    (error 'specifiy-representation "failed to index free var ~a from clos ~a"
           f clos))
  (define (clos-err c f)
    (error 'specify-representation
           "unbound closure index ~a from closure ~a inside of clos ~a"
           f c clos))
  (let ([map (for/hash : (HashTable Uid Nat)
                 ([fvar : Uid fvar*]
                  [i : Nat (in-naturals offset)])
               (values fvar i))])
    (lambda ([c : Uid] [f : Uid])
      : Nat
      (if (uid=? c clos)
          (hash-ref map f (fvar-err f))
          (clos-err c f)))))

(: sr-bndc* ((CoC6-Expr -> D0-Expr) CoC6-Bnd-Closure* -> D0-Expr*))
(define (sr-bndc* sr-expr b*)
  (: sr-bndc (CoC6-Bnd-Closure -> (Pair Uid (Pair D0-Expr D0-Expr*))))
  (define (sr-bndc bnd)
    (match-let ([(cons uid (Closure-Data lbl ctr? free*)) bnd])
      (let* ([lbl   (sr-expr lbl)]
             [free* (map sr-expr free*)]
             [data  (cons lbl (if ctr? (cons (sr-expr ctr?) free*) free*))]
             [size  (length data)]
             [clos  (Var uid)]
             [rhs  (op$ Alloc (Quote size))]
             [set*  (for/list : (Listof D0-Expr)
                        ([d : D0-Expr data]
                         [i : Integer (in-naturals)])
                      (op$ Array-set! clos (Quote i) d))])
        (cons uid (ann (cons rhs set*) (Pair D0-Expr D0-Expr*))))))
  (let* ([u.r.e*  (map sr-bndc b*)]
         [u*      (map (inst car Uid Any) u.r.e*)]
         [r.e*    (map (inst cdr Uid (Pair D0-Expr D0-Expr*)) u.r.e*)]
         [r*      (map (inst car D0-Expr D0-Expr*) r.e*)]
         [a*      (map (inst Assign Uid D0-Expr) u* r*)]
         [e*      (append-map (inst cdr D0-Expr D0-Expr*) r.e*)])
    ;; This code implies that the tag of a closure is #b000
    (append a* e*)))

(: sr-clos-ref-code (-> D0-Expr D0-Expr))
(define (sr-clos-ref-code clos) (op$ Array-ref  clos CLOS-CODE-INDEX))

(: sr-clos-ref-caster (-> D0-Expr D0-Expr))
(define (sr-clos-ref-caster clos) (op$ Array-ref clos CLOS-CSTR-INDEX))

(define-type Env (HashTable Uid D0-Expr))

(define-syntax-rule (extend e k v) (hash-set e k v))

(: extend* (-> Env Uid* D0-Expr* Env))
(define (extend* env id* exp*)
  (match* (id* exp*)
    [('() _) env]
    [(_ '()) env]
    [((cons id id*) (cons exp exp*))
     (extend* (extend env id exp) id* exp*)]))

;; The variable is a normal variable
(: var  (-> Uid D0-Expr))
(define (var id) (Var id))

(: label (-> Uid D0-Expr))
(define (label id) (Code-Label id))

(: lookup (-> Env Uid D0-Expr))
(define (lookup e u) (hash-ref e u (lookup-error e u)))

(define (lookup-error e u)
  (lambda ()
    (error 'specify-representation/lookup
           "Unbound uid ~a\n\tin program with env ~a" u e)))

(define-type Triv (U (Quote String) (Quote Integer) (Code-Label Uid) (Var Uid)))
(define-predicate triv? Triv)

(: rename (-> String (-> Any String)))
(define (rename name) (lambda (_) name))

(define sr-quote : (D0-Literal -> D0-Expr) Quote)

(define (empty-index-map u i)
  (error 'specify-representation "attempt to index without index map ~a ~a" u i))

(: sr-tagged-array-ref (D0-Expr D0-Expr D0-Expr -> D0-Expr))
(define (sr-tagged-array-ref e t i) (sr-array-ref (sr-untag e t) i))

(: sr-tagged-array-set! (D0-Expr D0-Expr D0-Expr D0-Expr -> D0-Expr))
(define (sr-tagged-array-set! e t i v) (sr-array-set! (sr-untag e t) i v))

(: sr-array-ref (D0-Expr D0-Expr -> D0-Expr))
(define (sr-array-ref e i) (op$ Array-ref e i))

(: sr-array-set! (D0-Expr D0-Expr D0-Expr -> D0-Expr))
(define (sr-array-set! e i v) (op$ Array-set! e i v))

(: sr-untag (D0-Expr D0-Expr -> D0-Expr))
(define (sr-untag e t) (op$ binary-xor e t))

(: unmask-rest : D0-Expr D0-Expr -> D0-Expr)
(define (unmask-rest e m) (op$ binary-and e (op$ binary-not m)))

(: tagged-array-ref : D0-Expr D0-Expr D0-Expr -> D0-Expr)
(define (tagged-array-ref e tm i)
  (sr-array-ref (unmask-rest e tm) i))

(: check-tag? : D0-Expr D0-Expr -> D0-Expr)
(define (check-tag? e m) (op$ not (op$ = ZERO-IMDT (sr-get-tag e m))))

(: sr-tag-value (D0-Expr D0-Expr -> D0-Expr))
(define (sr-tag-value e t)
  (match t
    [(Quote 0) e]
    [tag (Op 'binary-or `(,e ,tag))]))

(: sr-check-tag=? (D0-Expr D0-Expr D0-Expr -> D0-Expr))
(define (sr-check-tag=? e mask tag) (op$ = (sr-get-tag e mask) tag))

(: sr-plus (D0-Expr D0-Expr -> D0-Expr))
(define (sr-plus f s) (op$ + f s))

(: sr-get-tag (D0-Expr D0-Expr -> D0-Expr))
(define (sr-get-tag e mask) (op$ binary-and e mask))
