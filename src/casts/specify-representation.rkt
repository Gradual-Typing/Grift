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
(require "../errors.rkt"
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
    (define boxed-bnd-code*  : (Boxof D0-Bnd-Code*) (box '()))
    (define new-exp : D0-Expr
      (sr-expr boxed-bnd-code* (hash) empty-index-map exp))
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

(: sr-expr ((Boxof D0-Bnd-Code*) Env IndexMap CoC6-Expr -> D0-Expr))
(define (sr-expr new-code env cenv exp)
  (: add-new-code! (D0-Bnd-Code -> Void))
  (define (add-new-code! b)
    (set-box! new-code (cons b (unbox new-code))))
  
  (: mk-fn-type-glb-code-label? (Boxof (Option (Code-Label Uid))))
  (define mk-fn-type-glb-code-label? (box #f))

  (: mk-tuple-type-glb-code-label? (Boxof (Option (Code-Label Uid))))
  (define mk-tuple-type-glb-code-label? (box #f))

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
  
  ;; The way that boxed immediate work currently bothers me.)    
  ;; Since we have access to unboxed static ints should we just
  ;; abandon the unboxed dyn integers another a mixture of static
  ;; allocation and and constant lifting could be used to make all
  (: sr-dyn-make ((CoC6-Expr -> D0-Expr) D0-Expr CoC6-Expr -> D0-Expr))
  (define (sr-dyn-make sr-expr e1 e2)
    (cond
      [(Type? e2)
       (match e2
         [(Type (Int))
          (Op '+ (list (Op '%<< (list e1 DYN-IMDT-SHIFT))
                       DYN-INT-TAG))]
         [(Type (Bool))
          (Op '+ (list (Op '%<< (list e1 DYN-IMDT-SHIFT))
                       DYN-BOOL-TAG))]
         [(Type (Unit))
          (Op '+ (list (Op '%<< (list e1 DYN-IMDT-SHIFT))
                       DYN-UNIT-TAG))]
         [(Type (Character))
          (Op '+ (list (Op '%<< (list e1 DYN-IMDT-SHIFT))
                       DYN-CHAR-TAG))]
         [else (sr-alloc "dynamic_boxed" DYN-BOXED-TAG
                         `(("value" . ,e1)
                           ("type" . ,(sr-expr e2))))])]
      [else
       (begin$
         (assign$ val e1)
         (assign$ type (sr-expr e2))
         (assign$ tag (Op 'binary-and `(,type ,TYPE-TAG-MASK)))
         (Switch
          type
          (let* ([shifted-imm (Op '%<< (list val DYN-IMDT-SHIFT))]
                 [tag-shifted-imm
                  (lambda ([tag : D0-Expr]) : D0-Expr
                    (Op 'binary-or `(,shifted-imm ,tag)))])
            `([(,data:TYPE-INT-RT-VALUE)  . ,(tag-shifted-imm DYN-INT-TAG)]
              [(,data:TYPE-BOOL-RT-VALUE) . ,(tag-shifted-imm DYN-BOOL-TAG)]
              [(,data:TYPE-UNIT-RT-VALUE) . ,(tag-shifted-imm DYN-UNIT-TAG)]
              [(,data:TYPE-CHAR-RT-VALUE) . ,(tag-shifted-imm DYN-CHAR-TAG)]))
          ;; Notice that float types fall into this case also
          (sr-alloc "dynamic_boxed" DYN-BOXED-TAG
                    `(("" . ,val) ("" . ,type)))))]))
  
  (: get-mk-tuple-type-glb! (Uid -> (Code-Label Uid)))
  (define (get-mk-tuple-type-glb! mk-tglb)
    (: make-code! (Uid -> (Code-Label Uid)))
    (define (make-code! mk-tglb)
      (define-track-next-uid!$ mk-tuple-type-glb)
      (define mk-tuple-type-glb-label (Code-Label mk-tuple-type-glb))
      (define mk-tglb-label (Code-Label mk-tglb))
      (define mk-tuple-type-glb-c : D0-Code
        (code$ (t1 t2 i count)
               (If (Op '= `(,i ,count))
                   (begin$
                     (assign$ t (Op 'Alloc (list (sr-plus count (Quote 1)))))
                     (sr-array-set! t TYPE-TUPLE-COUNT-INDEX count)
                     (sr-tag-value t TYPE-TUPLE-TAG))
                   (begin$
                     (assign$
                      t1a
                      (sr-tagged-array-ref
                       t1
                       TYPE-TUPLE-TAG
                       (sr-plus TYPE-TUPLE-ELEMENTS-OFFSET i)))
                     (assign$
                      t2a
                      (sr-tagged-array-ref
                       t2
                       TYPE-TUPLE-TAG
                       (sr-plus TYPE-TUPLE-ELEMENTS-OFFSET i)))
                     (assign$ t-glb (App-Code mk-tglb-label `(,t1a ,t2a)))
                     (assign$
                      tmp-t
                      (App-Code
                       mk-tuple-type-glb-label
                       `(,t1 ,t2 ,(sr-plus (Quote 1) i) ,count)))
                     (sr-tagged-array-set!
                      tmp-t
                      TYPE-TUPLE-TAG
                      (sr-plus TYPE-TUPLE-ELEMENTS-OFFSET i)
                      t-glb)
                     tmp-t))))
      (add-new-code! (cons mk-tuple-type-glb mk-tuple-type-glb-c))
      (set-box! mk-tuple-type-glb-code-label? mk-tuple-type-glb-label)
      mk-tuple-type-glb-label)
    (let ([cl? (unbox mk-tuple-type-glb-code-label?)])
      (or cl? (make-code! mk-tglb))))

  (: get-mk-fn-type-glb! (Uid -> (Code-Label Uid)))
  (define (get-mk-fn-type-glb! mk-tglb)
    (: make-code! (Uid -> (Code-Label Uid)))
    (define (make-code! mk-tglb)
      (define-track-next-uid!$ mk-fn-type-glb)
      (define mk-fn-type-glb-label (Code-Label mk-fn-type-glb))
      (define mk-tglb-label (Code-Label mk-tglb))
      (define mk-fn-type-glb-c : D0-Code
        (code$ (t1 t2 i arity)
               (If (Op '= `(,i ,arity))
                   (begin$
                     (assign$
                      t1r
                      (sr-tagged-array-ref t1 TYPE-FN-TAG TYPE-FN-RETURN-INDEX))
                     (assign$
                      t2r
                      (sr-tagged-array-ref t2 TYPE-FN-TAG TYPE-FN-RETURN-INDEX))
                     (assign$ rt (App-Code mk-tglb-label `(,t1r ,t2r)))
                     (assign$ t (Op 'Alloc (list (sr-plus arity (Quote 2)))))
                     (sr-array-set! t TYPE-FN-ARITY-INDEX arity)
                     (sr-array-set! t TYPE-FN-RETURN-INDEX rt)
                     (sr-tag-value t TYPE-FN-TAG))
                   (begin$
                     (assign$
                      t1a
                      (sr-tagged-array-ref
                       t1 TYPE-FN-TAG (sr-plus TYPE-FN-FMLS-OFFSET i)))
                     (assign$
                      t2a
                      (sr-tagged-array-ref
                       t2 TYPE-FN-TAG (sr-plus TYPE-FN-FMLS-OFFSET i)))
                     (assign$ t-glb (App-Code mk-tglb-label `(,t2a ,t1a)))
                     (assign$
                      t-tmp
                      (App-Code
                       mk-fn-type-glb-label
                       `(,t1 ,t2 ,(sr-plus (Quote 1) i) ,arity)))
                     (sr-tagged-array-set!
                      t-tmp
                      TYPE-FN-TAG
                      (sr-plus TYPE-FN-FMLS-OFFSET i)
                      t-glb)
                     t-tmp))))
      (add-new-code! (cons mk-fn-type-glb mk-fn-type-glb-c))
      (set-box! mk-fn-type-glb-code-label? mk-fn-type-glb-label)
      mk-fn-type-glb-label)
    (let ([cl? (unbox mk-fn-type-glb-code-label?)])
      (or cl? (make-code! mk-tglb))))

  (: get-mk-fn-crcn! (Uid -> (Code-Label Uid)))
  (define (get-mk-fn-crcn! mk-crcn)
    (: make-code! (Uid -> (Code-Label Uid)))
    (define (make-code! mk-crcn)
      (define-track-next-uid!$ mk-fn-crcn)
      (define mk-fn-crcn-label (Code-Label mk-fn-crcn))
      (define mk-crcn-label (Code-Label mk-crcn))
      (define mk-fn-crcn-c : D0-Code
        (code$ (t1 t2 l i arity)
               (If (Op '= `(,i ,arity))
                   (begin$
                     (assign$
                      t1r
                      (sr-tagged-array-ref t1 TYPE-FN-TAG TYPE-FN-RETURN-INDEX))
                     (assign$
                      t2r
                      (sr-tagged-array-ref t2 TYPE-FN-TAG TYPE-FN-RETURN-INDEX))
                     (assign$ cr (App-Code mk-crcn-label `(,t1r ,t2r ,l)))
                     (assign$ crcn (Op 'Alloc (list (sr-plus arity (Quote 2)))))
                     (assign$
                      tagged-arity
                      (Op
                       '+
                       (list (Op '%<< (list arity COERCION-SECOND-TAG-SHIFT))
                             COERCION-FN-SECOND-TAG)))
                     (sr-array-set! crcn COERCION-FN-ARITY-INDEX tagged-arity)
                     (sr-array-set! crcn COERCION-FN-RETURN-INDEX cr)
                     (sr-tag-value crcn COERCION-MEDIATING-TAG))
                   (begin$
                     (assign$
                      t1a
                      (sr-tagged-array-ref
                       t1 TYPE-FN-TAG (sr-plus TYPE-FN-FMLS-OFFSET i)))
                     (assign$
                      t2a
                      (sr-tagged-array-ref
                       t2 TYPE-FN-TAG (sr-plus TYPE-FN-FMLS-OFFSET i)))
                     (assign$ ca (App-Code mk-crcn-label `(,t2a ,t1a ,l)))
                     (assign$
                      tmp-crcn
                      (App-Code
                       mk-fn-crcn-label
                       `(,t1 ,t2 ,l ,(sr-plus (Quote 1) i) ,arity)))
                     (sr-tagged-array-set!
                      tmp-crcn
                      COERCION-MEDIATING-TAG
                      (sr-plus COERCION-FN-FMLS-OFFSET i) ca)
                     tmp-crcn))))
      (add-new-code! (cons mk-fn-crcn mk-fn-crcn-c))
      (set-box! mk-fn-coercion-code-label? mk-fn-crcn-label)
      mk-fn-crcn-label)
    (let ([cl? (unbox mk-fn-coercion-code-label?)])
      (or cl? (make-code! mk-crcn))))

  (: get-comp-fn-crcn! (Uid -> (Code-Label Uid)))
  (define (get-comp-fn-crcn! comp-crcn)
    (: make-code! (Uid -> (Code-Label Uid)))
    (define (make-code! mk-crcn)
      (define-track-next-uid!$ comp-fn)
      (define comp-fn-label (Code-Label comp-fn))
      (define comp-c (Code-Label comp-crcn))
      (define comp-fn-c : D0-Code
        (code$ (c1 c2 i arity id?1)
               (If (Op '= `(,i ,arity))
                   (begin$
                     (assign$
                      c1r
                      (sr-tagged-array-ref
                       c1 COERCION-MEDIATING-TAG COERCION-FN-RETURN-INDEX))
                     (assign$
                      c2r
                      (sr-tagged-array-ref
                       c2 COERCION-MEDIATING-TAG COERCION-FN-RETURN-INDEX))
                     (assign$ cr  (App-Code comp-c `(,c1r ,c2r)))
                     (If (If id?1
                             (sr-check-tag=?
                              cr COERCION-TAG-MASK COERCION-IDENTITY-TAG)
                             FALSE-IMDT)
                         COERCION-IDENTITY-IMDT
                         (begin$
                           (assign$
                            crcn
                            (Op 'Alloc (list (sr-plus arity (Quote 2)))))
                           (assign$
                            tagged-arity
                            (Op
                             '+
                             (list (Op '%<< (list arity
                                                  COERCION-SECOND-TAG-SHIFT))
                                   COERCION-FN-SECOND-TAG)))
                           (sr-array-set!
                            crcn COERCION-FN-ARITY-INDEX tagged-arity)
                           (sr-array-set! crcn COERCION-FN-RETURN-INDEX cr)
                           (sr-tag-value crcn COERCION-MEDIATING-TAG))))
                   (begin$
                     (assign$
                      c1a
                      (sr-tagged-array-ref
                       c1
                       COERCION-MEDIATING-TAG
                       (sr-plus COERCION-FN-FMLS-OFFSET i)))
                     (assign$
                      c2a
                      (sr-tagged-array-ref
                       c2
                       COERCION-MEDIATING-TAG
                       (sr-plus COERCION-FN-FMLS-OFFSET i)))
                     (assign$ ca   (App-Code comp-c `(,c2a ,c1a)))
                     (assign$ id?2
                              (If id?1
                                  (sr-check-tag=?
                                   ca COERCION-TAG-MASK COERCION-IDENTITY-TAG)
                                  FALSE-IMDT))
                     (assign$
                      tmp-crcn
                      (App-Code
                       comp-fn-label
                       `(,c1 ,c2 ,(sr-plus (Quote 1) i) ,arity ,id?2)))
                     (If (sr-check-tag=?
                          tmp-crcn COERCION-TAG-MASK COERCION-IDENTITY-TAG)
                         COERCION-IDENTITY-IMDT
                         (Begin
                          (list
                           (sr-tagged-array-set!
                            tmp-crcn
                            COERCION-MEDIATING-TAG
                            (sr-plus COERCION-FN-FMLS-OFFSET i)
                            ca))
                          tmp-crcn))))))
      (add-new-code! (cons comp-fn comp-fn-c))
      (set-box! comp-fn-coercion-code-label? comp-fn-label)
      comp-fn-label)
    (let ([cl? (unbox comp-fn-coercion-code-label?)])
      (or cl? (make-code! comp-crcn))))

  (: get-coerce-tuple! (Uid -> (Code-Label Uid)))
  (define (get-coerce-tuple! cast)
    (: make-code! (Uid -> (Code-Label Uid)))
    (define (make-code! cast)
      (define-track-next-uid!$ coerce-tuple)
      (define coerce-tuple-label (Code-Label coerce-tuple))
      (define cast-label (Code-Label cast))
      (define coerce-tuple-c : D0-Code
        (code$ (v c i count)
               (If (Op '= `(,i ,count))
                   (begin$ (assign$ tpl (Op 'Alloc (list count))) tpl)
                   (begin$
                     (assign$ va (Op 'Array-ref (list v i)))
                     (assign$
                      ca
                      (sr-tagged-array-ref
                       c
                       COERCION-MEDIATING-TAG
                       (sr-plus COERCION-TUPLE-ELEMENTS-OFFSET i)))
                     (assign$ casted-val
                              (App-Code cast-label `(,va ,ca ,ZERO-IMDT)))
                     (assign$
                      tmp-tpl
                      (App-Code
                       coerce-tuple-label
                       `(,v ,c ,(sr-plus (Quote 1) i) ,count)))
                     (Op 'Array-set! (list tmp-tpl i casted-val))
                     tmp-tpl))))
      (add-new-code! (cons coerce-tuple coerce-tuple-c))
      (set-box! coerce-tuple-code-label? coerce-tuple-label)
      coerce-tuple-label)
    (let ([cl? (unbox coerce-tuple-code-label?)])
      (or cl? (make-code! cast))))

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
                 (assign$
                  tagged-count
                  (sr-tagged-array-ref
                   tpl-crcn COERCION-MEDIATING-TAG COERCION-TUPLE-COUNT-INDEX))
                 (assign$
                  count
                  (Op '%>> (list tagged-count COERCION-SECOND-TAG-SHIFT)))
                 (repeat$ (i ZERO-IMDT count) (_ UNIT-IMDT)
                          (assign$ val (Op 'Array-ref (list tpl-val i)))
                          (assign$
                           crcn
                           (sr-tagged-array-ref
                            tpl-crcn
                            COERCION-MEDIATING-TAG
                            (sr-plus COERCION-TUPLE-ELEMENTS-OFFSET i)))
                          (assign$
                           rtti1
                           (Op 'Array-ref (list mono-addr MONO-RTTI-INDEX)))
                          (assign$
                           new-val
                           (App-Code cast-label `(,val ,crcn ,mono-addr)))
                          (assign$
                           rtti2
                           (Op 'Array-ref (list mono-addr MONO-RTTI-INDEX)))
                          (If (Op '= (list rtti1 rtti2))
                              (Op 'Array-set! (list tpl-val i new-val))
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
                 (assign$
                  count
                  (sr-tagged-array-ref
                   t2 TYPE-TUPLE-TAG TYPE-TUPLE-COUNT-INDEX))
                 (repeat$ (i ZERO-IMDT count) (_ UNIT-IMDT)
                          (begin$
                            (assign$ val (Op 'Array-ref (list tpl-val i)))
                            (assign$
                             t1a
                             (sr-tagged-array-ref
                              t1
                              TYPE-TUPLE-TAG
                              (sr-plus TYPE-TUPLE-ELEMENTS-OFFSET i)))
                            (assign$
                             t2a
                             (sr-tagged-array-ref
                              t2
                              TYPE-TUPLE-TAG
                              (sr-plus TYPE-TUPLE-ELEMENTS-OFFSET i)))
                            (assign$
                             rtti1
                             (Op 'Array-ref (list mono-addr MONO-RTTI-INDEX)))
                            (assign$
                             new-val
                             (App-Code
                              cast-label `(,val ,t1a ,t2a ,l ,mono-addr)))
                            (assign$
                             rtti2
                             (Op 'Array-ref (list mono-addr MONO-RTTI-INDEX)))
                            (If (Op '= (list rtti1 rtti2))
                                (Op 'Array-set! (list tpl-val i new-val))
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
        (code$ (tpl-val t1 t2 l i count)
               (If (Op '= `(,i ,count))
                   (begin$ (assign$ crcn (Op 'Alloc (list count))) crcn)
                   (begin$
                     (assign$ val (Op 'Array-ref (list tpl-val i)))
                     (assign$
                      t1a
                      (sr-tagged-array-ref
                       t1
                       TYPE-TUPLE-TAG
                       (sr-plus TYPE-TUPLE-ELEMENTS-OFFSET i)))
                     (assign$
                      t2a
                      (sr-tagged-array-ref
                       t2
                       TYPE-TUPLE-TAG
                       (sr-plus TYPE-TUPLE-ELEMENTS-OFFSET i)))
                     (assign$
                      casted-val
                      (App-Code cast-label `(,val ,t1a ,t2a ,l ,ZERO-IMDT)))
                     (assign$
                      tmp-crcn
                      (App-Code
                       cast-tuple-label
                       `(,tpl-val ,t1 ,t2 ,l ,(sr-plus (Quote 1) i) ,count)))
                     (Op 'Array-set! (list tmp-crcn i casted-val))
                     tmp-crcn))))
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
        (code$ (t1 t2 l i count)
               (If (Op '= `(,i ,count))
                   (begin$
                     (assign$ crcn (Op 'Alloc (list (sr-plus count (Quote 1)))))
                     (assign$
                      tagged-count
                      (Op
                       '+
                       (list (Op '%<< (list count COERCION-SECOND-TAG-SHIFT))
                             COERCION-TUPLE-SECOND-TAG)))
                     (sr-array-set!
                      crcn COERCION-TUPLE-COUNT-INDEX tagged-count)
                     (sr-tag-value crcn COERCION-MEDIATING-TAG))
                   (begin$
                     (assign$
                      t1a
                      (sr-tagged-array-ref
                       t1
                       TYPE-TUPLE-TAG
                       (sr-plus TYPE-TUPLE-ELEMENTS-OFFSET i)))
                     (assign$
                      t2a
                      (sr-tagged-array-ref
                       t2
                       TYPE-TUPLE-TAG
                       (sr-plus TYPE-TUPLE-ELEMENTS-OFFSET i)))
                     (assign$ val-crcn (App-Code mk-crcn-label `(,t1a ,t2a ,l)))
                     (assign$
                      tmp-crcn
                      (App-Code
                       mk-tuple-crcn-label
                       `(,t1 ,t2 ,l ,(sr-plus (Quote 1) i) ,count)))
                     (sr-tagged-array-set!
                      tmp-crcn
                      COERCION-MEDIATING-TAG
                      (sr-plus COERCION-TUPLE-ELEMENTS-OFFSET i)
                      val-crcn)
                     tmp-crcn))))
      (add-new-code! (cons mk-tuple-crcn mk-tuple-crcn-c))
      (set-box! mk-tuple-coercion-code-label? mk-tuple-crcn-label)
      mk-tuple-crcn-label)
    (let ([cl? (unbox mk-tuple-coercion-code-label?)])
      (or cl? (make-code! mk-crcn))))

  (: get-comp-tuple-crcn! (Uid -> (Code-Label Uid)))
  (define (get-comp-tuple-crcn! comp-crcn)
    (: make-code! (Uid -> (Code-Label Uid)))
    (define (make-code! mk-crcn)
      (define-track-next-uid!$ comp-tuple-crcn)
      (define comp-tuple-crcn-label (Code-Label comp-tuple-crcn))
      (define comp-crcn-label (Code-Label comp-crcn))
      (define comp-tuple-crcn-c : D0-Code
        (code$ (crcn1 crcn2 i count id?1)
               (If (Op '= `(,i ,count))
                   (begin$
                     (assign$ crcn (Op 'Alloc (list (sr-plus count (Quote 1)))))
                     (assign$
                      tagged-count
                      (Op
                       '+
                       (list (Op '%<< (list count COERCION-SECOND-TAG-SHIFT))
                             COERCION-TUPLE-SECOND-TAG)))
                     (sr-array-set!
                      crcn COERCION-TUPLE-COUNT-INDEX tagged-count)
                     (sr-tag-value crcn COERCION-MEDIATING-TAG))
                   (begin$
                     (assign$
                      c1a
                      (sr-tagged-array-ref
                       crcn1
                       COERCION-MEDIATING-TAG
                       (sr-plus COERCION-TUPLE-ELEMENTS-OFFSET i)))
                     (assign$
                      c2a
                      (sr-tagged-array-ref
                       crcn2
                       COERCION-MEDIATING-TAG
                       (sr-plus COERCION-TUPLE-ELEMENTS-OFFSET i)))
                     (assign$
                      composed-crcn (App-Code comp-crcn-label `(,c1a ,c2a)))
                     (assign$
                      id?2
                      (If id?1
                          (sr-check-tag=?
                           composed-crcn
                           COERCION-TAG-MASK
                           COERCION-IDENTITY-TAG)
                          FALSE-IMDT))
                     (assign$
                      tmp-crcn
                      (App-Code
                       comp-tuple-crcn-label
                       `(,crcn1 ,crcn2 ,(sr-plus (Quote 1) i) ,count ,id?2)))
                     (If (sr-check-tag=?
                          tmp-crcn COERCION-TAG-MASK COERCION-IDENTITY-TAG)
                         COERCION-IDENTITY-IMDT
                         (Begin
                          (list
                           (sr-tagged-array-set!
                            tmp-crcn
                            COERCION-MEDIATING-TAG
                            (sr-plus COERCION-TUPLE-ELEMENTS-OFFSET i)
                            composed-crcn))
                          tmp-crcn))))))
      (add-new-code! (cons comp-tuple-crcn comp-tuple-crcn-c))
      (set-box! comp-tuple-coercion-code-label? comp-tuple-crcn-label)
      comp-tuple-crcn-label)
    (let ([cl? (unbox comp-tuple-coercion-code-label?)])
      (or cl? (make-code! comp-crcn))))

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
         (Op 'Array-ref `(,(Op 'binary-and `(,CLOSURE-VALUE-MASK ,e))
                          ,CLOS-CSTR-INDEX))]
        [(Closure-code (app recur e))
         (Op 'Array-ref `(,(Op 'binary-and `(,CLOSURE-VALUE-MASK ,e))
                          ,CLOS-CODE-INDEX))]
        [(Closure-ref clos fvar)
         (Op 'Array-ref `(,(Op 'binary-and `(,(Var clos) ,CLOSURE-VALUE-MASK))
                          ,(Quote (cenv clos fvar))))]
        [(Var i) (lookup env i)]
        [(Labels (app (sr-bnd-code* recur/env) b*) e)
         (let* ([u* (map (inst car Uid Any) b*)]
                [l* (map label u*)])
           (Labels b* (recur/env e (extend* env u* l*) cenv)))]
        [(App-Code e e*) (App-Code (recur e) (recur* e*))]
        [(Code-Label u) (Code-Label u)]
        ;; Type Representation
        [(Type t) (sr-prim-type t)]
        [(Type-Fn-Huh (app recur e))
         (sr-check-tag=? e TYPE-TAG-MASK TYPE-FN-TAG)]
        [(Type-Fn-arity (app recur e))
         (Op 'Array-ref (list e TYPE-FN-ARITY-INDEX))]
        [(Type-Fn-return (app recur e))
         (Op 'Array-ref (list e TYPE-FN-RETURN-INDEX))]
        [(Type-Fn-arg (app recur e1) (app recur e2))
         (define e2^
           (match e2
             [(Quote (? fixnum? k)) (Quote (+ data:TYPE-FN-FMLS-OFFSET k))]
             [otherwiths (Op '+ (list e2 TYPE-FN-FMLS-OFFSET))]))
         (Op 'Array-ref (list e1 e2^))]
        [(Type-GRef-Huh (app recur e))
         (sr-check-tag=? e TYPE-TAG-MASK TYPE-GREF-TAG)]
        [(Type-GRef-Of (app recur e))
         (define arg : D0-Expr
           (Op 'binary-xor (list e TYPE-GREF-TAG)))
         (Op 'Array-ref (list arg TYPE-GREF-TYPE-INDEX))]
        [(Type-GVect-Huh (app recur e))
         (sr-check-tag=? e TYPE-TAG-MASK TYPE-GVECT-TAG)]
        [(Type-GVect-Of (app recur e))
         (define arg : D0-Expr
           (Op 'binary-xor (list e TYPE-GVECT-TAG)))
         (Op 'Array-ref (list arg TYPE-GVECT-TYPE-INDEX))]
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
         (sr-check-tag=?
          e COERCION-TAG-MASK COERCION-PROJECT-TAG)]
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
                (Op 'binary-or `(,prj-bit ,inj-bit)))])) 
         (sr-alloc "hyper_Coercion" (ann tag-expr D0-Expr)
                   `(("t1" . ,t1)
                     ("label" . ,lbl)
                     ("t2" . ,t2)
                     ("med-coercion" . ,m)))]
        [(HC-Inject-Huh (app recur h))
         (HC-Inject-Huh h)
         (check-tag? h HC-INJ-TAG-MASK)]
        [(HC-Project-Huh (app recur h)) 
         (check-tag? h HC-PRJ-TAG-MASK)]
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
           (assign$ crcn-tag (Op 'binary-and `(,tmp-crcn ,COERCION-TAG-MASK)))
           (Op '= (list crcn-tag COERCION-FN-SECOND-TAG)))]
        [(Fn-Coercion-Arity (app recur e))
         (begin$
           (assign$ tagged-arity
                    (sr-tagged-array-ref
                     e COERCION-MEDIATING-TAG COERCION-FN-ARITY-INDEX))
           (Op '%>> (list tagged-arity COERCION-SECOND-TAG-SHIFT)))]
        [(Fn-Coercion-Arg (app recur e) (app recur i))
         (sr-tagged-array-ref
          e COERCION-MEDIATING-TAG (sr-plus COERCION-FN-FMLS-OFFSET i))]
        [(Fn-Coercion-Return (app recur e))
         (sr-tagged-array-ref e COERCION-MEDIATING-TAG COERCION-FN-RETURN-INDEX)]
        ;; TODO either repurpose or get rid of the arity field
        ;; One could immagine that we use it to dynamically dispatch on compose
        [(Fn-Coercion (app recur* e*) (app recur e))
         (begin$
           (assign$
            tag_and_arity
            (Op '+ (list (Op '%<< (list (Quote (length e*))
                                        COERCION-SECOND-TAG-SHIFT))
                         COERCION-FN-SECOND-TAG)))
           (sr-alloc "fn_coercion" COERCION-MEDIATING-TAG
                     `(("arity" . ,tag_and_arity)
                       ("return" . ,e) .
                       ,(map (lambda ([e : D0-Expr])
                               (cons "argument" e))
                             e*))))]
        [(Make-Fn-Coercion mk-crcn (app recur t1) (app recur t2) (app recur l))
         (: invoke-mk-fn-crcn ((Code-Label Uid) (Var Uid) D0-Expr D0-Expr -> D0-Expr))
         (define (invoke-mk-fn-crcn mk-fn t1 t2 l)
           (App-Code
            mk-fn
            (list t1 t2 l ZERO-IMDT
                  (sr-tagged-array-ref t1 TYPE-FN-TAG TYPE-FN-ARITY-INDEX))))
         (let ([mk-fn-crcn (get-mk-fn-crcn! mk-crcn)])
           (if (Var? t1)
               (invoke-mk-fn-crcn mk-fn-crcn t1 t2 l)
               (begin$
                 (assign$ fn-type1 t1)
                 (invoke-mk-fn-crcn mk-fn-crcn fn-type1 t2 l))))]
        [(Compose-Fn-Coercion compose (app recur c1) (app recur c2))
         (: invoke-comp ((Code-Label Uid) (Var Uid) D0-Expr -> D0-Expr))
         (define (invoke-comp comp-fn c1 c2)
           (begin$
             (assign$ tagged-arity
                      (sr-tagged-array-ref
                       c1 COERCION-MEDIATING-TAG COERCION-FN-ARITY-INDEX))
             (assign$ arity
                      (Op '%>> (list tagged-arity COERCION-SECOND-TAG-SHIFT)))
             (App-Code comp-fn (list c1 c2 ZERO-IMDT arity TRUE-IMDT))))
         (let ([mk-fn-crcn (get-comp-fn-crcn! compose)])
           (if (Var? c1)
               (invoke-comp mk-fn-crcn c1 c2)
               (begin$
                 (assign$ fn-crcn1 c1)
                 (invoke-comp mk-fn-crcn fn-crcn1 c2))))]
        [(Id-Fn-Coercion (app recur a))
         (begin$
           (assign$ size a)
           (assign$ fn-c  (Op 'Alloc (list (sr-plus size (Quote 2)))))
           (assign$ info
                    (Op '+ `(,(Op '%<< `(,size ,COERCION-SECOND-TAG-SHIFT))
                             ,COERCION-FN-SECOND-TAG)))
           (sr-array-set! fn-c COERCION-FN-ARITY-INDEX info)
           (sr-array-set! fn-c COERCION-FN-RETURN-INDEX COERCION-IDENTITY-IMDT)
           (repeat$ (i (Quote 0) size) (_ (Quote 0))
                    (sr-array-set!
                     fn-c
                     (Op '+ `(,i ,COERCION-FN-FMLS-OFFSET))
                     COERCION-IDENTITY-IMDT))
           (sr-tag-value fn-c COERCION-MEDIATING-TAG))]
        [(Id-Tuple-Coercion (app recur a))
         (begin$
           (assign$ size a)
           (assign$ tup-c
                    (ann (Op 'Alloc (list (sr-plus size (Quote 1)))) D0-Expr))
           (assign$
            info
            (ann (Op '+ (list (Op '%<< (list size COERCION-SECOND-TAG-SHIFT))
                              COERCION-TUPLE-SECOND-TAG))
                 D0-Expr))
           (sr-array-set! tup-c COERCION-TUPLE-COUNT-INDEX info)
           (repeat$ (i (Quote 0) size) (_ (Quote 0))
                    (sr-array-set!
                     tup-c
                     (Op '+ `(,i ,COERCION-TUPLE-ELEMENTS-OFFSET))
                     COERCION-IDENTITY-IMDT))
           (sr-tag-value tup-c COERCION-MEDIATING-TAG))]
        [(Fn-Coercion-Arg-Set! (app recur f) (app recur i) (app recur a))
         (sr-tagged-array-set!
          f COERCION-MEDIATING-TAG (Op '+ (list i COERCION-FN-FMLS-OFFSET)) a)]
        [(Fn-Coercion-Return-Set! (app recur f) (app recur r))
         (sr-tagged-array-set!
          f COERCION-MEDIATING-TAG COERCION-FN-RETURN-INDEX r)]
        [(Tuple-Coercion-Item-Set! (app recur t) (app recur i) (app recur e))
         (sr-tagged-array-set!
          t
          COERCION-MEDIATING-TAG
          (Op '+ (list i COERCION-TUPLE-ELEMENTS-OFFSET))
          e)]
        [(Ref-Coercion-Huh (app recur e))
         (begin$
           (assign$
            tmp-crcn
            (sr-tagged-array-ref
             e COERCION-MEDIATING-TAG COERCION-REF-TAG-INDEX))
           (assign$ crcn-tag (Op 'binary-and `(,tmp-crcn ,COERCION-TAG-MASK)))
           (Op '= (list crcn-tag COERCION-REF-SECOND-TAG)))]
        [(Ref-Coercion (app recur r) (app recur w))
         (begin$
           (assign$
            second-tag
            (Op '+ (list (Op '%<< (list ZERO-IMDT COERCION-SECOND-TAG-SHIFT))
                         COERCION-REF-SECOND-TAG)))
           (sr-alloc "ref-coercion" COERCION-MEDIATING-TAG
                     `(("tag" . ,second-tag)
                       ("read-coercion" . ,r)
                       ("write-coercion" . ,w))))]        
        [(Ref-Coercion-Read (app recur e))
         (sr-tagged-array-ref
          e COERCION-MEDIATING-TAG COERCION-REF-READ-INDEX)]
        [(Ref-Coercion-Write (app recur e))
         (sr-tagged-array-ref
          e COERCION-MEDIATING-TAG COERCION-REF-WRITE-INDEX)]
        [(Failed-Coercion-Huh (app recur e))
         (sr-check-tag=? e COERCION-TAG-MASK COERCION-FAILED-TAG)]
        ;; For now I am allocating the blame label in a box.
        ;; Make this cheaper by ensuring that the string pointer is alligned and
        ;; tagging it.
        [(Failed-Coercion (app recur l))
         (sr-alloc "failed-coercion" COERCION-FAILED-TAG `(("label" . ,l)))]
        [(Failed-Coercion-Label (app recur e))
         (sr-tagged-array-ref
          e COERCION-FAILED-TAG COERCION-FAILED-LABEL-INDEX)]
        ;; FN-Proxy Stuff
        [(Fn-Proxy i (app recur clos) (app recur crcn))
         (sr-alloc "fn-proxy" FN-PROXY-TAG
                   `(("closure" . ,clos)
                     ("coercion" . ,crcn)))]
        [(Fn-Proxy-Huh (app recur e))
         (sr-check-tag=? e FN-TAG-MASK FN-PROXY-TAG)]
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
        [(Construct (Dyn) 'make (list e t))
         (sr-dyn-make recur (recur e) t)]
        [(Access (Dyn) 'value (app recur e) #f)
         (begin$
           (assign$ tmp e)
           (assign$ tag (Op 'binary-and `(,tmp ,DYN-TAG-MASK)))
           (If (Op '= (list tag DYN-BOXED-TAG))
               (Op 'Array-ref (list tmp DYN-VALUE-INDEX))
               (Op '%>> (list tmp DYN-IMDT-SHIFT))))]
        [(Access (Dyn) 'type (app recur e) #f)
         (define err-msg
           (Quote "specify-representation/Dyn-type: switch failure"))
         (begin$
           (assign$ tmp e)
           (assign$ tag (Op 'binary-and `(,tmp ,DYN-TAG-MASK)))
           (Switch tag
                   `([(,data:DYN-BOXED-TAG) . ,(Op 'Array-ref
                                                   (list tmp DYN-TYPE-INDEX))]
                     [(,data:DYN-INT-TAG) . ,TYPE-INT-RT-VALUE]
                     [(,data:DYN-BOOL-TAG) . ,TYPE-BOOL-RT-VALUE]
                     [(,data:DYN-UNIT-TAG) . ,TYPE-UNIT-RT-VALUE]
                     [(,data:DYN-CHAR-TAG) . ,TYPE-CHAR-RT-VALUE])
                   (begin$ (Op 'Print (list err-msg))
                           (Op 'Exit  (list (Quote 1)))
                           UNDEF-IMDT)))]
        [(Access (Dyn) 'immediate-value (app recur e) #f)
         (Op '%>> (list e DYN-IMDT-SHIFT))]
        [(Access (Dyn) 'immediate-tag (app recur e) #f)
         (Op 'binary-and (list e DYN-TAG-MASK))]
        [(Access (Dyn) 'box-value (app recur e) #f)
         (Op 'Array-ref (list e DYN-VALUE-INDEX))]
        [(Access (Dyn) 'box-type (app recur e) #f)
         (Op 'Array-ref (list e DYN-TYPE-INDEX))]
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
            (Op '= `(,tag ,(Op 'binary-and `(,e ,DYN-TAG-MASK))))]
           [other (error 'dyn-immediate-tag=? "expected type literal: ~a" t)])]
        ;; Observable Results Representation
        [(Blame (app recur e))
         (Begin
          (list (Op 'Print (list e))
                (Op 'Exit  (list (Quote -1))))
          UNDEF-IMDT)]
        [(Observe (app recur e) t) (sr-observe next-uid! e t)]
        [(and nop (No-Op)) nop]
        ;; References Representation
        [(Begin (app recur* e*) (app recur e))
         (Begin e* e)]
        [(Repeat i (app recur e1) (app recur e2) u (app recur e3) e4)
         (Repeat i e1 e2 u e3
                 (recur/env e4 (extend (extend env u (Var u)) i (Var i)) cenv))]
        [(Break-Repeat) (Break-Repeat)]
        ;; Guarded
        [(Unguarded-Box (app recur e))
         (sr-alloc "unguarded_box" UGBOX-TAG (list (cons "init_value" e)))]
        [(Unguarded-Box-Ref (app recur e))
         (Op 'Array-ref (list e UGBOX-VALUE-INDEX))]
        [(Unguarded-Box-Set! (app recur e1) (app recur e2))
         (Op 'Array-set! (list e1 UGBOX-VALUE-INDEX e2))]
        [(Unguarded-Vect (app recur e1) (app recur e2))
         (begin$
           (assign$ size e1)
           (assign$ init-val e2)
           (assign$ repr-size (Op '+ (list size UGVECT-OFFSET)))
           (assign$ vect (Op 'Alloc (list repr-size)))
           (Op 'Array-set! (list vect UGVECT-SIZE-INDEX size))
           (repeat$ (i UGVECT-OFFSET repr-size) (_ UNIT-IMDT)
                    (Op 'Array-set! (list vect i init-val)))
           vect)]
        [(Unguarded-Vect-Ref (app recur e1) (app recur e2))
         (begin$
           (assign$ ind e2)
           (assign$ vect e1)
           ;; TODO This duplicates the error exit code (fix this so it doesn't)
           (if (bounds-checks?)
               (If (Op '>= (list ind ZERO-IMDT)) ;; vectors indices starts from 0
                   (If (Op '< (list ind (Op 'Array-ref (list vect ZERO-IMDT))))
                       (Op 'Array-ref (list vect (Op '+ (list ind UGVECT-OFFSET))))
                       (Begin
                        (list (Op 'Printf (list (Quote "index out of bound %ld\n") ind)))
                        (Op 'Exit (list (Quote -1)))))
                   (Begin
                    (list (Op 'Printf (list (Quote "index out of bound %ld\n") ind)))
                    (Op 'Exit (list (Quote -1)))))
               (Op 'Array-ref (list vect (Op '+ (list ind UGVECT-OFFSET))))))]
        [(Unguarded-Vect-Set! (app recur e1) (app recur e2) (app recur e3))
         (begin$
           (assign$ ind e2)
           (assign$ vect e1)
           (if (bounds-checks?)
               (If (Op '>= (list ind ZERO-IMDT)) ;; vectors indices starts from 0
                   (If (Op '< (list ind (Op 'Array-ref (list vect ZERO-IMDT))))
                       (Op 'Array-set! (list vect (Op '+ (list ind UGVECT-OFFSET)) e3))
                       (Begin
                        (list (Op 'Printf (list (Quote "index out of bound %ld\n") ind)))
                        (Op 'Exit (list (Quote -1)))))
                   (Begin
                    `(,(Op 'Printf (list (Quote "index out of bound %ld\n") ind)))
                    (Op 'Exit (list (Quote -1)))))
               (Op 'Array-set! (list vect (Op '+ (list ind UGVECT-OFFSET)) e3))))]
        [(Guarded-Proxy-Huh (app recur e))
         (Op '= `(,(Op 'binary-and (list e GREP-TAG-MASK))
                  ,GPROXY-TAG))]
        [(Unguarded-Vect-length (app recur e))
         (Op 'Array-ref (list e UGVECT-SIZE-INDEX))]
        [(Guarded-Proxy (app recur e) r)
         ;; Consider using sr-alloc here
         (match r
           [(Twosome (app recur t1) (app recur t2) (app recur l))
            (alloc-tag-set-gproxy/twosome next-uid! e t1 t2 l)]
           [(Coercion (app recur c))
            (alloc-tag-set-gproxy/coercion next-uid! e c)])]
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
         (Op 'Array-set! (list e1 MBOX-VALUE-INDEX e2))]
        [(Mbox-val-ref (app recur e))
         (Op 'Array-ref (list e MBOX-VALUE-INDEX))]
        [(Mbox-rtti-set! (app recur addr) (app recur e))
         (Op 'Array-set! (list addr MONO-RTTI-INDEX e))]
        [(Mbox-rtti-ref (app recur addr))
         (Op 'Array-ref (list addr MONO-RTTI-INDEX))]
        [(Mvector (app recur e1) (app recur e2) (app sr-prim-type t))
         (begin$
           (assign$ size e1)
           (assign$ init-val e2)
           (assign$ rtti t)
           (assign$ repr-size (Op '+ (list size MVECT-OFFSET)))
           (assign$ vect (Op 'Alloc (list repr-size)))
           (Op 'Array-set! (list vect MVECT-SIZE-INDEX size))
           (Op 'Array-set! (list vect MONO-RTTI-INDEX rtti))
           (repeat$ (i MVECT-OFFSET repr-size) (_ UNIT-IMDT)
                    (Op 'Array-set! (list vect i init-val)))
           vect)]
        [(Mvector-length (app recur e))
         (Op 'Array-ref (list e MVECT-SIZE-INDEX))]
        [(Mvector-rtti-set! (app recur addr) (app recur e))
         (Op 'Array-set! (list addr MONO-RTTI-INDEX e))]
        [(Mvector-rtti-ref (app recur addr))
         (Op 'Array-ref (list addr MONO-RTTI-INDEX))]
        [(Mvector-val-ref (app recur e1) (app recur e2))
         (begin$
           (assign$ ind e2)
           (assign$ mvect e1)
           (if (bounds-checks?)
               (If (Op '>= (list ind ZERO-IMDT)) ;; vectors indices starts from 0
                   (If (Op '< (list ind (Op 'Array-ref (list mvect MVECT-SIZE-INDEX))))
                       (Op 'Array-ref (list mvect (Op '+ (list ind MVECT-OFFSET))))
                       (Begin
                        (list (Op 'Printf (list (Quote "index out of bound %ld\n") ind)))
                        (Op 'Exit (list (Quote -1)))))
                   (Begin
                    (list (Op 'Printf (list (Quote "index out of bound %ld\n") ind)))
                    (Op 'Exit (list (Quote -1)))))
               (Op 'Array-ref (list mvect (Op '+ (list ind MVECT-OFFSET))))))]
        [(Mvector-val-set! (app recur e1) (app recur e2) (app recur e3))
         (begin$
           (assign$ ind e2)
           (assign$ mvect e1)
           (if (bounds-checks?)
               (If (Op '>= (list ind ZERO-IMDT)) ;; vectors indices starts from 0
                   (If (Op '< (list ind (Op 'Array-ref (list mvect MVECT-SIZE-INDEX))))
                       (Op 'Array-set! (list mvect (Op '+ (list ind MVECT-OFFSET)) e3))
                       (Begin
                        (list (Op 'Printf (list (Quote "index out of bound %ld\n") ind)))
                        (Op 'Exit (list (Quote -1)))))
                   (Begin
                    `(,(Op 'Printf (list (Quote "index out of bound %ld\n") ind)))
                    (Op 'Exit (list (Quote -1)))))
               (Begin
                `(,(Op 'Printf (list (Quote "index out of bound %ld\n") ind)))
                (Op 'Exit (list (Quote -1))))))]
        [(Type-MVect e) (sr-type-mvect recur e)]
        [(Type-MVect-Huh (app recur e))
         (sr-check-tag=? e TYPE-TAG-MASK TYPE-MVECT-TAG)]
        [(Type-MVect-Of (app recur e))
         (define arg : D0-Expr
           (Op 'binary-xor (list e TYPE-MVECT-TAG)))
         (Op 'Array-ref (list arg TYPE-MVECT-TYPE-INDEX))]
        [(MVect-Coercion-Huh (app recur e))
         (begin$
           (assign$
            tmp-crcn
            (sr-tagged-array-ref
             e COERCION-MEDIATING-TAG COERCION-MVECT-TAG-INDEX))
           (assign$ crcn-tag (Op 'binary-and `(,tmp-crcn ,COERCION-TAG-MASK)))
           (Op '= (list crcn-tag COERCION-MVECT-SECOND-TAG)))]
        [(MVect-Coercion-Type (app recur e))
         (sr-tagged-array-ref
          e COERCION-MEDIATING-TAG COERCION-MVECT-TYPE-INDEX)]
        [(MVect-Coercion (app recur t))
         (begin$
           (assign$
            second-tag
            (Op '+ (list (Op '%<< (list ZERO-IMDT COERCION-SECOND-TAG-SHIFT))
                         COERCION-MVECT-SECOND-TAG)))
           (sr-alloc "mvect-coercion" COERCION-MEDIATING-TAG
                     `(("tag" . ,second-tag)
                       ("type" . ,t))))]
        [(MRef-Coercion-Huh (app recur e))
         (begin$
           (assign$
            tmp-crcn
            (sr-tagged-array-ref
             e COERCION-MEDIATING-TAG COERCION-MREF-TAG-INDEX))
           (assign$ crcn-tag (Op 'binary-and `(,tmp-crcn ,COERCION-TAG-MASK)))
           (Op '= (list crcn-tag COERCION-MREF-SECOND-TAG)))]
        [(MRef-Coercion-Type (app recur e))
         (sr-tagged-array-ref
          e COERCION-MEDIATING-TAG COERCION-MREF-TYPE-INDEX)]
        [(MRef-Coercion (app recur t))
         (begin$
           (assign$
            second-tag
            (Op '+ (list (Op '%<< (list ZERO-IMDT COERCION-SECOND-TAG-SHIFT))
                         COERCION-MREF-SECOND-TAG)))
           (sr-alloc "ref-coercion" COERCION-MEDIATING-TAG
                     `(("tag" . ,second-tag)
                       ("type" . ,t))))]
        [(Make-GLB-Two-Fn-Types mk-glb (app recur t1) (app recur t2))
         (: invoke-mk-fn-type-glb ((Code-Label Uid) (Var Uid) D0-Expr -> D0-Expr))
         (define (invoke-mk-fn-type-glb mk-fn t1 t2)
           (App-Code
            mk-fn
            (list t1 t2 ZERO-IMDT
                  (sr-tagged-array-ref t1 TYPE-FN-TAG TYPE-FN-ARITY-INDEX))))
         (let ([mk-fn-type-glb (get-mk-fn-type-glb! mk-glb)])
           (if (Var? t1)
               (invoke-mk-fn-type-glb mk-fn-type-glb t1 t2)
               (begin$
                 (assign$ fn-type1 t1)
                 (invoke-mk-fn-type-glb mk-fn-type-glb fn-type1 t2))))]
        [(Make-GLB-Two-Tuple-Types mk-glb (app recur t1) (app recur t2))
         (: invoke-mk-tuple-type-glb ((Code-Label Uid) (Var Uid) D0-Expr -> D0-Expr))
         (define (invoke-mk-tuple-type-glb mk-tuple t1 t2)
           (App-Code
            mk-tuple
            (list t1 t2 ZERO-IMDT
                  (sr-tagged-array-ref t2 TYPE-TUPLE-TAG TYPE-TUPLE-COUNT-INDEX))))
         (let ([mk-tuple-type-glb (get-mk-tuple-type-glb! mk-glb)])
           (if (Var? t1)
               (invoke-mk-tuple-type-glb mk-tuple-type-glb t1 t2)
               (begin$
                 (assign$ tuple-type1 t1)
                 (invoke-mk-tuple-type-glb mk-tuple-type-glb tuple-type1 t2))))]
        [(Type-GRef e) (sr-type-gref recur e)]
        [(Type-GVect e) (sr-type-gvect recur e)]
        [(Type-MRef e) (sr-type-mref recur e)]
        [(Type-MRef-Huh (app recur e))
         (sr-check-tag=? e TYPE-TAG-MASK TYPE-MREF-TAG)]
        [(Type-MRef-Of (app recur e))
         (define arg : D0-Expr
           (Op 'binary-xor (list e TYPE-MREF-TAG)))
         (Op 'Array-ref (list arg TYPE-MREF-TYPE-INDEX))]
        [(Error (app recur e))
         (Begin
          (list (Op 'Print (list e))
                (Op 'Exit  (list (Quote -1))))
          UNDEF-IMDT)]
        [(Create-tuple (app recur* e*))
         (sr-alloc "tuple" #f
                   (map (lambda ([e : D0-Expr])
                          (cons "element" e))
                        e*))]
        [(Copy-Tuple (app recur n) (app recur v))
         (begin$
           (assign$ new-tpl (Op 'Alloc (list n)))
           (repeat$ (i ZERO-IMDT n) (_ UNIT-IMDT)
                    (begin$
                      (assign$ val (Op 'Array-ref (list v i)))
                      (Op 'Array-set! (list new-tpl i val))))
           new-tpl)]
        [(Tuple-proj (app recur e) (app recur i))
         (Op 'Array-ref (list e i))]
        [(Tuple-Coercion-Huh (app recur e))
         (begin$
           (assign$
            tmp-crcn
            (sr-tagged-array-ref
             e COERCION-MEDIATING-TAG COERCION-TUPLE-COUNT-INDEX))
           (assign$ crcn-tag (Op 'binary-and `(,tmp-crcn ,COERCION-TAG-MASK)))
           (Op '= (list crcn-tag COERCION-TUPLE-SECOND-TAG)))]
        [(Tuple-Coercion-Num (app recur e))
         (begin$
           (assign$
            tagged-count
            (sr-tagged-array-ref
             e COERCION-MEDIATING-TAG COERCION-TUPLE-COUNT-INDEX))
           (Op '%>> (list tagged-count COERCION-SECOND-TAG-SHIFT)))]
        [(Tuple-Coercion-Item (app recur e) (app recur i))
         (sr-tagged-array-ref
          e COERCION-MEDIATING-TAG (sr-plus COERCION-TUPLE-ELEMENTS-OFFSET i))]
        [(Type-Tuple-Huh (app recur e))
         (sr-check-tag=? e TYPE-TAG-MASK TYPE-TUPLE-TAG)]
        [(Type-Tuple-num (app recur e))
         (sr-tagged-array-ref e TYPE-TUPLE-TAG TYPE-TUPLE-COUNT-INDEX)]
        [(Type-Tuple-item (app recur e) (app recur i))
         (sr-tagged-array-ref
          e TYPE-TUPLE-TAG (sr-plus TYPE-TUPLE-ELEMENTS-OFFSET i))]
        [(Coerce-Tuple uid (app recur v) (app recur c))
         (: invoke-coerce-tuple ((Code-Label Uid) (Var Uid) D0-Expr -> D0-Expr))
         (define (invoke-coerce-tuple coerce-tuple v c)
           (begin$
             (assign$
              tagged-count
              (sr-tagged-array-ref
               c COERCION-MEDIATING-TAG COERCION-TUPLE-COUNT-INDEX))
             (App-Code
              coerce-tuple
              (list v c ZERO-IMDT
                    (Op '%>> (list tagged-count COERCION-SECOND-TAG-SHIFT))))))
         (let ([coerce-tuple (get-coerce-tuple! uid)])
           (if (Var? v)
               (invoke-coerce-tuple coerce-tuple v c)
               (begin$
                 (assign$ tuple-val1 v)
                 (invoke-coerce-tuple coerce-tuple tuple-val1 c))))]
        [(Coerce-Tuple-In-Place uid (app recur v) (app recur c) (app recur mono-type))
         (: invoke-coerce-tuple ((Code-Label Uid) (Var Uid) D0-Expr D0-Expr -> D0-Expr))
         (define (invoke-coerce-tuple coerce-tuple v c a)
           (App-Code
            coerce-tuple
            (list v c a)))
         (let ([coerce-tuple (get-coerce-tuple-in-place! uid)])
           (if (Var? v)
               (invoke-coerce-tuple coerce-tuple v c mono-type)
               (begin$
                 (assign$ tuple-val1 v)
                 (invoke-coerce-tuple coerce-tuple tuple-val1 c mono-type))))]
        [(Cast-Tuple-In-Place
          uid (app recur v) (app recur t1) (app recur t2) (app recur lbl)
          (app recur mono-address))
         (: invoke-cast-tuple
            ((Code-Label Uid) (Var Uid) D0-Expr D0-Expr D0-Expr D0-Expr
                              -> D0-Expr))
         (define (invoke-cast-tuple cast-tuple v t1 t2 lbl a)
           (App-Code cast-tuple (list v t1 t2 lbl a)))
         (let ([cast-tuple (get-cast-tuple-in-place! uid)])
           (if (Var? v)
               (invoke-cast-tuple cast-tuple v t1 t2 lbl mono-address)
               (begin$
                 (assign$ tuple-val1 v)
                 (invoke-cast-tuple cast-tuple tuple-val1 t1 t2 lbl mono-address))))]
        [(Cast-Tuple uid (app recur v) (app recur t1) (app recur t2) (app recur lbl))
         (: invoke-cast-tuple ((Code-Label Uid) (Var Uid) D0-Expr D0-Expr D0-Expr -> D0-Expr))
         (define (invoke-cast-tuple cast-tuple v t1 t2 lbl)
           (begin$
             (assign$
              tagged-count
              (sr-tagged-array-ref t2 TYPE-TUPLE-TAG TYPE-TUPLE-COUNT-INDEX))
             (App-Code cast-tuple (list v t1 t2 lbl ZERO-IMDT tagged-count))))
         (let ([cast-tuple (get-cast-tuple! uid)])
           (if (Var? v)
               (invoke-cast-tuple cast-tuple v t1 t2 lbl)
               (begin$
                 (assign$ tuple-val1 v)
                 (invoke-cast-tuple cast-tuple tuple-val1 t1 t2 lbl))))]
        [(Make-Tuple-Coercion mk-crcn (app recur t1) (app recur t2) (app recur l))
         (: invoke-mk-tuple-crcn ((Code-Label Uid) (Var Uid) D0-Expr D0-Expr -> D0-Expr))
         (define (invoke-mk-tuple-crcn mk-tuple-crcn t1 t2 l)
           (App-Code
            mk-tuple-crcn
            (list t1 t2 l ZERO-IMDT
                  (sr-tagged-array-ref t2 TYPE-TUPLE-TAG TYPE-TUPLE-COUNT-INDEX))))
         (let ([mk-tuple-crcn (get-mk-tuple-crcn! mk-crcn)])
           (if (Var? t1)
               (invoke-mk-tuple-crcn mk-tuple-crcn t1 t2 l)
               (begin$
                 (assign$ tuple-type1 t1)
                 (invoke-mk-tuple-crcn mk-tuple-crcn tuple-type1 t2 l))))]
        [(Compose-Tuple-Coercion compose (app recur c1) (app recur c2))
         (: invoke-comp ((Code-Label Uid) (Var Uid) D0-Expr -> D0-Expr))
         (define (invoke-comp comp-tuple c1 c2)
           (begin$
             (assign$
              tagged-count
              (sr-tagged-array-ref
               c2 COERCION-MEDIATING-TAG COERCION-TUPLE-COUNT-INDEX))
             (assign$
              count
              (Op '%>> (list tagged-count COERCION-SECOND-TAG-SHIFT)))
             (App-Code comp-tuple (list c1 c2 ZERO-IMDT count TRUE-IMDT))))
         (let ([mk-tuple-crcn (get-comp-tuple-crcn! compose)])
           (if (Var? c1)
               (invoke-comp mk-tuple-crcn c1 c2)
               (begin$
                 (assign$ tuple-crcn1 c1)
                 (invoke-comp mk-tuple-crcn tuple-crcn1 c2))))]
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
    (Op 'Array-set! (list mem (Quote offset) value)))
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
  (define alloc-id  (track-next-uid!$ name))
  (define alloc-var (Var alloc-id))
  (define alloc-ass (Assign alloc-id (Op 'Alloc `(,(Quote size)))))
  (define set* (map (sr-alloc-init alloc-var) ind* var*)) 
  (define tag-return : D0-Expr
    (cond
      [(not tag?) alloc-var]
      [else
       (match tag?
         [(Quote 0) alloc-var]
         [tag (Op 'binary-or (list alloc-var tag))])]))
  (define result (Begin (append ass* (cons alloc-ass set*)) tag-return))
  (debug off name tag? slots result))

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


;; The following functions specify the types runtime layout,
;; parameterized on whether the type is known in compile-time or
;; run-time

(: sr-type-gref (All (A) (A -> D0-Expr) A -> D0-Expr))
(define (sr-type-gref compile-type-static/dynamic ty)
  (sr-alloc "GRefT" TYPE-GREF-TAG
            `(("type" . ,(compile-type-static/dynamic ty)))))

(: sr-type-gvect (All (A) (A -> D0-Expr) A -> D0-Expr))
(define (sr-type-gvect compile-type-static/dynamic ty)
  (sr-alloc "GVectT" TYPE-GVECT-TAG
            `(("type" . ,(compile-type-static/dynamic ty)))))

(: sr-type-mref (All (A) (A -> D0-Expr) A -> D0-Expr))
(define (sr-type-mref compile-type-static/dynamic ty)
  (sr-alloc "MRefT" TYPE-MREF-TAG
            `(("type" . ,(compile-type-static/dynamic ty)))))

(: sr-type-mvect (All (A) (A -> D0-Expr) A -> D0-Expr))
(define (sr-type-mvect compile-type-static/dynamic ty)
  (sr-alloc "MVectT" TYPE-MVECT-TAG
            `(("type" . ,(compile-type-static/dynamic ty)))))

(: allocate-bound-type (CoC6-Bnd-Type -> D0-Expr))
(define (allocate-bound-type bnd)
  (: sr-type (Compact-Type -> D0-Expr))
  (define (sr-type t)
    (match t
      [(GRef t) (sr-type-gref sr-prim-type t)]
      [(GVect t) (sr-type-gvect sr-prim-type t)]
      [(MRef t) (sr-type-mref sr-prim-type t)]
      [(MVect t) (sr-type-mvect sr-prim-type t)]
      [(Fn a f* r)
       (sr-alloc "Fun_Type" TYPE-FN-TAG
                 `(("arity" . ,(Quote a))
                   ("return" . ,(sr-prim-type r)) .
                   ,(map (lambda ([t : Immediate-Type])
                           (cons "argument" (sr-prim-type t)))
                         f*)))]
      [(STuple n a*)
       (sr-alloc "Tuple_Type" TYPE-TUPLE-TAG
                 `(("num" . ,(Quote n))
                   .
                   ,(map (lambda ([t : Immediate-Type])
                           (cons "argument" (sr-prim-type t)))
                         a*)))]
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
     (sr-alloc "inject-coercion" COERCION-INJECT-TAG
               `(("type" . ,t)))]
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
    [(Sequence (app sr-immediate-coercion f)
               (app sr-immediate-coercion s))
     (sr-alloc "sequence_coecion" COERCION-SEQUENCE-TAG
               `(("first" . ,f) (,"second" . ,s)))]
    [(Fn l a* (app sr-immediate-coercion r))
     (define len : Index (length a*))
     (unless (= l len)
       (error 'sr-coercion "length mismatch"))
     (begin$
       (assign$
        second-tag
        (Op '+ (list (Op '%<< (list (Quote l) COERCION-SECOND-TAG-SHIFT))
                     COERCION-FN-SECOND-TAG)))
       (sr-alloc "fn_coercion" COERCION-MEDIATING-TAG
                 `(("arity"  . ,second-tag)
                   ("return" . ,r) .
                   ,(map (lambda ([a : Immediate-Coercion])
                           (cons "argument" (sr-immediate-coercion a)))
                         a*))))]
    [(Ref (app sr-immediate-coercion r) (app sr-immediate-coercion w))
     (begin$
       (assign$
        second-tag
        (Op '+ (list (Op '%<< (list ZERO-IMDT COERCION-SECOND-TAG-SHIFT))
                     COERCION-REF-SECOND-TAG)))
       (sr-alloc "ref-coercion" COERCION-MEDIATING-TAG
                 `(("tag" . ,second-tag)
                   ("read-coercion" . ,r)
                   ("write-coercion" . ,w))))]
    [(MonoRef (app sr-prim-type t))
     (begin$
       (assign$
        second-tag
        (Op '+ (list (Op '%<< (list ZERO-IMDT COERCION-SECOND-TAG-SHIFT))
                     COERCION-MREF-SECOND-TAG)))
       (sr-alloc "mref-coercion" COERCION-MEDIATING-TAG
                 `(("tag" . ,second-tag)
                   ("type" . ,t))))]
    [(MonoVect (app sr-prim-type t))
     (begin$
       (assign$
        second-tag
        (Op '+ (list (Op '%<< (list ZERO-IMDT COERCION-SECOND-TAG-SHIFT))
                     COERCION-MVECT-SECOND-TAG)))
       (sr-alloc "mvect-coercion" COERCION-MEDIATING-TAG
                 `(("tag" . ,second-tag)
                   ("type" . ,t))))]
    [(CTuple l a*)
     (define len : Index (length a*))
     (unless (= l len)
       (error 'sr-coercion "length mismatch"))
     (begin$
       (assign$
        second-tag
        (Op '+ (list (Op '%<< (list (Quote (length a*))
                                    COERCION-SECOND-TAG-SHIFT))
                     COERCION-TUPLE-SECOND-TAG)))
       (sr-alloc "tuple_coercion" COERCION-MEDIATING-TAG
                 `(("num"  . ,second-tag)
                   .
                   ,(map (lambda ([a : Immediate-Coercion])
                           (cons "item" (sr-immediate-coercion a)))
                         a*))))]
    [(Failed l)
     (sr-alloc "failed-coercion" COERCION-FAILED-TAG
               `(("label" . ,(Quote l))))]
    [other (error 'specify-representation/type "unmatched ~a" other)]))

(: allocate-bound-coercion : CoC6-Bnd-Crcn -> D0-Expr)
(define (allocate-bound-coercion bnd)
  (match-define (cons u c) bnd)
  (Assign u (sr-coercion c)))

(: untag-deref-gproxy (-> D0-Expr (-> D0-Expr D0-Expr)))
(define ((untag-deref-gproxy index) proxy)
  (Op 'Array-ref
      (list (Op 'binary-xor (list proxy GPROXY-TAG))
            index)))

(: alloc-tag-set-gproxy/twosome
   ((String -> Uid) D0-Expr D0-Expr D0-Expr D0-Expr -> D0-Expr))
(define (alloc-tag-set-gproxy/twosome next-uid! ref-e src-e tar-e lbl-e)
  ;; TODO Consider using sr-alloc here
  (begin$
    (assign$ ref ref-e)
    (assign$ src src-e)
    (assign$ tar tar-e)
    (assign$ lbl lbl-e)
    (assign$ proxy (Op 'Alloc (list GPROXY/TWOSOME-SIZE)))
    (Op 'Array-set! (list proxy GPROXY-FOR-INDEX ref))
    (Op 'Array-set! (list proxy GPROXY-FROM-INDEX src))
    (Op 'Array-set! (list proxy GPROXY-TO-INDEX tar))
    (Op 'Array-set! (list proxy GPROXY-BLAMES-INDEX lbl))
    (Op 'binary-or (list proxy GPROXY-TAG))))

(: alloc-tag-set-gproxy/coercion
   ((String -> Uid) D0-Expr D0-Expr -> D0-Expr))
(define (alloc-tag-set-gproxy/coercion next-uid! ref-e crcn-e)
  (begin$
    (assign$ ref ref-e)
    (assign$ crcn crcn-e)
    (assign$ proxy (Op 'Alloc (list GPROXY/COERCION-SIZE)))
    (Op 'Array-set! (list proxy GPROXY-FOR-INDEX ref))
    (Op 'Array-set! (list proxy GPROXY-COERCION-INDEX crcn))
    (Op 'binary-or (list proxy GPROXY-TAG))))

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
      [(Int? t) (Op 'Printf (list (Quote "Int : %ld\n") id))]
      ;; This is a fragile hack
      ;; switch to typed ASTs
      [(Float? t) (Begin (list (Op 'Print (list (Quote "Float : ")))
                               (Op 'print-float (list id (Quote 9))))
                         (Op 'Print (list (Quote "\n"))))]
      [(Character? t)
       (Begin (list (Op 'Print (list (Quote "Char : ")))
                    (Op 'print-char (list id)))
              (Op 'Print (list (Quote "\n"))))]
      [(Bool? t)
       (If id
           (Op 'Print (list (Quote "Bool : #t\n")))
           (Op 'Print (list (Quote "Bool : #f\n"))))]
      [(Unit? t) (Op 'Print (list (Quote "Unit : ()\n")))]
      [(Fn? t) (Op 'Print (list (Quote "Function : ?\n")))]
      [(GRef? t) (Op 'Print (list (Quote "GReference : ?\n")))]
      [(GVect? t) (Op 'Print (list (Quote "GVector : ?\n")))]
      [(MRef? t) (Op 'Print (list (Quote "MReference : ?\n")))]
      [(MVect? t) (Op 'Print (list (Quote "MVector : ?\n")))]
      [(STuple? t) (Op 'Print (list (Quote "Tuple : ?\n")))]
      [(Dyn? t) (Op 'Print (list (Quote "Dynamic : ?\n")))]
      [else (error 'sr-observe "printing other things")]))
  (begin$ (assign$ res e)
          (generate-print res t)))

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
                    (values fvar (Op 'Array-ref (list closv (Quote i)))))]
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
    (lambda ([c : Uid] [f : Uid]) : Nat
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
             [rhs  (Op 'Alloc `(,(Quote size)))]
             [set*  (for/list : (Listof D0-Expr)
                      ([d : D0-Expr data]
                       [i : Integer (in-naturals)])
                      (Op 'Array-set! (list clos (Quote i) d)))])
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
(define (sr-clos-ref-code clos)
  (Op 'Array-ref  (list clos CLOS-CODE-INDEX)))

(: sr-clos-ref-caster (-> D0-Expr D0-Expr))
(define (sr-clos-ref-caster clos)
  (Op 'Array-ref  (list clos CLOS-CSTR-INDEX)))

(define-type Env (HashTable Uid D0-Expr))

(define-syntax-rule (extend e k v)
  (hash-set e k v))

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
(define (lookup e u)
  (hash-ref e u (lookup-error e u)))

(define (lookup-error e u)
  (lambda ()
    (error 'specify-representation/lookup
           "Unbound uid ~a\n\tin program with env ~a" u e)))

(define-type Triv (U (Quote String) (Quote Integer) (Code-Label Uid) (Var Uid)))
(define-predicate triv? Triv)

(: rename (-> String (-> Any String)))
(define (rename name)
  (lambda (_) name))

(define sr-quote : (D0-Literal -> D0-Expr) Quote)

(define (empty-index-map u i)
  (error 'specify-representation "attempt to index without index map ~a ~a" u i))

(: sr-tagged-array-ref (D0-Expr D0-Expr D0-Expr -> D0-Expr))
(define (sr-tagged-array-ref e t i)
  (sr-array-ref (sr-untag e t) i))

(: sr-tagged-array-set! (D0-Expr D0-Expr D0-Expr D0-Expr -> D0-Expr))
(define (sr-tagged-array-set! e t i v)
  (sr-array-set! (sr-untag e t) i v))

(: sr-array-ref (D0-Expr D0-Expr -> D0-Expr))
(define (sr-array-ref e i)
  (Op 'Array-ref (list e i)))

(: sr-array-set! (D0-Expr D0-Expr D0-Expr -> D0-Expr))
(define (sr-array-set! e i v)
  (Op 'Array-set! (list e i v)))

(: sr-untag (D0-Expr D0-Expr -> D0-Expr))
(define (sr-untag e t)
  (Op 'binary-xor `(,e ,t)))

(: unmask-rest : D0-Expr D0-Expr -> D0-Expr)
(define (unmask-rest e m)
  (Op 'binary-and `(,e ,(Op 'binary-not `(,m)))))

(: tagged-array-ref : D0-Expr D0-Expr D0-Expr -> D0-Expr)
(define (tagged-array-ref e tm i)
  (sr-array-ref (unmask-rest e tm) i))

(: check-tag? : D0-Expr D0-Expr -> D0-Expr)
(define (check-tag? e m)
  (Op 'not `(,(Op '= `(,ZERO-IMDT ,(Op 'binary-and `(,e ,m)))))))

;; there is some naming conflicts that must be taken care of
;; in this file
(: sr-tag-value (D0-Expr D0-Expr -> D0-Expr))
(define (sr-tag-value e t)
  (Op 'binary-or `(,e ,t)))

(: sr-check-tag=? (D0-Expr D0-Expr D0-Expr -> D0-Expr))
(define (sr-check-tag=? e mask tag)
  (Op '= `(,(Op 'binary-and `(,e ,mask)) ,tag)))

(: sr-plus (D0-Expr D0-Expr -> D0-Expr))
(define (sr-plus f s)
  (Op '+ (list f s)))
