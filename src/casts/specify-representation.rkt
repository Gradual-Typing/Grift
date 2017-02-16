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
         )

;; Only the pass is provided by this module
(provide
 (all-from-out
  "../language/cast-or-coerce6.rkt"
  "../language/data0.rkt")
 specify-representation)

(: specify-representation (Cast-or-Coerce6-Lang -> Data0-Lang))
(define (specify-representation prgm)
  (match-let ([(Prog (list name next type) (Let-Static* bndt* bnd-crcn* exp)) prgm])
    (let* ([next       : (Boxof Nat) (box next)]
           [bnd-code*  : (Boxof D0-Bnd-Code*) (box '())]
           [exp        : D0-Expr (sr-expr next bnd-code* (hash) empty-index-map exp)]
           [init-type* : D0-Expr* (map (sr-bndt next) bndt*)]
           [type-id*   : Uid*     (map (inst car Uid Any) bndt*)]
           [init-crcn* : D0-Expr* (map (sr-bnd-coercion next) bnd-crcn*)]
           [crcn-id*   : Uid*     (map (inst car Uid Any) bnd-crcn*)]
           [next       : Nat (unbox next)]
           [bnd-code*  : D0-Bnd-Code* (unbox bnd-code*)])
      (debug
       (Prog (list name next type)
        (GlobDecs (append type-id* crcn-id*)
         (Labels bnd-code*
          (Begin (append init-type* init-crcn*) exp))))))))
              
;; Env must be maintained as a mapping from uids to how to access those
;; values. This is important because uid references to variable inside a
;; closure must turn into memory loads.

(define-type IndexMap (Uid Uid -> Nat))

(: sr-expr ((Boxof Nat) (Boxof D0-Bnd-Code*) Env IndexMap CoC6-Expr -> D0-Expr))
(define (sr-expr next new-code env cenv exp)
  ;; This is the only piece of code that should touch the unique counter
  (: next-uid! (String -> Uid))
  (define (next-uid! x)
    (let ([n (unbox next)])
      (set-box! next (add1 n))
      (Uid x n)))
  
  (: add-new-code! (D0-Bnd-Code -> Void))
  (define (add-new-code! b)
    (set-box! new-code (cons b (unbox new-code))))

  (: mk-fn-type-code-label? (Boxof (Option (Code-Label Uid))))
  (define mk-fn-type-code-label? (box #f))
 
 (: mk-tuple-type-code-label? (Boxof (Option (Code-Label Uid))))
  (define mk-tuple-type-code-label? (box #f))

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
  

  (: sr-alloc (String Fixnum (Listof (Pair String D0-Expr)) -> D0-Expr))
  (define sr-alloc (sr-alloc/next next))

  
  ;; The way that boxed immediate work currently bothers me.
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
         [else (sr-alloc "dynamic_boxed" #b000
                         `(("value" . ,e1)
                           ("type" . ,(sr-expr e2))))])]
      [else
       (define val (next-uid! "value")) 
       (define type   (next-uid! "type"))
       (define tag    (next-uid! "tag"))
       (define imm    (next-uid!  "imm"))
       (define val-var (Var val))
       (define type-var (Var type))
       (define tag-var (Var tag))
       (define imm-var (Var imm))
       (define shifted-imm (Op '%<< (list val-var DYN-IMDT-SHIFT)))
       (define (tag-shifted-imm [tag : D0-Expr]) : D0-Expr
         (Op 'binary-or `(,shifted-imm ,tag)))
       (Begin
         (list
          (Assign val  e1)
          (Assign type (sr-expr e2))
          (Assign tag (Op 'binary-and `(,type-var ,TYPE-TAG-MASK))))
         ;; TODO use a switch here!
         (Switch type-var
          `([(,data:TYPE-INT-RT-VALUE)  . ,(tag-shifted-imm DYN-INT-TAG)]
            [(,data:TYPE-BOOL-RT-VALUE) . ,(tag-shifted-imm DYN-BOOL-TAG)]
            [(,data:TYPE-UNIT-RT-VALUE) . ,(tag-shifted-imm DYN-UNIT-TAG)]
            [(,data:TYPE-CHAR-RT-VALUE) . ,(tag-shifted-imm DYN-CHAR-TAG)])
          ;; Notice that float types fall into this case also
           (sr-alloc "dynamic_boxed" data:DYN-BOXED-TAG
                     `(("" . ,val-var) ("" . ,type-var))))
         #;
         (If (Op '= `(,tag-var ,TYPE-ATOMIC-TAG))
             (If (Op '= `(,type-var ,TYPE-INT-RT-VALUE))
                 (Op 'binary-or `(,(Op '%<< (list val-var DYN-IMDT-SHIFT)) ,DYN-INT-TAG))
                 (If (Op '= `(,type-var ,TYPE-BOOL-RT-VALUE))
                     (Op 'binary-or `(,(Op '%<< (list val-var DYN-IMDT-SHIFT)) ,DYN-BOOL-TAG))
                     (If (Op '= `(,type-var ,TYPE-UNIT-RT-VALUE))
                         (Op 'binary-or `(,(Op '%<< (list val-var DYN-IMDT-SHIFT)) ,DYN-UNIT-TAG))
                         (sr-alloc "float_dynamic_boxed" data:DYN-BOXED-TAG
                                   `(("" . ,val-var) ("" . ,type-var))))))
             (sr-alloc "dynamic_boxed" data:DYN-BOXED-TAG
                       `(("" . ,val-var) ("" . ,type-var)))))]))
  
  (: get-mk-tuple-type! (Uid -> (Code-Label Uid)))
  (define (get-mk-tuple-type! mk-glb)
    (: make-code! (Uid -> (Code-Label Uid)))
    (define (make-code! mk-glb)
      (define mk-tuple-u (next-uid! "make-tuple-type"))
      (define t1-u    (next-uid! "tuple-type1"))
      (define t2-u    (next-uid! "tuple-type2"))
      (define i-u     (next-uid! "index"))
      (define a-u     (next-uid! "tuple-type-num"))
      (define t1a-u   (next-uid! "tuple-type1-argument"))
      (define t2a-u   (next-uid! "tuple-type2-argument"))
      (define c1-u    (next-uid! "resulting_type"))
      (define c2-u    (next-uid! "resulting_type"))
      (define ca-u    (next-uid! "argument-type"))
      (define t1      (Var t1-u))
      (define t2      (Var t2-u))
      (define c1      (Var c1-u))
      (define c2      (Var c2-u))
      (define i       (Var i-u))
      (define a       (Var a-u))
      (define mk-tuple   (Code-Label mk-tuple-u))
      (define mk-g    (Code-Label mk-glb))
      (define mk-tuple-t : D0-Code
        (Code `(,t1-u ,t2-u ,i-u ,a-u)
          (If (Op '= `(,i ,a))
              (Begin
                (list
                 (Assign c1-u (Op 'Alloc (list (sr-plus a (Quote 1)))))
                 (sr-array-set! c1 TYPE-TUPLE-COUNT-INDEX a))
                (sr-tag-value c1 TYPE-TUPLE-TAG))
              (Begin
                (list
                 (Assign t1a-u (sr-tagged-array-ref t1 TYPE-TUPLE-TAG (sr-plus TYPE-TUPLE-ELEMENTS-OFFSET i)))
                 (Assign t2a-u (sr-tagged-array-ref t2 TYPE-TUPLE-TAG (sr-plus TYPE-TUPLE-ELEMENTS-OFFSET i)))
                 (Assign ca-u (App-Code mk-g `(,(Var t1a-u) ,(Var t2a-u))))
                 (Assign c2-u (App-Code mk-tuple `(,t1 ,t2 ,(sr-plus (Quote 1) i) ,a)))
                 (sr-tagged-array-set! c2 TYPE-TUPLE-TAG (sr-plus TYPE-TUPLE-ELEMENTS-OFFSET i) (Var ca-u)))
                c2))))
      (add-new-code! (cons mk-tuple-u mk-tuple-t))
      (set-box! mk-tuple-type-code-label? mk-tuple)
      mk-tuple)
    (let ([cl? (unbox mk-tuple-type-code-label?)])
      (or cl? (make-code! mk-glb))))

  
  (: get-mk-fn-type! (Uid -> (Code-Label Uid)))
  (define (get-mk-fn-type! mk-glb)
    (: make-code! (Uid -> (Code-Label Uid)))
    (define (make-code! mk-glb)
      (define mk-fn-u (next-uid! "make-fn-type"))
      (define t1-u    (next-uid! "fn-type1"))
      (define t2-u    (next-uid! "fn-type2"))
      (define i-u     (next-uid! "index"))
      (define a-u     (next-uid! "fn-type-arity"))
      (define t1r-u   (next-uid! "fn-type1-return"))
      (define t2r-u   (next-uid! "fn-type2-return"))
      (define t1a-u   (next-uid! "fn-type1-argument"))
      (define t2a-u   (next-uid! "fn-type2-argument"))
      (define c1-u    (next-uid! "resulting_type"))
      (define c2-u    (next-uid! "resulting_type"))
      (define cr-u    (next-uid! "return-type"))
      (define ca-u    (next-uid! "argument-type"))
      (define t1      (Var t1-u))
      (define t2      (Var t2-u))
      (define c1      (Var c1-u))
      (define c2      (Var c2-u))
      (define i       (Var i-u))
      (define a       (Var a-u))
      (define mk-fn   (Code-Label mk-fn-u))
      (define mk-g    (Code-Label mk-glb))
      ;; This code is carfully crafted so that the allocation occurs after
      ;; all of the coercion have been made this ensures that there isn't the
      ;; possibility of collecting while there is unitialized data in the heap.
      (define mk-fn-t : D0-Code
        (Code `(,t1-u ,t2-u ,i-u ,a-u)
          (If (Op '= `(,i ,a))
              (Begin
                (list
                 (Assign t1r-u (sr-tagged-array-ref t1 TYPE-FN-TAG TYPE-FN-RETURN-INDEX))
                 (Assign t2r-u (sr-tagged-array-ref t2 TYPE-FN-TAG TYPE-FN-RETURN-INDEX))
                 (Assign cr-u (App-Code mk-g `(,(Var t1r-u) ,(Var t2r-u))))
                 (Assign c1-u (Op 'Alloc (list (sr-plus a (Quote 2)))))
                 (sr-array-set! c1 TYPE-FN-ARITY-INDEX a)
                 (sr-array-set! c1 TYPE-FN-RETURN-INDEX (Var cr-u)))
                (sr-tag-value c1 TYPE-FN-TAG))
              (Begin
                (list
                 (Assign t1a-u (sr-tagged-array-ref t1 TYPE-FN-TAG (sr-plus TYPE-FN-FMLS-OFFSET i)))
                 (Assign t2a-u (sr-tagged-array-ref t2 TYPE-FN-TAG (sr-plus TYPE-FN-FMLS-OFFSET i)))
                 (Assign ca-u (App-Code mk-g `(,(Var t2a-u) ,(Var t1a-u))))
                 (Assign c2-u (App-Code mk-fn `(,t1 ,t2 ,(sr-plus (Quote 1) i) ,a)))
                 (sr-tagged-array-set! c2 TYPE-FN-TAG (sr-plus TYPE-FN-FMLS-OFFSET i) (Var ca-u)))
                c2))))
      (add-new-code! (cons mk-fn-u mk-fn-t))
      (set-box! mk-fn-type-code-label? mk-fn)
      mk-fn)
    (let ([cl? (unbox mk-fn-type-code-label?)])
      (or cl? (make-code! mk-glb))))

  (: get-mk-fn-crcn! (Uid -> (Code-Label Uid)))
  (define (get-mk-fn-crcn! mk-crcn)
    (: make-code! (Uid -> (Code-Label Uid)))
    (define (make-code! mk-crcn)
      (define mk-fn-u (next-uid! "make-fn-coercion"))
      (define t1-u    (next-uid! "fn-type1"))
      (define t2-u    (next-uid! "fn-type2"))
      (define l-u     (next-uid! "label"))
      (define i-u     (next-uid! "index"))
      (define a-u     (next-uid! "fn-type-arity"))
      (define t1r-u   (next-uid! "fn-type1-return"))
      (define t2r-u   (next-uid! "fn-type2-return"))
      (define t1a-u   (next-uid! "fn-type1-argument"))
      (define t2a-u   (next-uid! "fn-type2-argument"))
      (define c1-u    (next-uid! "resulting_coercion"))
      (define c2-u    (next-uid! "resulting_coercion"))
      (define cr-u    (next-uid! "return-coercion"))
      (define ca-u    (next-uid! "argument-coercion"))
      (define st-u    (next-uid! "second-tagged"))
      (define t1      (Var t1-u))
      (define t2      (Var t2-u))
      (define l       (Var l-u))
      (define c1      (Var c1-u))
      (define c2      (Var c2-u))
      (define i       (Var i-u))
      (define a       (Var a-u))
      (define st      (Var st-u))
      (define mk-fn   (Code-Label mk-fn-u))
      (define mk-c    (Code-Label mk-crcn))
      ;; This code is carfully crafted so that the allocation occurs after
      ;; all of the coercion have been made this ensures that there isn't the
      ;; possibility of collecting while there is unitialized data in the heap.
      (define mk-fn-c : D0-Code
        (Code `(,t1-u ,t2-u ,l-u ,i-u ,a-u)
          (If (Op '= `(,i ,a))
              (Begin
                (list
                 (Assign t1r-u (sr-tagged-array-ref t1 TYPE-FN-TAG TYPE-FN-RETURN-INDEX))
                 (Assign t2r-u (sr-tagged-array-ref t2 TYPE-FN-TAG TYPE-FN-RETURN-INDEX))
                 (Assign cr-u  (App-Code mk-c `(,(Var t1r-u) ,(Var t2r-u) ,l)))
                 (Assign c1-u  (Op 'Alloc (list (sr-plus a (Quote 2)))))
                 (Assign st-u  (Op '+ (list (Op '%<< (list a COERCION-SECOND-TAG-SHIFT))
                                            COERCION-FN-SECOND-TAG)))
                 (sr-array-set! c1 COERCION-FN-ARITY-INDEX st)
                 (sr-array-set! c1 COERCION-FN-RETURN-INDEX (Var cr-u)))
                (sr-tag-value c1 COERCION-MEDIATING-TAG))
              (Begin
                (list
                 (Assign t1a-u (sr-tagged-array-ref t1 TYPE-FN-TAG (sr-plus TYPE-FN-FMLS-OFFSET i)))
                 (Assign t2a-u (sr-tagged-array-ref t2 TYPE-FN-TAG (sr-plus TYPE-FN-FMLS-OFFSET i)))
                 (Assign ca-u  (App-Code mk-c `(,(Var t2a-u) ,(Var t1a-u) ,l)))
                 (Assign c2-u  (App-Code mk-fn `(,t1 ,t2 ,l ,(sr-plus (Quote 1) i) ,a)))
                 (sr-tagged-array-set! c2 COERCION-MEDIATING-TAG (sr-plus COERCION-FN-FMLS-OFFSET i)
                                       (Var ca-u)))
                c2))))
      (add-new-code! (cons mk-fn-u mk-fn-c))
      (set-box! mk-fn-coercion-code-label? mk-fn)
      mk-fn)
    (let ([cl? (unbox mk-fn-coercion-code-label?)])
      (or cl? (make-code! mk-crcn))))

  (: get-comp-fn-crcn! (Uid -> (Code-Label Uid)))
  (define (get-comp-fn-crcn! comp-crcn)
    (: make-code! (Uid -> (Code-Label Uid)))
    (define (make-code! mk-crcn)
      (define comp-fn-u (next-uid! "compose-fn-coercion"))
      (define c1-u      (next-uid! "fn-coercion1"))
      (define c2-u      (next-uid! "fn-coercion2"))
      (define c3-u      (next-uid! "result-coercion"))
      (define c4-u      (next-uid! "result-coercion"))
      (define i-u       (next-uid! "index"))
      (define a-u       (next-uid! "fn-coercion-arity"))
      (define c1r-u     (next-uid! "fn-coercion1-return"))
      (define c2r-u     (next-uid! "fn-coercion2-return"))
      (define c1a-u     (next-uid! "fn-coercion1-argument"))
      (define c2a-u     (next-uid! "fn-coercion2-argument"))
      (define cr-u      (next-uid! "return-coercion"))
      (define ca-u      (next-uid! "argument-coercion"))
      (define id?1-u    (next-uid! "is_identity"))
      (define id?2-u    (next-uid! "is_still_identity"))
      (define st-u      (next-uid! "secondary-tagged"))
      (define c1        (Var c1-u))
      (define c2        (Var c2-u))
      (define c3        (Var c3-u))
      (define c4        (Var c4-u))
      (define cr        (Var cr-u))
      (define ca        (Var ca-u))
      (define i         (Var i-u))
      (define a         (Var a-u))
      (define id?1      (Var id?1-u))
      (define id?2      (Var id?2-u))
      (define st        (Var st-u))
      (define comp-fn   (Code-Label comp-fn-u))
      (define comp-c    (Code-Label comp-crcn))
      ;; This code is carfully crafted so that the allocation occurs after
      ;; all of the coercion have been made this ensures that there isn't the
      ;; possibility of collecting while there is unitialized data in the heap.
      (define comp-fn-c : D0-Code
        (Code `(,c1-u ,c2-u ,i-u ,a-u ,id?1-u)
          (If (Op '= `(,i ,a))
              (Begin
                (list
                 (Assign c1r-u (sr-tagged-array-ref c1 COERCION-MEDIATING-TAG COERCION-FN-RETURN-INDEX))
                 (Assign c2r-u (sr-tagged-array-ref c2 COERCION-MEDIATING-TAG COERCION-FN-RETURN-INDEX))
                 (Assign cr-u  (App-Code comp-c `(,(Var c1r-u) ,(Var c2r-u)))))
                (If (If id?1
                        (sr-check-tag=? cr COERCION-TAG-MASK COERCION-IDENTITY-TAG)
                        FALSE-IMDT)
                    COERCION-IDENTITY-IMDT
                    (Begin
                      (list
                       (Assign c3-u (Op 'Alloc (list (sr-plus a (Quote 2)))))
                       (Assign st-u (Op '+ (list (Op '%<< (list a COERCION-SECOND-TAG-SHIFT))
                                                 COERCION-FN-SECOND-TAG)))
                       (sr-array-set! c3 COERCION-FN-ARITY-INDEX st)
                       (sr-array-set! c3 COERCION-FN-RETURN-INDEX cr))
                      (sr-tag-value c3 COERCION-MEDIATING-TAG))))
              (Begin
                (list
                 (Assign c1a-u  (sr-tagged-array-ref c1 COERCION-MEDIATING-TAG
                                                     (sr-plus COERCION-FN-FMLS-OFFSET i)))
                 (Assign c2a-u  (sr-tagged-array-ref c2 COERCION-MEDIATING-TAG
                                                     (sr-plus COERCION-FN-FMLS-OFFSET i)))
                 (Assign ca-u   (App-Code comp-c `(,(Var c2a-u) ,(Var c1a-u))))
                 (Assign id?2-u (If id?1
                                    (sr-check-tag=? ca COERCION-TAG-MASK COERCION-IDENTITY-TAG)
                                    FALSE-IMDT))
                 (Assign c4-u   (App-Code comp-fn `(,c1 ,c2 ,(sr-plus (Quote 1) i) ,a ,id?2))))
                (If (sr-check-tag=? c4 COERCION-TAG-MASK COERCION-IDENTITY-TAG)
                    COERCION-IDENTITY-IMDT
                    (Begin
                      (list
                       (sr-tagged-array-set! c4 COERCION-MEDIATING-TAG
                                             (sr-plus COERCION-FN-FMLS-OFFSET i) ca))
                      c4))))))
      (add-new-code! (cons comp-fn-u comp-fn-c))
      (set-box! comp-fn-coercion-code-label? comp-fn)
      comp-fn)
    (let ([cl? (unbox comp-fn-coercion-code-label?)])
      (or cl? (make-code! comp-crcn))))

  (: get-coerce-tuple! (Uid -> (Code-Label Uid)))
  (define (get-coerce-tuple! cast)
    (: make-code! (Uid -> (Code-Label Uid)))
    (define (make-code! cast)
      (define coerce-tuple-u (next-uid! "coerce-tuple"))
      (define v-u            (next-uid! "tuple-val"))
      (define c-u            (next-uid! "tuple-coercion"))
      (define i-u            (next-uid! "index"))
      (define a-u            (next-uid! "tuple-type-num"))
      (define va-u           (next-uid! "tuple-val-item"))
      (define ca-u           (next-uid! "tuple-coercion-item"))
      (define v1-u           (next-uid! "resulting_tuple"))
      (define v2-u           (next-uid! "resulting_tuple"))
      (define cva-u          (next-uid! "item"))
      (define v              (Var v-u))
      (define c              (Var c-u))
      (define v1             (Var v1-u))
      (define v2             (Var v2-u))
      (define i              (Var i-u))
      (define a              (Var a-u))
      (define coerce-tuple   (Code-Label coerce-tuple-u))
      (define cast-l         (Code-Label cast))
      (define coerce-tuple-c : D0-Code
        (Code `(,v-u ,c-u ,i-u ,a-u)
          (If (Op '= `(,i ,a))
              (Begin (list (Assign v1-u (Op 'Alloc (list a)))) v1)
              (Begin
                (list
                 (Assign va-u (Op 'Array-ref (list v i)))
                 (Assign ca-u (sr-tagged-array-ref c COERCION-MEDIATING-TAG
                                                   (sr-plus COERCION-TUPLE-ELEMENTS-OFFSET i)))
                 (Assign cva-u (App-Code cast-l `(,(Var va-u) ,(Var ca-u) ,(Quote 0))))
                 (Assign v2-u (App-Code coerce-tuple `(,v ,c ,(sr-plus (Quote 1) i) ,a)))
                 (Op 'Array-set! (list v2 i (Var cva-u))))
                v2))))
      (add-new-code! (cons coerce-tuple-u coerce-tuple-c))
      (set-box! coerce-tuple-code-label? coerce-tuple)
      coerce-tuple)
    (let ([cl? (unbox coerce-tuple-code-label?)])
      (or cl? (make-code! cast))))

  (: get-coerce-tuple-in-place! (Uid -> (Code-Label Uid)))
  (define (get-coerce-tuple-in-place! cast)
    (: make-code! (Uid -> (Code-Label Uid)))
    (define (make-code! cast)
      (define coerce-tuple-u (next-uid! "coerce-tuple-in-place"))
      (define v-u            (next-uid! "tuple-val"))
      (define c-u            (next-uid! "tuple-coercion"))
      (define mono-u         (next-uid! "mono-address"))
      (define tmp-u          (next-uid! "tuple-count-tagged"))
      (define n-u            (next-uid! "tuple-count"))
      (define i-u            (next-uid! "index"))
      (define va-u           (next-uid! "tuple-val-item"))
      (define ca-u           (next-uid! "tuple-coercion-item"))
      (define t1-u           (next-uid! "mono-type1"))
      (define t2-u           (next-uid! "mono-type2"))
      (define a              (next-uid! "_"))
      (define new-va-u       (next-uid! "new-tuple-item"))
      (define v              (Var v-u))
      (define c              (Var c-u))
      (define mono           (Var mono-u))
      (define n              (Var n-u))
      (define t1             (Var t1-u))
      (define t2             (Var t2-u))
      (define i              (Var i-u))
      (define cast-l         (Code-Label cast))
      (define coerce-tuple   (Code-Label coerce-tuple-u))
      (define coerce-tuple-c : D0-Code
        (Code `(,v-u ,c-u ,mono-u)
          (Begin
            (list
             (Assign tmp-u (sr-tagged-array-ref c COERCION-MEDIATING-TAG COERCION-TUPLE-COUNT-INDEX))
             (Assign n-u (Op '%>> (list (Var tmp-u) COERCION-SECOND-TAG-SHIFT)))
             (Repeat i-u (Quote 0) n a UNIT-IMDT
                     (Begin
                       (list
                        (Assign va-u (Op 'Array-ref (list v i)))
                        (Assign ca-u (sr-tagged-array-ref c COERCION-MEDIATING-TAG
                                                          (sr-plus COERCION-TUPLE-ELEMENTS-OFFSET i)))
                        (Assign t1-u (Op 'Array-ref (list mono MONO-RTTI-INDEX)))
                        (Assign new-va-u (App-Code cast-l `(,(Var va-u) ,(Var ca-u) ,mono)))
                        (Assign t2-u (Op 'Array-ref (list mono MONO-RTTI-INDEX))))
                       (If (Op '= (list t1 t2))
                           (Op 'Array-set! (list v i (Var new-va-u)))
                           (Quote 0)))))
            v)))
      (add-new-code! (cons coerce-tuple-u coerce-tuple-c))
      (set-box! coerce-tuple-in-place-code-label? coerce-tuple)
      coerce-tuple)
    (let ([cl? (unbox coerce-tuple-in-place-code-label?)])
      (or cl? (make-code! cast))))

  (: get-cast-tuple-in-place! (Uid -> (Code-Label Uid)))
  (define (get-cast-tuple-in-place! cast)
    (: make-code! (Uid -> (Code-Label Uid)))
    (define (make-code! cast)
      (define cast-u     (next-uid! "cast-tuple-in-place"))
      (define v-u        (next-uid! "val"))
      (define mono-u     (next-uid! "mono-address"))
      (define t1-u       (next-uid! "tuple-type1"))
      (define t2-u       (next-uid! "tuple-type2"))
      (define t1m-u      (next-uid! "mono-type"))
      (define t2m-u      (next-uid! "mono-type-updated"))
      (define l-u        (next-uid! "label"))
      (define i-u        (next-uid! "index"))
      (define n-u        (next-uid! "tuple-type-num"))
      (define t1a-u      (next-uid! "tuple-type1-item"))
      (define t2a-u      (next-uid! "tuple-type2-item"))
      (define va-u       (next-uid! "item-val"))
      (define new-va-u   (next-uid! "new-tuple-item"))
      (define a          (next-uid! "_"))
      (define v          (Var v-u))
      (define t1         (Var t1-u))
      (define t2         (Var t2-u))
      (define l          (Var l-u))
      (define va         (Var va-u))
      (define i          (Var i-u))
      (define n          (Var n-u))
      (define mono       (Var mono-u))
      (define cast-tuple (Code-Label cast-u))
      (define cast-l       (Code-Label cast))
      (define cast-tuple-c : D0-Code
        (Code `(,v-u ,t1-u ,t2-u ,l-u ,mono-u)
          (Begin
            (list
             (Assign n-u (sr-tagged-array-ref t2 TYPE-TUPLE-TAG TYPE-TUPLE-COUNT-INDEX))
             (Repeat i-u (Quote 0) n a UNIT-IMDT
                     (Begin
                       (list
                        (Assign va-u (Op 'Array-ref (list v i)))
                        (Assign t1a-u (sr-tagged-array-ref t1 TYPE-TUPLE-TAG (sr-plus TYPE-TUPLE-ELEMENTS-OFFSET i)))
                        (Assign t2a-u (sr-tagged-array-ref t2 TYPE-TUPLE-TAG (sr-plus TYPE-TUPLE-ELEMENTS-OFFSET i)))
                        (Assign t1m-u (Op 'Array-ref (list mono MONO-RTTI-INDEX)))
                        (Assign new-va-u (App-Code cast-l `(,va ,(Var t1a-u) ,(Var t2a-u) ,l ,mono)))
                        (Assign t2m-u (Op 'Array-ref (list mono MONO-RTTI-INDEX))))
                       (If (Op '= (list (Var t1m-u) (Var t2m-u)))
                           (Op 'Array-set! (list v i (Var new-va-u)))
                           (Quote 0)))))
            v)))
      (add-new-code! (cons cast-u cast-tuple-c))
      (set-box! cast-tuple-in-place-code-label? cast-tuple)
      cast-tuple)
    (let ([cl? (unbox cast-tuple-in-place-code-label?)])
      (or cl? (make-code! cast))))

  (: get-cast-tuple! (Uid -> (Code-Label Uid)))
  (define (get-cast-tuple! cast)
    (: make-code! (Uid -> (Code-Label Uid)))
    (define (make-code! cast)
      (define cast-u     (next-uid! "cast-tuple"))
      (define v-u        (next-uid! "val"))
      (define t1-u       (next-uid! "tuple-type1"))
      (define t2-u       (next-uid! "tuple-type2"))
      (define l-u        (next-uid! "label"))
      (define i-u        (next-uid! "index"))
      (define a-u        (next-uid! "tuple-type-num"))
      (define t1a-u      (next-uid! "tuple-type1-item"))
      (define t2a-u      (next-uid! "tuple-type2-item"))
      (define c1-u       (next-uid! "resulting_val"))
      (define c2-u       (next-uid! "resulting_val"))
      (define ca-u       (next-uid! "item-type"))
      (define va-u       (next-uid! "item-val"))
      (define v          (Var v-u))
      (define t1         (Var t1-u))
      (define t2         (Var t2-u))
      (define l          (Var l-u))
      (define c1         (Var c1-u))
      (define c2         (Var c2-u))
      (define va         (Var va-u))
      (define i          (Var i-u))
      (define a          (Var a-u))
      (define cast-tuple (Code-Label cast-u))
      (define cast-l       (Code-Label cast))
      (define cast-tuple-c : D0-Code
        (Code `(,v-u ,t1-u ,t2-u ,l-u ,i-u ,a-u)
          (If (Op '= `(,i ,a))
              (Begin (list (Assign c1-u (Op 'Alloc (list a)))) c1)
              (Begin
                (list
                 (Assign va-u (Op 'Array-ref (list v i)))
                 (Assign t1a-u (sr-tagged-array-ref t1 TYPE-TUPLE-TAG (sr-plus TYPE-TUPLE-ELEMENTS-OFFSET i)))
                 (Assign t2a-u (sr-tagged-array-ref t2 TYPE-TUPLE-TAG (sr-plus TYPE-TUPLE-ELEMENTS-OFFSET i)))
                 (Assign ca-u (App-Code cast-l `(,va ,(Var t1a-u) ,(Var t2a-u) ,l ,(Quote 0))))
                 (Assign c2-u (App-Code cast-tuple `(,v ,t1 ,t2 ,l ,(sr-plus (Quote 1) i) ,a)))
                 (Op 'Array-set! (list c2 i (Var ca-u))))
                c2))))
      (add-new-code! (cons cast-u cast-tuple-c))
      (set-box! cast-tuple-code-label? cast-tuple)
      cast-tuple)
    (let ([cl? (unbox cast-tuple-code-label?)])
      (or cl? (make-code! cast))))

  (: get-mk-tuple-crcn! (Uid -> (Code-Label Uid)))
  (define (get-mk-tuple-crcn! mk-crcn)
    (: make-code! (Uid -> (Code-Label Uid)))
    (define (make-code! mk-crcn)
      (define mk-tuple-u (next-uid! "make-tuple-coercion"))
      (define t1-u       (next-uid! "tuple-type1"))
      (define t2-u       (next-uid! "tuple-type2"))
      (define l-u        (next-uid! "label"))
      (define i-u        (next-uid! "index"))
      (define a-u        (next-uid! "tuple-type-num"))
      (define t1a-u      (next-uid! "tuple-type1-item"))
      (define t2a-u      (next-uid! "tuple-type2-item"))
      (define c1-u       (next-uid! "resulting_coercion"))
      (define c2-u       (next-uid! "resulting_coercion"))
      (define ca-u       (next-uid! "item-coercion"))
      (define st-u       (next-uid! "second-tagged"))
      (define t1         (Var t1-u))
      (define t2         (Var t2-u))
      (define l          (Var l-u))
      (define c1         (Var c1-u))
      (define c2         (Var c2-u))
      (define i          (Var i-u))
      (define a          (Var a-u))
      (define st         (Var st-u))
      (define mk-tuple   (Code-Label mk-tuple-u))
      (define mk-c       (Code-Label mk-crcn))
      (define mk-tuple-c : D0-Code
        (Code `(,t1-u ,t2-u ,l-u ,i-u ,a-u)
          (If (Op '= `(,i ,a))
              (Begin
                (list
                 (Assign c1-u (Op 'Alloc (list (sr-plus a (Quote 1)))))
                 (Assign st-u (Op '+ (list (Op '%<< (list a COERCION-SECOND-TAG-SHIFT))
                                           COERCION-TUPLE-SECOND-TAG)))
                 (sr-array-set! c1 COERCION-TUPLE-COUNT-INDEX st))
                (sr-tag-value c1 COERCION-MEDIATING-TAG))
              (Begin
                (list
                 (Assign t1a-u (sr-tagged-array-ref t1 TYPE-TUPLE-TAG (sr-plus TYPE-TUPLE-ELEMENTS-OFFSET i)))
                 (Assign t2a-u (sr-tagged-array-ref t2 TYPE-TUPLE-TAG (sr-plus TYPE-TUPLE-ELEMENTS-OFFSET i)))
                 (Assign ca-u (App-Code mk-c `(,(Var t1a-u) ,(Var t2a-u) ,l)))
                 (Assign c2-u (App-Code mk-tuple `(,t1 ,t2 ,l ,(sr-plus (Quote 1) i) ,a)))
                 (sr-tagged-array-set! c2 COERCION-MEDIATING-TAG (sr-plus COERCION-TUPLE-ELEMENTS-OFFSET i) (Var ca-u)))
                c2))))
      (add-new-code! (cons mk-tuple-u mk-tuple-c))
      (set-box! mk-tuple-coercion-code-label? mk-tuple)
      mk-tuple)
    (let ([cl? (unbox mk-tuple-coercion-code-label?)])
      (or cl? (make-code! mk-crcn))))

  (: get-comp-tuple-crcn! (Uid -> (Code-Label Uid)))
  (define (get-comp-tuple-crcn! comp-crcn)
    (: make-code! (Uid -> (Code-Label Uid)))
    (define (make-code! mk-crcn)
      (define comp-tuple-u (next-uid! "compose-tuple-coercion"))
      (define c1-u      (next-uid! "tuple-coercion1"))
      (define c2-u      (next-uid! "tuple-coercion2"))
      (define c3-u      (next-uid! "result-coercion"))
      (define c4-u      (next-uid! "result-coercion"))
      (define i-u       (next-uid! "index"))
      (define a-u       (next-uid! "tuple-coercion-num"))
      (define c1a-u     (next-uid! "tuple-coercion1-argument"))
      (define c2a-u     (next-uid! "tuple-coercion2-argument"))
      (define cr-u      (next-uid! "return-coercion"))
      (define ca-u      (next-uid! "argument-coercion"))
      (define id?1-u    (next-uid! "is_identity"))
      (define id?2-u    (next-uid! "is_still_identity"))
      (define st-u      (next-uid! "second-tagged"))
      (define c1        (Var c1-u))
      (define c2        (Var c2-u))
      (define c3        (Var c3-u))
      (define c4        (Var c4-u))
      (define cr        (Var cr-u))
      (define ca        (Var ca-u))
      (define i         (Var i-u))
      (define a         (Var a-u))
      (define id?1      (Var id?1-u))
      (define id?2      (Var id?2-u))
      (define st        (Var st-u))
      (define comp-tuple   (Code-Label comp-tuple-u))
      (define comp-c    (Code-Label comp-crcn))
      (define comp-tuple-c : D0-Code
        (Code `(,c1-u ,c2-u ,i-u ,a-u ,id?1-u)
          (If (Op '= `(,i ,a))
              (Begin
                (list
                 (Assign c3-u (Op 'Alloc (list (sr-plus a (Quote 1)))))
                 (Assign st-u (Op '+ (list (Op '%<< (list a COERCION-SECOND-TAG-SHIFT))
                                           COERCION-TUPLE-SECOND-TAG)))
                 (sr-array-set! c3 COERCION-TUPLE-COUNT-INDEX st))
                (sr-tag-value c3 COERCION-MEDIATING-TAG))
              (Begin
                (list
                 (Assign c1a-u (sr-tagged-array-ref c1 COERCION-MEDIATING-TAG
                                                    (sr-plus COERCION-TUPLE-ELEMENTS-OFFSET i)))
                 (Assign c2a-u (sr-tagged-array-ref c2 COERCION-MEDIATING-TAG
                                                    (sr-plus COERCION-TUPLE-ELEMENTS-OFFSET i)))
                 (Assign ca-u (App-Code comp-c `(,(Var c1a-u) ,(Var c2a-u))))
                 (Assign id?2-u (If id?1
                                    (sr-check-tag=? ca COERCION-TAG-MASK COERCION-IDENTITY-TAG)
                                    FALSE-IMDT))
                 (Assign c4-u (App-Code comp-tuple `(,c1 ,c2 ,(sr-plus (Quote 1) i) ,a ,id?2))))
                (If (sr-check-tag=? c4 COERCION-TAG-MASK COERCION-IDENTITY-TAG)
                    COERCION-IDENTITY-IMDT
                    (Begin
                      (list
                       (sr-tagged-array-set! c4 COERCION-MEDIATING-TAG
                                             (sr-plus COERCION-TUPLE-ELEMENTS-OFFSET i) ca))
                      c4))))))
      (add-new-code! (cons comp-tuple-u comp-tuple-c))
      (set-box! comp-tuple-coercion-code-label? comp-tuple)
      comp-tuple)
    (let ([cl? (unbox comp-tuple-coercion-code-label?)])
      (or cl? (make-code! comp-crcn))))


  ;; (: sr-coercion (Coercion/Immediate-Type -> D0-Expr))
  ;; (define (sr-coercion c)
  ;;   (match c
  ;;     [(Identity) COERCION-IDENTITY-IMDT]
  ;;     [(Project (app sr-prim-type t) l)
  ;;      (sr-alloc "project_coercion" data:COERCION-PROJECT-TAG
  ;;                `(("type" . ,t) ("label" . ,(Quote l))))]
  ;;     [(Inject (app sr-prim-type t))
  ;;      (sr-alloc "inject-coercion" data:COERCION-INJECT-TAG
  ;;                `(("type" . ,t)))]
  ;;     [(Sequence (app sr-coercion f) (app sr-coercion s))
  ;;      (sr-alloc "sequence_coecion" data:COERCION-SEQUENCE-TAG
  ;;                `(("first" . ,f) (,"second" . ,s)))]
  ;;     [(Fn _ a* (app sr-coercion r))
  ;;      (define st-u      (next-uid! "second-tagged"))
  ;;      (Let `((,st-u . ,(Op '+ (list (Op '%<< (list (Quote (length a*)) COERCION-SECOND-TAG-SHIFT))
  ;;                                  COERCION-FN-SECOND-TAG))))
  ;;           (sr-alloc "fn_coercion" data:COERCION-MEDIATING-TAG
  ;;                     `(("arity"  . ,(Var st-u))
  ;;                       ("return" . ,r) .
  ;;                       ,(map (lambda ([a : Coercion/Immediate-Type])
  ;;                               (cons "argument" (sr-coercion a)))
  ;;                             a*))))]
  ;;     [(Ref (app sr-coercion r) (app sr-coercion w))
  ;;      (define st-u    (next-uid! "second-tagged"))
  ;;      (Let `((,st-u . ,(Op '+ (list (Op '%<< (list (Quote 0) COERCION-SECOND-TAG-SHIFT))
  ;;                                    COERCION-REF-SECOND-TAG))))
  ;;           (sr-alloc "ref-coercion" data:COERCION-MEDIATING-TAG
  ;;                     `(("tag" . ,(Var st-u))
  ;;                       ("read-coercion" . ,r)
  ;;                       ("write-coercion" . ,w))))]
  ;;      [(MonoRef (app sr-prim-type t))
  ;;       (sr-alloc "mref-coercion" data:COERCION-MREF-SECOND-TAG
  ;;                `(("type" . ,t)))]
  ;;     [(CTuple _ a*)
  ;;      (define st-u      (next-uid! "second-tagged"))
  ;;      (Let `((,st-u . ,(Op '+ (list (Op '%<< (list (Quote (length a*)) COERCION-SECOND-TAG-SHIFT))
  ;;                                    COERCION-TUPLE-SECOND-TAG))))
  ;;           (sr-alloc "tuple_coercion" data:COERCION-MEDIATING-TAG
  ;;                     `(("num"  . ,(Var st-u))
  ;;                       .
  ;;                       ,(map (lambda ([a : Coercion/Immediate-Type])
  ;;                               (cons "item" (sr-coercion a)))
  ;;                             a*))))]
  ;;     [(Failed l)
  ;;      (sr-alloc "failed-coercion" data:COERCION-FAILED-TAG
  ;;                `(("label" . ,(Quote l))))]
  ;;     [other (error 'specify-representation/coercion "unmatched ~a" other)]))

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
         (sr-alloc "project_coercion" data:COERCION-PROJECT-TAG
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
         (sr-alloc "inject_coercion" data:COERCION-INJECT-TAG `(("type" . ,t)))]
        [(Inject-Coercion-Huh (app recur e))
         (sr-check-tag=? e COERCION-TAG-MASK COERCION-INJECT-TAG)]
        [(Inject-Coercion-Type (app recur e))
         (sr-tagged-array-ref e COERCION-INJECT-TAG COERCION-INJECT-TYPE-INDEX)]
        ;; Sequence Coercions
        [(Sequence-Coercion (app recur f) (app recur s))
         (sr-alloc "sequence_coercion" data:COERCION-SEQUENCE-TAG
                   `(("first" . ,f) (,"second" . ,s)))]
        [(Sequence-Coercion-Huh (app recur e))
         (sr-check-tag=? e COERCION-TAG-MASK COERCION-SEQUENCE-TAG)]
        [(Sequence-Coercion-Fst (app recur e))
         (sr-tagged-array-ref e COERCION-SEQUENCE-TAG COERCION-SEQUENCE-FST-INDEX)]
        [(Sequence-Coercion-Snd (app recur e))
         (sr-tagged-array-ref e COERCION-SEQUENCE-TAG COERCION-SEQUENCE-SND-INDEX)]
        ;; Identity Coercions can only be created by coercion quoting
        ;; But  their representation is just (Quote ID-COERCION-TAG)
        [(Id-Coercion-Huh (app recur e))
         (sr-check-tag=? e COERCION-TAG-MASK COERCION-IDENTITY-TAG)]
        ;; Function Coercions
        [(Fn-Coercion-Huh (app recur e))
         (define tmp (next-uid! "crcn_tmp"))
         (define tag (next-uid! "crcn_tag"))
         (define tmp-var (Var tmp))
         (define tag-var (Var tag))
         (Begin
           (list (Assign tmp (sr-tagged-array-ref e COERCION-MEDIATING-TAG COERCION-FN-ARITY-INDEX))
                 (Assign tag (Op 'binary-and `(,tmp-var ,COERCION-TAG-MASK))))
           (Op '= (list tag-var COERCION-FN-SECOND-TAG)))]
        [(Fn-Coercion-Arity (app recur e))
         (define tmp (next-uid! "tagged_arity"))
         (define tmp-var (Var tmp))
         (Begin (list (Assign tmp (sr-tagged-array-ref e COERCION-MEDIATING-TAG COERCION-FN-ARITY-INDEX)))
                (Op '%>> (list tmp-var COERCION-SECOND-TAG-SHIFT)))]
        [(Fn-Coercion-Arg (app recur e) (app recur i))
         (sr-tagged-array-ref e COERCION-MEDIATING-TAG (sr-plus COERCION-FN-FMLS-OFFSET i))]
        [(Fn-Coercion-Return (app recur e))
         (sr-tagged-array-ref e COERCION-MEDIATING-TAG COERCION-FN-RETURN-INDEX)]
        ;; TODO either repurpose or get rid of the arrity field
        ;; One could immagine that we use it to dynamically dispatch on compose
        [(Fn-Coercion (app recur* e*) (app recur e))
         (define st-u    (next-uid! "second-tagged"))
         (Begin (list (Assign st-u (Op '+ (list (Op '%<< (list (Quote (length e*)) COERCION-SECOND-TAG-SHIFT))
                                                COERCION-FN-SECOND-TAG))))
                (sr-alloc "fn_coercion" data:COERCION-MEDIATING-TAG
                          `(("arity" . ,(Var st-u))
                            ("return" . ,e) .
                            ,(map (lambda ([e : D0-Expr])
                                    (cons "argument" e))
                                  e*))))]
        [(Make-Fn-Coercion mk-crcn (app recur t1) (app recur t2) (app recur l))
         (: invoke-mk-fn-crcn ((Code-Label Uid) (Var Uid) D0-Expr D0-Expr -> D0-Expr))
         (define (invoke-mk-fn-crcn mk-fn t1 t2 l)
           (App-Code
            mk-fn
            (list t1 t2 l (Quote 0)
                  (sr-tagged-array-ref t1 TYPE-FN-TAG TYPE-FN-ARITY-INDEX))))
         (let ([mk-fn-crcn (get-mk-fn-crcn! mk-crcn)])
           (if (Var? t1)
               (invoke-mk-fn-crcn mk-fn-crcn t1 t2 l)
               (let ([u (next-uid! "fn_type1")])
                 (Begin (list (Assign u t1))
                        (invoke-mk-fn-crcn mk-fn-crcn (Var u) t2 l)))))]
        [(Compose-Fn-Coercion compose (app recur c1) (app recur c2))
         (: invoke-comp ((Code-Label Uid) (Var Uid) D0-Expr -> D0-Expr))
         (define (invoke-comp comp-fn c1 c2)
           (define tmp (next-uid! "tagged_arity"))
           (define tmp-var (Var tmp))
           (define a (next-uid! "untagged_arity"))
           (define a-var (Var a))
           (Begin (list (Assign tmp (sr-tagged-array-ref c1 COERCION-MEDIATING-TAG COERCION-FN-ARITY-INDEX))
                        (Assign a (Op '%>> (list tmp-var COERCION-SECOND-TAG-SHIFT))))
                  (App-Code comp-fn (list c1 c2 (Quote 0) a-var TRUE-IMDT))))
         (let ([mk-fn-crcn (get-comp-fn-crcn! compose)])
           (if (Var? c1)
               (invoke-comp mk-fn-crcn c1 c2)
               (let ([u (next-uid! "fn_coercion1")])
                 (Begin  (list (Assign u c1))
                         (invoke-comp mk-fn-crcn (Var u) c2)))))]

        [(Ref-Coercion-Huh (app recur e))
         (define tmp (next-uid! "crcn_tmp"))
         (define tag (next-uid! "crcn_tag"))
         (define tmp-var (Var tmp))
         (define tag-var (Var tag))
         (Begin
           (list
            (Assign tmp (sr-tagged-array-ref e COERCION-MEDIATING-TAG COERCION-REF-TAG-INDEX))
            (Assign tag (Op 'binary-and `(,tmp-var ,COERCION-TAG-MASK))))
           (Op '= (list tag-var COERCION-REF-SECOND-TAG)))]
        [(Ref-Coercion (app recur r) (app recur w))
         (define st-u    (next-uid! "second-tagged"))
         (Begin
           (list
            (Assign st-u (Op '+ (list (Op '%<< (list (Quote 0) COERCION-SECOND-TAG-SHIFT))
                                      COERCION-REF-SECOND-TAG))))
           (sr-alloc "ref-coercion" data:COERCION-MEDIATING-TAG
                     `(("tag" . ,(Var st-u))
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
         (sr-alloc "failed-coercion" data:COERCION-FAILED-TAG `(("label" . ,l)))]
        [(Failed-Coercion-Label (app recur e))
         (sr-tagged-array-ref e COERCION-FAILED-TAG COERCION-FAILED-LABEL-INDEX)]
        ;; FN-Proxy Stuff
        [(Fn-Proxy i (app recur clos) (app recur crcn))
         (sr-alloc "fn-proxy" data:FN-PROXY-TAG
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
         (sr-alloc "hybrid-proxy" data:FN-PROXY-TAG
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
        [(Dyn-tag (app recur e))
         (Op 'binary-and (list e DYN-TAG-MASK))]
        [(Dyn-immediate (app recur e))
         (Op '%>> (list e DYN-IMDT-SHIFT))]
        [(Dyn-make (app recur e1) e2)
         (sr-dyn-make recur e1 e2)]
        [(Dyn-type (app recur e))
         (define tmp (next-uid! "tmp"))
         (define tag (next-uid! "tag"))
         (define tagv (Var tag))
         (define tmpv (Var tmp))
         (define err-msg
           (Quote "specify-representation/Dyn-type: switch failure"))
         (Begin
           (list (Assign tmp e)
                 (Assign tag (Op 'binary-and `(,tmpv ,DYN-TAG-MASK))))
           ;; TODO this could be a switch
           (Switch
            tagv
            `([(,data:DYN-BOXED-TAG) .
               ,(Op 'Array-ref (list tmpv DYN-TYPE-INDEX))]
              [(,data:DYN-INT-TAG) . ,TYPE-INT-RT-VALUE]
              [(,data:DYN-BOOL-TAG) . ,TYPE-BOOL-RT-VALUE]
              [(,data:DYN-UNIT-TAG) . ,TYPE-UNIT-RT-VALUE]
              [(,data:DYN-CHAR-TAG) . ,TYPE-CHAR-RT-VALUE])
            (Begin
              (list (Op 'Print (list err-msg))
                    (Op 'Exit  (list (Quote 1))))
              UNDEF-IMDT))
           #;
           (If (Op '= `(,tagv ,DYN-BOXED-TAG))
               (Op 'Array-ref (list tmpv DYN-TYPE-INDEX))
               (If (Op '= `(,tagv ,DYN-INT-TAG))
                   TYPE-INT-RT-VALUE
                   (If (Op '= `(,tagv ,DYN-BOOL-TAG))
                       TYPE-BOOL-RT-VALUE
                       TYPE-UNIT-RT-VALUE))))]
        [(Dyn-value (app recur e))
         (define tmp (next-uid! "dyn_value_tmp"))
         (define tag (next-uid! "dyn_value_tag"))
         (define tmp-var (Var tmp))
         (define tag-var (Var tag))
         (Begin
           (list (Assign tmp e)
                 (Assign tag (Op 'binary-and `(,tmp-var ,DYN-TAG-MASK))))
           (If (Op '= (list tag-var DYN-BOXED-TAG))
               (Op 'Array-ref (list tmp-var DYN-VALUE-INDEX))
               (Op '%>> (list tmp-var DYN-IMDT-SHIFT))))]
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
         (Repeat i e1 e2 u e3 (recur/env e4 (extend (extend env u (Var u)) i (Var i)) cenv))]
        [(Break-Repeat) (Break-Repeat)]
        ;; Guarded
        [(Unguarded-Box (app recur e))
         (sr-alloc "unguarded_box" data:UGBOX-TAG (list (cons "init_value" e)))]
        [(Unguarded-Box-Ref (app recur e))
         (Op 'Array-ref (list e UGBOX-VALUE-INDEX))]
        [(Unguarded-Box-Set! (app recur e1) (app recur e2))
         (Op 'Array-set! (list e1 UGBOX-VALUE-INDEX e2))]
        [(Unguarded-Vect (app recur e1) (app recur e2))
         (define tmp1     (next-uid! "ugvect1"))
         (define tmp2     (next-uid! "ugvect2"))
         (define tmp3     (next-uid! "ugvect3"))
         (define tmp4     (next-uid! "ugvect4"))
         (define i        (next-uid! "index"))
         (define a        (next-uid! "_"))
         (define tmp1-var (Var tmp1))
         (define tmp2-var (Var tmp2))
         (define tmp3-var (var tmp3))
         (define tmp4-var (var tmp4))
         (define tl       (Op '+ (list tmp1-var UGVECT-OFFSET)))
         (define alloc    (Op 'Alloc (list tmp4-var)))
         (define set      (Repeat i UGVECT-OFFSET tmp4-var a UNIT-IMDT
                                  (Op 'Array-set! (list tmp3-var (Var i) tmp2-var))))
         (define set-n    (Op 'Array-set! (list tmp3-var UGVECT-SIZE-INDEX tmp1-var)))  
         (Begin (list (Assign tmp1 e1)
                      (Assign tmp2 e2)
                      (Assign tmp4 tl)
                      (Assign tmp3 alloc)
                      set-n
                      set)
                tmp3-var)]
        [(Unguarded-Vect-Ref (app recur e1) (app recur e2))
         (define ind  (next-uid! "index"))
         (define tmp1 (next-uid! "e"))
         (define zro (Quote 0))
         (define tmp1-var (Var tmp1))
         (define ind-var (Var ind))
         (Begin
           (list (Assign ind e2)
                 (Assign tmp1 e1))
           ;; TODO This duplicates the error exit code (fix this so it doesn't)
           (if (bounds-checks?)
               (If (Op '>= (list ind-var zro)) ;; vectors indices starts from 0
                   (If (Op '< (list ind-var (Op 'Array-ref (list tmp1-var zro))))
                       (Op 'Array-ref (list tmp1-var (Op '+ (list ind-var UGVECT-OFFSET))))
                       (Begin
                         (list (Op 'Printf (list (Quote "index out of bound %ld\n") ind-var)))
                         (Op 'Exit (list (Quote -1)))))
                   (Begin
                     (list (Op 'Printf (list (Quote "index out of bound %ld\n") ind-var)))
                     (Op 'Exit (list (Quote -1)))))
               (Op 'Array-ref (list tmp1-var (Op '+ (list ind-var UGVECT-OFFSET))))))]
        [(Unguarded-Vect-Set! (app recur e1) (app recur e2) (app recur e3))
         (define ind  (next-uid! "index"))
         (define tmp1 (next-uid! "ugvect"))
         (define zro (Quote 0))
         (define tmp1-var (Var tmp1))
         (define ind-var (Var ind))
         (Begin
           (list (Assign ind e2)
                 (Assign tmp1 e1))
           (if (bounds-checks?)
               (If (Op '>= (list ind-var zro)) ;; vectors indices starts from 0
                   (If (Op '< (list ind-var (Op 'Array-ref (list tmp1-var zro))))
                       (Op 'Array-set! (list tmp1-var (Op '+ (list ind-var UGVECT-OFFSET)) e3))
                         (Begin
                           (list (Op 'Printf (list (Quote "index out of bound %ld\n") ind-var)))
                           (Op 'Exit (list (Quote -1)))))
                   (Begin
                     `(,(Op 'Printf (list (Quote "index out of bound %ld\n") ind-var)))
                     (Op 'Exit (list (Quote -1)))))
               (Op 'Array-set! (list tmp1-var (Op '+ (list ind-var UGVECT-OFFSET)) e3))))]
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
         (sr-alloc "mbox" data:MBOX-TAG (list (cons "init_type" t) (cons "init_value" e)))]
        [(Mbox-val-set! (app recur e1) (app recur e2))
         (Op 'Array-set! (list e1 MBOX-VALUE-INDEX e2))]
        [(Mbox-val-ref (app recur e))
         (Op 'Array-ref (list e MBOX-VALUE-INDEX))]
        [(Mbox-rtti-set! u (app recur e))
         (Op 'Array-set! (list (Var u) MONO-RTTI-INDEX e))]
        [(Mbox-rtti-ref u)
         (Op 'Array-ref (list (Var u) MONO-RTTI-INDEX))]
        [(Mvector (app recur e1) (app recur e2) (app sr-prim-type t))
         (define tmp1     (next-uid! "mvect1"))
         (define tmp2     (next-uid! "mvect2"))
         (define tmp3     (next-uid! "mvect3"))
         (define tmp4     (next-uid! "mvect4"))
         (define i        (next-uid! "index"))
         (define a        (next-uid! "a"))
         (define rtti     (next-uid! "rtti"))
         (Begin (list
                 (Assign tmp1 e1)
                 (Assign tmp2 e2)
                 (Assign rtti t)
                 (Assign tmp4 (Op '+ (list (Var tmp1) MVECT-OFFSET)))
                 (Assign tmp3 (Op 'Alloc (list (Var tmp4))))
                 (Op 'Array-set! (list (Var tmp3) MVECT-SIZE-INDEX (Var tmp1)))
                 (Op 'Array-set! (list (Var tmp3) MONO-RTTI-INDEX (Var rtti)))
                 (Repeat i MVECT-OFFSET (Var tmp4) a UNIT-IMDT
                         (Op 'Array-set! (list (Var tmp3) (Var i) (Var tmp2)))))
                (Var tmp3))]
        [(Mvector-length (app recur e))
         (Op 'Array-ref (list e MVECT-SIZE-INDEX))]
        [(Mvector-rtti-set! u (app recur e))
         (Op 'Array-set! (list (Var u) MONO-RTTI-INDEX e))]
        [(Mvector-rtti-ref u)
         (Op 'Array-ref (list (Var u) MONO-RTTI-INDEX))]
        [(Mvector-val-ref (app recur e1) (app recur e2))
         (define ind  (next-uid! "index"))
         (define tmp1 (next-uid! "mvect"))
         (define zro (Quote 0))
         (define tmp1-var (Var tmp1))
         (define ind-var (Var ind))
         (Begin
           (list (Assign ind e2)
                 (Assign tmp1 e1))
           (if (bounds-checks?)
               (If (Op '>= (list ind-var zro)) ;; vectors indices starts from 0
                   (If (Op '< (list ind-var (Op 'Array-ref (list tmp1-var MVECT-SIZE-INDEX))))
                       (Op 'Array-ref (list tmp1-var (Op '+ (list ind-var MVECT-OFFSET))))
                       (Begin
                         (list (Op 'Printf (list (Quote "index out of bound %ld\n") ind-var)))
                         (Op 'Exit (list (Quote -1)))))
                   (Begin
                     (list (Op 'Printf (list (Quote "index out of bound %ld\n") ind-var)))
                     (Op 'Exit (list (Quote -1)))))
               (Op 'Array-ref (list tmp1-var (Op '+ (list ind-var MVECT-OFFSET))))))]
        [(Mvector-val-set! (app recur e1) (app recur e2) (app recur e3))
         (define ind  (next-uid! "index"))
         (define tmp1 (next-uid! "mvect"))
         (define zro (Quote 0))
         (define tmp1-var (Var tmp1))
         (define ind-var (Var ind))
         (Begin
           (list
            (Assign ind e2)
            (Assign tmp1 e1))
           (if (bounds-checks?)
               (If (Op '>= (list ind-var zro)) ;; vectors indices starts from 0
                   (If (Op '< (list ind-var (Op 'Array-ref (list tmp1-var MVECT-SIZE-INDEX))))
                       (Op 'Array-set! (list tmp1-var (Op '+ (list ind-var MVECT-OFFSET)) e3))
                       (Begin
                         (list (Op 'Printf (list (Quote "index out of bound %ld\n") ind-var)))
                         (Op 'Exit (list (Quote -1)))))
                   (Begin
                     `(,(Op 'Printf (list (Quote "index out of bound %ld\n") ind-var)))
                     (Op 'Exit (list (Quote -1)))))
               (Begin
                 `(,(Op 'Printf (list (Quote "index out of bound %ld\n") ind-var)))
                 (Op 'Exit (list (Quote -1))))))]
        [(Type-MVect (app recur e))
         (sr-alloc "MVectT" data:TYPE-MVECT-TAG `(("type" . ,e)))]
        [(Type-MVect-Huh (app recur e))
         (sr-check-tag=? e TYPE-TAG-MASK TYPE-MVECT-TAG)]
        [(Type-MVect-Of (app recur e))
         (define arg : D0-Expr
           (Op 'binary-xor (list e TYPE-MVECT-TAG)))
         (Op 'Array-ref (list arg TYPE-MVECT-TYPE-INDEX))]
        [(MVect-Coercion-Huh (app recur e))
         (define tmp (next-uid! "crcn_tmp"))
         (define tag (next-uid! "crcn_tag"))
         (define tmp-var (Var tmp))
         (define tag-var (Var tag))
         (Begin
           (list
            (Assign tmp (sr-tagged-array-ref e COERCION-MEDIATING-TAG COERCION-MVECT-TAG-INDEX))
            (Assign tag (Op 'binary-and `(,tmp-var ,COERCION-TAG-MASK))))
           (Op '= (list tag-var COERCION-MVECT-SECOND-TAG)))]
        [(MVect-Coercion-Type (app recur e))
         (sr-tagged-array-ref e COERCION-MEDIATING-TAG COERCION-MVECT-TYPE-INDEX)]
        [(MVect-Coercion (app recur t))
         (define st-u    (next-uid! "second-tagged"))
         (Begin
           (list
            (Assign st-u (Op '+ (list (Op '%<< (list (Quote 0) COERCION-SECOND-TAG-SHIFT))
                                      COERCION-MVECT-SECOND-TAG))))
           (sr-alloc "mvect-coercion" data:COERCION-MEDIATING-TAG
                     `(("tag" . ,(Var st-u))
                       ("type" . ,t))))]
        [(MRef-Coercion-Huh (app recur e))
         (define tmp (next-uid! "crcn_tmp"))
         (define tag (next-uid! "crcn_tag"))
         (define tmp-var (Var tmp))
         (define tag-var (Var tag))
         (Begin
           (list
            (Assign tmp (sr-tagged-array-ref e COERCION-MEDIATING-TAG COERCION-MREF-TAG-INDEX))
            (Assign tag (Op 'binary-and `(,tmp-var ,COERCION-TAG-MASK))))
           (Op '= (list tag-var COERCION-MREF-SECOND-TAG)))]
        [(MRef-Coercion-Type (app recur e))
         (sr-tagged-array-ref e COERCION-MEDIATING-TAG COERCION-MREF-TYPE-INDEX)]
        [(MRef-Coercion (app recur t))
         (define st-u    (next-uid! "second-tagged"))
         (Begin
           (list
            (Assign st-u (Op '+ (list (Op '%<< (list (Quote 0) COERCION-SECOND-TAG-SHIFT))
                                      COERCION-MREF-SECOND-TAG))))
           (sr-alloc "ref-coercion" data:COERCION-MEDIATING-TAG
                     `(("tag" . ,(Var st-u))
                       ("type" . ,t))))]
        [(Make-Fn-Type mk-glb (app recur t1) (app recur t2))
         (: invoke-mk-fn-type ((Code-Label Uid) (Var Uid) D0-Expr -> D0-Expr))
         (define (invoke-mk-fn-type mk-fn t1 t2)
           (App-Code
            mk-fn
            (list t1 t2 (Quote 0)
                  (sr-tagged-array-ref t1 TYPE-FN-TAG TYPE-FN-ARITY-INDEX))))
         (let ([mk-fn-type (get-mk-fn-type! mk-glb)])
           (if (Var? t1)
               (invoke-mk-fn-type mk-fn-type t1 t2)
               (let ([u (next-uid! "fn_type1")])
                 (Begin
                   (list (Assign u t1))
                   (invoke-mk-fn-type mk-fn-type (Var u) t2)))))]
        [(Make-Tuple-Type mk-glb (app recur t1) (app recur t2))
         (: invoke-mk-tuple-type ((Code-Label Uid) (Var Uid) D0-Expr -> D0-Expr))
         (define (invoke-mk-tuple-type mk-tuple t1 t2)
           (App-Code
            mk-tuple
            (list t1 t2 (Quote 0)
                  (sr-tagged-array-ref t2 TYPE-TUPLE-TAG TYPE-TUPLE-COUNT-INDEX))))
         (let ([mk-tuple-type (get-mk-tuple-type! mk-glb)])
           (if (Var? t1)
               (invoke-mk-tuple-type mk-tuple-type t1 t2)
               (let ([u (next-uid! "tuple_type1")])
                 (Begin
                   (list (Assign u t1))
                   (invoke-mk-tuple-type mk-tuple-type (Var u) t2)))))]
        [(Type-GRef (app recur e))
         (sr-alloc "GRefT" data:TYPE-GREF-TAG `(("type" . ,e)))]
        [(Type-GVect (app recur e))
         (sr-alloc "GVectT" data:TYPE-GVECT-TAG `(("type" . ,e)))]
        [(Type-MRef (app recur e))
         (sr-alloc "MRefT" data:TYPE-MREF-TAG `(("type" . ,e)))]
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
         (sr-alloc "tuple" 0
                   (map (lambda ([e : D0-Expr])
                          (cons "element" e))
                        e*))]
        [(Copy-Tuple (app recur n) (app recur v))
         (define i-u            (next-uid! "index"))
         (define a              (next-uid! "_"))
         (define va-u           (next-uid! "tuple-val-item"))
         (define v1-u           (next-uid! "resulting_tuple"))
         (define v1             (Var v1-u))
         (define i              (Var i-u))
         (Begin (list
                 (Assign v1-u (Op 'Alloc (list n)))
                 (Repeat i-u (Quote 0) n a UNIT-IMDT
                         (Begin
                           (list
                            (Assign va-u (Op 'Array-ref (list v i))))
                           (Op 'Array-set! (list v1 i (Var va-u))))))
                v1)]
        [(Tuple-proj (app recur e) i)
         (Op 'Array-ref (list e (Quote i)))]
        [(Tuple-Coercion-Huh (app recur e))
         (define tmp (next-uid! "crcn_tmp"))
         (define tag (next-uid! "crcn_tag"))
         (define tmp-var (Var tmp))
         (define tag-var (Var tag))
         (Begin
           (list (Assign tmp (sr-tagged-array-ref e COERCION-MEDIATING-TAG COERCION-TUPLE-COUNT-INDEX))
                 (Assign tag (Op 'binary-and `(,tmp-var ,COERCION-TAG-MASK))))
           (Op '= (list tag-var COERCION-TUPLE-SECOND-TAG)))]
        [(Tuple-Coercion-Num (app recur e))
         (define tmp (next-uid! "tagged_num"))
         (define tmp-var (Var tmp))
         (Begin
           (list (Assign tmp (sr-tagged-array-ref e COERCION-MEDIATING-TAG COERCION-TUPLE-COUNT-INDEX)))
           (Op '%>> (list tmp-var COERCION-SECOND-TAG-SHIFT)))]
        [(Tuple-Coercion-Item (app recur e) i)
         (sr-tagged-array-ref e COERCION-MEDIATING-TAG (sr-plus COERCION-TUPLE-ELEMENTS-OFFSET (Quote i)))]
        [(Type-Tuple-Huh (app recur e))
         (sr-check-tag=? e TYPE-TAG-MASK TYPE-TUPLE-TAG)]
        [(Type-Tuple-num (app recur e))
         (sr-tagged-array-ref e TYPE-TUPLE-TAG TYPE-TUPLE-COUNT-INDEX)]
        [(Type-Tuple-item (app recur e) i)
         (sr-tagged-array-ref e TYPE-TUPLE-TAG (sr-plus TYPE-TUPLE-ELEMENTS-OFFSET (Quote i)))]
        [(Coerce-Tuple uid (app recur v) (app recur c))
         (: invoke-coerce-tuple ((Code-Label Uid) (Var Uid) D0-Expr -> D0-Expr))
         (define (invoke-coerce-tuple coerce-tuple v c)
           (define tmp (next-uid! "tagged_num"))
           (Begin
             (list (Assign tmp (sr-tagged-array-ref c COERCION-MEDIATING-TAG COERCION-TUPLE-COUNT-INDEX)))
             (App-Code
              coerce-tuple
              (list v c (Quote 0) (Op '%>> (list (Var tmp) COERCION-SECOND-TAG-SHIFT))))))
         (let ([coerce-tuple (get-coerce-tuple! uid)])
           (if (Var? v)
               (invoke-coerce-tuple coerce-tuple v c)
               (let ([u (next-uid! "tuple_val1")])
                 (Begin
                   (list (Assign u v))
                   (invoke-coerce-tuple coerce-tuple (Var u) c)))))]
        [(Coerce-Tuple-In-Place uid (app recur v) (app recur c) (app recur mono-type))
         (: invoke-coerce-tuple ((Code-Label Uid) (Var Uid) D0-Expr D0-Expr -> D0-Expr))
         (define (invoke-coerce-tuple coerce-tuple v c a)
           (App-Code
            coerce-tuple
            (list v c a)))
         (let ([coerce-tuple (get-coerce-tuple-in-place! uid)])
           (if (Var? v)
               (invoke-coerce-tuple coerce-tuple v c mono-type)
               (let ([u (next-uid! "tuple_val1")])
                 (Begin
                   (list (Assign u v))
                   (invoke-coerce-tuple coerce-tuple (Var u) c mono-type)))))]
        [(Cast-Tuple-In-Place uid (app recur v) (app recur t1) (app recur t2) (app recur lbl) (app recur mono-address))
         (: invoke-cast-tuple ((Code-Label Uid) (Var Uid) D0-Expr D0-Expr D0-Expr D0-Expr -> D0-Expr))
         (define (invoke-cast-tuple cast-tuple v t1 t2 lbl a)
           (App-Code cast-tuple (list v t1 t2 lbl a)))
         (let ([cast-tuple (get-cast-tuple-in-place! uid)])
           (if (Var? v)
               (invoke-cast-tuple cast-tuple v t1 t2 lbl mono-address)
               (let ([u (next-uid! "tuple_val1")])
                 (Begin (list (Assign u v))
                        (invoke-cast-tuple cast-tuple (Var u) t1 t2 lbl mono-address)))))]
        [(Cast-Tuple uid (app recur v) (app recur t1) (app recur t2) (app recur lbl))
         (: invoke-cast-tuple ((Code-Label Uid) (Var Uid) D0-Expr D0-Expr D0-Expr -> D0-Expr))
         (define (invoke-cast-tuple cast-tuple v t1 t2 lbl)
           (define num (next-uid! "num"))
           (Begin (list (Assign num (sr-tagged-array-ref t2 TYPE-TUPLE-TAG TYPE-TUPLE-COUNT-INDEX)))
                  (App-Code cast-tuple (list v t1 t2 lbl (Quote 0) (Var num)))))
         (let ([cast-tuple (get-cast-tuple! uid)])
           (if (Var? v)
               (invoke-cast-tuple cast-tuple v t1 t2 lbl)
               (let ([u (next-uid! "tuple_val1")])
                 (Begin (list (Assign u v))
                        (invoke-cast-tuple cast-tuple (Var u) t1 t2 lbl)))))]
        [(Make-Tuple-Coercion mk-crcn (app recur t1) (app recur t2) (app recur l))
         (: invoke-mk-tuple-crcn ((Code-Label Uid) (Var Uid) D0-Expr D0-Expr -> D0-Expr))
         (define (invoke-mk-tuple-crcn mk-tuple-crcn t1 t2 l)
           (App-Code
            mk-tuple-crcn
            (list t1 t2 l (Quote 0)
                  (sr-tagged-array-ref t2 TYPE-TUPLE-TAG TYPE-TUPLE-COUNT-INDEX))))
         (let ([mk-tuple-crcn (get-mk-tuple-crcn! mk-crcn)])
           (if (Var? t1)
               (invoke-mk-tuple-crcn mk-tuple-crcn t1 t2 l)
               (let ([u (next-uid! "tuple_type1")])
                 (Begin (list (Assign u t1))
                        (invoke-mk-tuple-crcn mk-tuple-crcn (Var u) t2 l)))))]
        [(Compose-Tuple-Coercion compose (app recur c1) (app recur c2))
         (: invoke-comp ((Code-Label Uid) (Var Uid) D0-Expr -> D0-Expr))
         (define (invoke-comp comp-tuple c1 c2)
           (define tmp (next-uid! "tagged_num"))
           (define tmp-var (Var tmp))
           (define a (next-uid! "untagged_num"))
           (define a-var (Var a))
           (Begin
             (list (Assign tmp (sr-tagged-array-ref c2 COERCION-MEDIATING-TAG COERCION-TUPLE-COUNT-INDEX))
                   (Assign a (Op '%>> (list tmp-var COERCION-SECOND-TAG-SHIFT))))
             (App-Code comp-tuple (list c1 c2 (Quote 0) a-var TRUE-IMDT))))
         (let ([mk-tuple-crcn (get-comp-tuple-crcn! compose)])
           (if (Var? c1)
               (invoke-comp mk-tuple-crcn c1 c2)
               (let ([u (next-uid! "tuple_coercion1")])
                 (Begin (list (Assign u c1))
                        (invoke-comp mk-tuple-crcn (Var u) c2)))))]
        [(Mediating-Coercion-Huh (app recur e))
         (sr-check-tag=? e COERCION-TAG-MASK COERCION-MEDIATING-TAG)]
        [other (error 'specify-representation "unmatched ~a" other)]))

    (recur exp))



  (recur/env exp env cenv))


;; Allocate without forgetting to lift evaluating subterms first
;; this prevents evaluating terms which may cause more allocation
;; will initializing the values of an allocation
;; it essentially produces expression of the form:
;; (let ((t* e*) ...) (let ((tmp (alloc ...))) (begin (set tmp i t*) ... (binary-or tmp tag))))
;; though it does eliminate any form that it can based on it's input
(: sr-alloc/next ((Boxof Nat) ->
                  (String Fixnum (Listof (Pair String D0-Expr)) ->
                          D0-Expr)))
(define ((sr-alloc/next next) name tag slots)
  (: next-uid! (String -> Uid))
  (define (next-uid! x)
    (let ([n (unbox next)])
      (set-box! next (add1 n))
      (Uid x n)))
  ;; As long as this is used to initialize all the data I have
  ;; a faily stong guarentee that no other allocation could possibly occur.
  (: sr-alloc-init ((Var Uid) -> (Index (Var Uid) -> D0-Expr)))
  (define ((sr-alloc-init mem) offset value)
    (Op 'Array-set! (list mem (Quote offset) value)))
  ;; Take a list of variables and expressions paired with their names
  ;; make variables that are bound to the expressions and the bindings
  (: get-assignments/vars ((Listof (Pair String D0-Expr)) -> (Values D0-Expr* (Listof (Var Uid)))))
  (define (get-assignments/vars b*)
    (if (null? b*)
        (values '() '())
        (let*-values ([(a  d)  (values (car b*) (cdr b*))]
                      [(a* v*) (get-assignments/vars d)]
                      [(n  e)  (values (car a) (cdr a))])
          (if (Var? e)
              (values a* (cons e v*))
              (let* ([u (next-uid! n)])
                (values (cons (Assign u e) a*) (cons (Var u) v*)))))))
  (define result
    (let ([size (length slots)])
      (if (= size 0)
          (error 'specify-representation "Empty objects can not be allocated")
          (let*-values ([(ass* var*) (get-assignments/vars slots)]
                        [(ind*)      (range 0 size)]
                        [(alloc-id)  (next-uid! name)]
                        [(alloc-var) (Var alloc-id)]
                        [(alloc-ass) (Assign alloc-id (Op 'Alloc `(,(Quote size))))]
                        [(set*)       (map (sr-alloc-init alloc-var) ind* var*)]
                        [(tag-return) (if (= tag 0)
                                          alloc-var
                                          (Op 'binary-or (list alloc-var (Quote tag))))])
            (Begin (append ass* (cons alloc-ass set*)) tag-return)))))
  (debug off name tag slots result))

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

(: sr-bndt ((Boxof Nat) -> (CoC6-Bnd-Type -> D0-Expr)))
(define ((sr-bndt next) bnd)
  (define sr-alloc (sr-alloc/next next))
  (: sr-type (Compact-Type -> D0-Expr))
  (define (sr-type t)
    (match t
      [(GRef t)
       (sr-alloc "GRefT" data:TYPE-GREF-TAG
                 `(("type" . ,(sr-prim-type t))))]
      [(GVect t)
       (sr-alloc "GVect_Type" data:TYPE-GVECT-TAG
                 `(("type" . ,(sr-prim-type t))))]
      [(MRef t)
       (sr-alloc "MRefT" data:TYPE-MREF-TAG
                 `(("type" . ,(sr-prim-type t))))]
      [(MVect t)
       (sr-alloc "MVectT" data:TYPE-MVECT-TAG
                 `(("type" . ,(sr-prim-type t))))]
      [(Fn a f* r)
       (sr-alloc "Fun_Type" data:TYPE-FN-TAG
                 `(("arity" . ,(Quote a))
                   ("return" . ,(sr-prim-type r)) .
                   ,(map (lambda ([t : Immediate-Type])
                           (cons "argument" (sr-prim-type t)))
                         f*)))]
      [(STuple n a*)
       (sr-alloc "Tuple_Type" data:TYPE-TUPLE-TAG
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

(: sr-bnd-coercion ((Boxof Nat) -> (CoC6-Bnd-Crcn -> D0-Expr)))
(define (sr-bnd-coercion next)
  (: next-uid! (String -> Uid))
  (define (next-uid! x)
    (let ([n (unbox next)])
      (set-box! next (add1 n))
      (Uid x n)))
  
  (define sr-alloc (sr-alloc/next next))
  (: sr-coercion (Compact-Coercion -> D0-Expr))
  (define (sr-coercion t)
    (match t
      [(Identity) COERCION-IDENTITY-IMDT]
      [(Project t l)
       ;; TODO Make it possible to turn off type hoisting
       (define t^ (sr-prim-type t))
       (sr-alloc "project_coercion" data:COERCION-PROJECT-TAG
                 `(("type" . ,t^) ("label" . ,(Quote l))))]
      [(Inject (app sr-prim-type t))
       (sr-alloc "inject-coercion" data:COERCION-INJECT-TAG
                 `(("type" . ,t)))]
      [(Sequence (app sr-immediate-coercion f)
                 (app sr-immediate-coercion s))
       (sr-alloc "sequence_coecion" data:COERCION-SEQUENCE-TAG
                 `(("first" . ,f) (,"second" . ,s)))]
      [(Fn l a* (app sr-immediate-coercion r))
       (define len : Index (length a*))
       (unless (= l len)
         (error 'sr-coercion "length mismatch"))
       (define st-u      (next-uid! "second-tagged"))
       (Begin
         (list (Assign st-u (Op '+ (list (Op '%<< (list (Quote l) COERCION-SECOND-TAG-SHIFT))
                                         COERCION-FN-SECOND-TAG))))
         (sr-alloc "fn_coercion" data:COERCION-MEDIATING-TAG
                   `(("arity"  . ,(Var st-u))
                     ("return" . ,r) .
                     ,(map (lambda ([a : Immediate-Coercion])
                             (cons "argument" (sr-immediate-coercion a)))
                           a*))))]
      [(Ref (app sr-immediate-coercion r) (app sr-immediate-coercion w))
       (define st-u    (next-uid! "second-tagged"))
       (Begin
         (list (Assign st-u (Op '+ (list (Op '%<< (list (Quote 0) COERCION-SECOND-TAG-SHIFT))
                                         COERCION-REF-SECOND-TAG))))
         (sr-alloc "ref-coercion" data:COERCION-MEDIATING-TAG
                   `(("tag" . ,(Var st-u))
                     ("read-coercion" . ,r)
                     ("write-coercion" . ,w))))]
      [(MonoRef (app sr-prim-type t))
       (define st-u    (next-uid! "second-tagged"))
       (Begin
         (list (Assign st-u (Op '+ (list (Op '%<< (list (Quote 0) COERCION-SECOND-TAG-SHIFT))
                                         COERCION-MREF-SECOND-TAG))))
         (sr-alloc "mref-coercion" data:COERCION-MEDIATING-TAG
                   `(("tag" . ,(Var st-u))
                     ("type" . ,t))))]
      [(MonoVect (app sr-prim-type t))
       (define st-u    (next-uid! "second-tagged"))
       (Begin
         (list (Assign st-u (Op '+ (list (Op '%<< (list (Quote 0) COERCION-SECOND-TAG-SHIFT))
                                         COERCION-MVECT-SECOND-TAG))))
         (sr-alloc "mvect-coercion" data:COERCION-MEDIATING-TAG
                   `(("tag" . ,(Var st-u))
                     ("type" . ,t))))]
      [(CTuple l a*)
       (define len : Index (length a*))
       (unless (= l len)
         (error 'sr-coercion "length mismatch"))
       (define st-u      (next-uid! "second-tagged"))
       (Begin
         (list (Assign st-u (Op '+ (list (Op '%<< (list (Quote (length a*)) COERCION-SECOND-TAG-SHIFT))
                                         COERCION-TUPLE-SECOND-TAG))))
         (sr-alloc "tuple_coercion" data:COERCION-MEDIATING-TAG
                   `(("num"  . ,(Var st-u))
                     .
                     ,(map (lambda ([a : Immediate-Coercion])
                             (cons "item" (sr-immediate-coercion a)))
                           a*))))]
      [(Failed l)
       (sr-alloc "failed-coercion" data:COERCION-FAILED-TAG
                 `(("label" . ,(Quote l))))]
      [other (error 'specify-representation/type "unmatched ~a" other)]))
  (lambda ([b : CoC6-Bnd-Crcn])
    (match-let ([(cons u c) b])
      (Assign u (sr-coercion c)))))

(: untag-deref-gproxy (-> D0-Expr (-> D0-Expr D0-Expr)))
(define ((untag-deref-gproxy index) proxy)
  (Op 'Array-ref
      (list (Op 'binary-xor (list proxy GPROXY-TAG))
            index)))

(: alloc-tag-set-gproxy/twosome
   ((String -> Uid) D0-Expr D0-Expr D0-Expr D0-Expr -> D0-Expr))
(define (alloc-tag-set-gproxy/twosome uid! ref-e src-e tar-e lbl-e)
  ;; TODO Consider using sr-alloc here
  (define proxy (uid! "guarded_proxy"))
  (define ref   (uid! "guarded_ref"))
  (define src   (uid! "source_t"))
  (define tar   (uid! "target_t"))
  (define lbl   (uid! "blame"))
  (define var   (Var proxy))
  (Begin
    (list (Assign ref ref-e)
          (Assign src src-e)
          (Assign tar tar-e)
          (Assign lbl lbl-e)
          (Assign proxy (Op 'Alloc (list GPROXY/TWOSOME-SIZE)))
          (Op 'Array-set! (list var GPROXY-FOR-INDEX    (Var ref)))
          (Op 'Array-set! (list var GPROXY-FROM-INDEX   (Var src)))
          (Op 'Array-set! (list var GPROXY-TO-INDEX     (Var tar)))
          (Op 'Array-set! (list var GPROXY-BLAMES-INDEX (Var lbl))))
    (Op 'binary-or (list var GPROXY-TAG))))

(: alloc-tag-set-gproxy/coercion
   ((String -> Uid) D0-Expr D0-Expr -> D0-Expr))
(define (alloc-tag-set-gproxy/coercion uid! ref-e crcn-e)
  (define proxy (uid! "guarded_proxy"))
  (define ref   (uid! "guarded_ref"))
  (define crcn  (uid! "coercion"))
  (define var   (Var proxy))
  (Begin
    (list
     (Assign ref ref-e)
     (Assign crcn crcn-e)
     (Assign proxy (Op 'Alloc (list GPROXY/COERCION-SIZE)))
     (Op 'Array-set! (list var GPROXY-FOR-INDEX      (Var ref)))
     (Op 'Array-set! (list var GPROXY-COERCION-INDEX (Var crcn))))
    (Op 'binary-or (list var GPROXY-TAG))))

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
(define (sr-observe uid! e t)
  (: generate-print (Uid Schml-Type -> D0-Expr))
  (define (generate-print id ty)
    (cond
      [(Int? t) (Op 'Printf (list (Quote "Int : %ld\n") (Var id)))]
      ;; This is a fragile hack
      ;; switch to typed ASTs
      [(Float? t) (Begin (list (Op 'Print (list (Quote "Float : ")))
                               (Op 'print-float (list (Var id) (Quote 9))))
                         (Op 'Print (list (Quote "\n"))))]
      [(Character? t)
       (Begin (list (Op 'Print (list (Quote "Char : ")))
                    (Op 'print-char (list (Var id))))
              (Op 'Print (list (Quote "\n"))))]
      [(Bool? t)
       (If (Var id)
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
  (let* ([res (uid! "result")])
    (Begin (list (Assign res e)) (generate-print res t))))

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

