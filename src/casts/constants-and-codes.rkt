#lang typed/racket

(require (for-syntax racket/syntax syntax/parse racket/list
                     "../language/forms.rkt" "../language/c-helpers.rkt")
         "memory-layout-helpers.rkt"
         "../language/forms.rkt"
         "../language/data0.rkt"
         "../language/syntax.rkt")

(provide (all-defined-out))

(define-syntax (define-constants stx)
  (syntax-case stx ()
    [(_ (name val) ...)
     (let ([fmt (lambda (x) (format-id x "data:~a" x))])
       (with-syntax ([(fmt-name ...) (map fmt (syntax->list #'(name ...)))])
         (replace/append-to-constants.h
          (lambda (in)
            (map (define-line in)
                 (syntax-e #'(name ...))
                 (syntax-e #'(val ...)))))
         #'(begin
             (define fmt-name val) ...
             (define name (Quote fmt-name)) ...)))]))

(define-constants
  ;; types
  (TYPE-TAG-MASK   #b111)  
  ;; unallocated types
  (TYPE-ATOMIC-TAG #b111) ;; This should be TYPE-IMDT-TAG
  ;; Immediate types are tagged with #b111
  (TYPE-DYN-RT-VALUE        #b000111)
  (TYPE-INT-RT-VALUE        #b001111)
  (TYPE-BOOL-RT-VALUE       #b010111)
  (TYPE-UNIT-RT-VALUE       #b011111)
  (TYPE-FLOAT-RT-VALUE      #b100111)
  (TYPE-CHAR-RT-VALUE       #b101111)
  (TYPE-MAX-ATOMIC-RT-VALUE #b111111)

  (HC-PRJ-TAG-MASK  #b10)
  (HC-INJ-TAG-MASK  #b01)
  (HC-TAG-MASK      #b11)
  (HC-T1-INDEX      0)
  (HC-LABEL-INDEX   1)
  (HC-T2-INDEX      2)
  (HC-MED-INDEX     3)
  ;; coercions
  (COERCION-TAG-MASK              #b111) ;; the same for primary and secondary tags
  ;; project coercion representation
  (COERCION-PROJECT-TAG           #b000)
  (COERCION-PROJECT-TYPE-INDEX    0)
  (COERCION-PROJECT-LABEL-INDEX   1)
  ;; inject coercion representation
  (COERCION-INJECT-TAG            #b001)
  (COERCION-INJECT-TYPE-INDEX     0)
  ;; sequence coercion representation
  (COERCION-SEQUENCE-TAG          #b010)
  (COERCION-SEQUENCE-FST-INDEX    0)
  (COERCION-SEQUENCE-SND-INDEX    1)
  ;; identity coercion representation
  (COERCION-IDENTITY-TAG          #b011)
  (COERCION-IDENTITY-IMDT         #b011)
  ;; fail coercion representation
  (COERCION-FAILED-TAG            #b110)
  (COERCION-FAILED-LABEL-INDEX    0)
  ;; mediating coercion representation
  (COERCION-SECOND-TAG-SHIFT      3)
  (COERCION-MEDIATING-TAG         #b100)
  ;; function coercion representation
  (COERCION-FN-SECOND-TAG         #b001)
  (COERCION-FN-ARITY-INDEX        0)
  (COERCION-FN-RETURN-INDEX       1)
  (COERCION-FN-FMLS-OFFSET        2)
  ;; tuple coercion representation
  (COERCION-TUPLE-SECOND-TAG      #b010)
  (COERCION-TUPLE-COUNT-INDEX     0)
  (COERCION-TUPLE-ELEMENTS-OFFSET 1)
  ;; monotonic coercion representation
  (COERCION-MREF-SECOND-TAG       #b011)
  (COERCION-MREF-TAG-INDEX        0)
  (COERCION-MREF-TYPE-INDEX       1)
  ;; monotonic vector coercion representation
  (COERCION-MVECT-SECOND-TAG      #b100)
  (COERCION-MVECT-TAG-INDEX       0)
  (COERCION-MVECT-TYPE-INDEX      1)
  ;; guarded coercion representation
  (COERCION-REF-SECOND-TAG        #b000)
  (COERCION-REF-TAG-INDEX         0)
  (COERCION-REF-READ-INDEX        1)
  (COERCION-REF-WRITE-INDEX       2)
  (COERCION-REF-FLAG-INDEX        3)
  (COERCION-REF-REF-FLAG          0)
  (COERCION-REF-VEC-FLAG          1)
  ;; values
  ;; simple dynamic value representation
  (DYN-TAG-MASK                   #b111)
  ;; Note: specify representation relies on the fact that boxes are tagged zero and
  ;; do not clear this tag. (If this changes that will also need to change)
  (DYN-BOXED-TAG                  #b000)
  (DYN-INT-TAG                    #b001)
  (DYN-BOOL-TAG                   #b111)
  (DYN-UNIT-TAG                   #b010)
  (DYN-CHAR-TAG                   #b011)
  (DYN-IMDT-SHIFT                 3)
  ;; allocated dynamic value representation
  (DYN-BOX-SIZE                   2)
  (DYN-VALUE-INDEX                0)
  (DYN-TYPE-INDEX                 1)
  ;; bool value representation
  (FALSE-IMDT                     #b000)
  (TRUE-IMDT                      #b001)
  ;; unit representation
  (UNIT-IMDT                      #b000)
  (DYN-UNIT-IMDT                  (bitwise-ior
                                   (arithmetic-shift data:UNIT-IMDT data:DYN-IMDT-SHIFT)
                                   data:DYN-UNIT-TAG)) 
  (GREP-TAG-MASK                  #b111)
  ;; 0
  (UNDEF-IMDT                     0)
  (ZERO-IMDT                      0)
  ;; guarded values representation
  (UGBOX-TAG                      #b000)
  (UGBOX-SIZE                     1)
  (UGBOX-VALUE-INDEX              0)
  (UGVECT-SIZE-INDEX              0)
  (UGVECT-OFFSET                  1)
  (GPROXY-TAG                     #b001)
  (GPROXY/TWOSOME-SIZE            4)
  (GPROXY/COERCION-SIZE           2)
  (GPROXY-COERCION-INDEX          1)
  (GPROXY-FOR-INDEX               0)
  (GPROXY-FROM-INDEX              1)
  (GPROXY-TO-INDEX                2)
  (GPROXY-BLAMES-INDEX            3)
  ;; monotonic values representation
  (MONO-RTTI-INDEX                0)
  (MBOX-SIZE                      2)
  (MBOX-VALUE-INDEX               1)
  (MBOX-TAG                       #b000)
  (MVECT-SIZE-INDEX               1)
  (MVECT-OFFSET                   2)
  (MVECT-TAG                      #b000)
  ;; function value representation
  (CLOS-CODE-INDEX                0)
  (CLOS-CSTR-INDEX                1)
  (CLOS-FVAR-OFFSET               2)
  (FN-TAG-MASK                    #b111)
  (CLOSURE-VALUE-MASK             -8) ;; signed long compliment of fn tag mask
  (FN-PROXY-TAG                   #b001)
  (FN-PROXY-CRCN-INDEX            1)
  (FN-PROXY-CLOS-INDEX            0)
  (HYBRID-PROXY-TAG               #b001)
  (HYBRID-PROXY-CRCN-INDEX        2)
  (HYBRID-PROXY-CLOS-INDEX        1)
  (EXIT-FAILURE                   1))

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
     (define many-field-dtm (syntax->datum #'many-field))
     (define field-string* (map syntax->datum (syntax->list #'(field* ...))))
     (define index-def* (map (gen-index/offset-id "INDEX") field-string*))
     (define offset-def
       (if many-field-dtm ((gen-index/offset-id "OFFSET") many-field-dtm) #f))
     (define offset-data-def (if many-field-dtm (format-id stx "data:~a" offset-def) #f))
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
         (define-constants
           (tag-def tag)
           (index-def 0)
           (hash-def 1)
           (sindex/offset-def* sindex/offset-val*) ...)
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
      [else (op$ Print err-msg2) (op$ Exit EXIT-FAILURE) UNDEF-IMDT])))

;; These bindings are C functions that are needed by the compiled code and is
;; hoisted by specify-representation
(define boxed-bnd-code* : (Boxof D0-Bnd-Code*) (box '()))

(: add-new-code! (D0-Bnd-Code -> Void))
(define (add-new-code! b)
  (set-box! boxed-bnd-code* (cons b (unbox boxed-bnd-code*))))

(: hashcons-types-code-label? (Boxof (Option (Code-Label Uid))))
(define hashcons-types-code-label? (box #f))

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
