#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: src/casts/build-structured-types
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
Description: This pass exposes the memory layout of aspects of the program.
After this pass language complexity decreases greatly! But all operations are
exposed as the effects that they truelly are.
+-------------------------------------------------------------------------------+
| Source Grammar : Cast4
| Target Grammar : Data0
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require "../helpers.rkt"
         "../errors.rkt"
         "../configuration.rkt"
         "../language/cast-or-coerce6.rkt"
         "../language/data0.rkt"
         )

;; Only the pass is provided by this module
(provide
 (all-from-out
  "../language/cast-or-coerce6.rkt"
  "../language/data0.rkt")
 specify-representation)

#|
(TODO Rewrite any allocating procedures in this pass.
      Remebering the lessons of the runtime class.
      No complex expressions should be able to be evaluated while
      Allocating and setting a chunk of memory.
      Otherwise you will end up with partially initiallized chucks
      of memory in the heap while you are executing other code.
      Write some test cases to test this.
      Is it possible to enforce this by require all constructors to only
      have variables as sub expressions.
      Perhaps one pass before this.
      member to force the evaluation of sub-arguments)

(TODO Talk to jeremy about taging all heap values)
|#

(require (prefix-in l: "../language/data-representation.rkt"))
;; Only allocate each of these once
(define FN-ARITY-INDEX        (Quote l:FN-ARITY-INDEX))
(define FN-RETURN-INDEX       (Quote l:FN-RETURN-INDEX))
(define FN-FMLS-OFFSET        (Quote l:FN-FMLS-OFFSET))
                          
(define TYPE-TAG-MASK         (Quote l:TYPE-TAG-MASK))
(define TYPE-FN-TAG           (Quote l:TYPE-FN-TAG))
(define TYPE-ATOMIC-TAG       (Quote l:TYPE-ATOMIC-TAG))
(define TYPE-DYN-RT-VALUE     (Quote l:TYPE-DYN-RT-VALUE))
(define TYPE-INT-RT-VALUE     (Quote l:TYPE-INT-RT-VALUE))
(define TYPE-BOOL-RT-VALUE    (Quote l:TYPE-BOOL-RT-VALUE))
(define TYPE-UNIT-RT-VALUE    (Quote l:TYPE-UNIT-RT-VALUE))
(define TYPE-GREF-SIZE        (Quote l:TYPE-GREF-SIZE))
(define TYPE-GREF-TAG         (Quote l:TYPE-GREF-TAG))
(define TYPE-GVECT-SIZE       (Quote l:TYPE-GVECT-SIZE))
(define TYPE-GVECT-TAG        (Quote l:TYPE-GVECT-TAG))
(define DYN-TAG-MASK          (Quote l:DYN-TAG-MASK))
(define DYN-BOXED-TAG         (Quote l:DYN-BOXED-TAG))
(define DYN-INT-TAG           (Quote l:DYN-INT-TAG))
(define DYN-BOOL-TAG          (Quote l:DYN-BOOL-TAG))
(define DYN-UNIT-TAG          (Quote l:DYN-UNIT-TAG))
(define DYN-IMDT-SHIFT        (Quote l:DYN-IMDT-SHIFT))
(define DYN-BOX-SIZE          (Quote l:DYN-BOX-SIZE))
(define DYN-VALUE-INDEX       (Quote l:DYN-VALUE-INDEX))
(define DYN-TYPE-INDEX        (Quote l:DYN-TYPE-INDEX))
(define FALSE-IMDT            (Quote l:FALSE-IMDT))
(define TRUE-IMDT             (Quote l:TRUE-IMDT))
(define UNDEF-IMDT            (Quote l:UNDEF-IMDT))
(define UNIT-IMDT             (Quote l:UNIT-IMDT))
(define GREP-TAG-MASK         (Quote l:GREP-TAG-MASK))
(define UGBOX-TAG  (Quote l:UGBOX-TAG))
(define UGBOX-SIZE            (Quote l:UGBOX-SIZE))
(define UGBOX-VALUE-INDEX     (Quote l:UGBOX-VALUE-INDEX))
(define UGVECT-SIZE-INDEX     (Quote l:UGVECT-SIZE-INDEX))
(define UGVECT-OFFSET         (Quote l:UGVECT-OFFSET))
(define GPROXY-TAG            (Quote l:GPROXY-TAG))
(define GPROXY/TWOSOME-SIZE   (Quote l:GPROXY/TWOSOME-SIZE))
(define GPROXY/COERCION-SIZE  (Quote l:GPROXY/COERCION-SIZE))
(define GPROXY-COERCION-INDEX (Quote l:GPROXY-COERCION-INDEX))
(define GPROXY-FOR-INDEX      (Quote l:GPROXY-FOR-INDEX))
(define GPROXY-FROM-INDEX     (Quote l:GPROXY-FROM-INDEX))
(define GPROXY-TO-INDEX       (Quote l:GPROXY-TO-INDEX))
(define GPROXY-BLAMES-INDEX   (Quote l:GPROXY-BLAMES-INDEX))
(define CLOS-CODE-INDEX       (Quote l:CLOS-CODE-INDEX))
(define CLOS-CSTR-INDEX       (Quote l:CLOS-CSTR-INDEX))
(define CLOS-FVAR-OFFSET      (Quote l:CLOS-FVAR-OFFSET))
(define GREF-TO-INDEX         (Quote l:GREF-TO-INDEX))
(define GVECT-TO-INDEX        (Quote l:GVECT-TO-INDEX))

(define l:PROJECT-COERCION-TAG #b000)
(define PROJECT-COERCION-TAG (Quote l:PROJECT-COERCION-TAG))
(define PROJECT-COERCION-TYPE-INDEX (Quote 0))
(define PROJECT-COERCION-LABEL-INDEX (Quote 1))
(define l:INJECT-COERCION-TAG #b001)
(define INJECT-COERCION-TAG (Quote l:INJECT-COERCION-TAG))
(define INJECT-COERCION-TYPE-INDEX (Quote 0))
(define l:SEQUENCE-COERCION-TAG #b010)
(define SEQUENCE-COERCION-TAG (Quote l:SEQUENCE-COERCION-TAG))
(define SEQUENCE-COERCION-FST-INDEX (Quote 0))
(define SEQUENCE-COERCION-SND-INDEX (Quote 1))
(define COERCION-TAG-MASK (Quote #b111))
(define IDENTITY-COERCION-TAG (Quote #b011))
(define IDENTITY-COERCION-IMDT IDENTITY-COERCION-TAG)
(define l:FN-COERCION-TAG #b100)
(define FN-COERCION-TAG (Quote l:FN-COERCION-TAG))

(define l:FN-TAG-MASK #b111)
(define FN-TAG-MASK (Quote l:FN-TAG-MASK))
(define l:CLOSURE-VALUE-MASK -8) ;; signed long compliment of fn tag mask
(define CLOSURE-VALUE-MASK (Quote l:CLOSURE-VALUE-MASK))
(define l:FN-PROXY-TAG #b001)
(define FN-PROXY-TAG (Quote l:FN-PROXY-TAG))
(define l:FN-PROXY-CRCN-INDEX 1)
(define l:FN-PROXY-CLOS-INDEX 0)
(define FN-PROXY-CRCN-INDEX (Quote l:FN-PROXY-CRCN-INDEX))
(define FN-PROXY-CLOS-INDEX (Quote l:FN-PROXY-CLOS-INDEX))
(define l:HYBRID-PROXY-TAG #b001)
(define HYBRID-PROXY-TAG (Quote l:HYBRID-PROXY-TAG))
(define HYBRID-PROXY-CRCN-INDEX (Quote l:HYBRID-PROXY-CRCN-INDEX))
(define HYBRID-PROXY-CLOS-INDEX (Quote l:HYBRID-PROXY-CLOS-INDEX))
(define l:REF-COERCION-TAG #b101)
(define REF-COERCION-TAG (Quote l:REF-COERCION-TAG))
(define REF-COERCION-READ-INDEX (Quote 0))
(define REF-COERCION-WRITE-INDEX (Quote 1))
(define l:FAILED-COERCION-TAG #b110)
(define FAILED-COERCION-TAG (Quote l:FAILED-COERCION-TAG))
(define FAILED-COERCION-LABEL-INDEX (Quote 0))



(: specify-representation (Cast-or-Coerce6-Lang Config -> Data0-Lang))
(trace-define (specify-representation prgm comp-config)
  (match-let ([(Prog (list name next type) exp) prgm])
    (let* ([next  : (Boxof Nat) (box next)]
           [bndc* : (Boxof D0-Bnd-Code*) (box '())]
           [exp   : D0-Expr (sr-expr next bndc* (hash) empty-index-map exp)]
           [next  : Nat (unbox next)]
           [bndc* : D0-Bnd-Code* (unbox bndc*)])
      (Prog (list name next type) (Labels bndc* exp)))))

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

  (: mk-fn-coercion-code-label? (Boxof (Option (Code-Label Uid))))
  (define mk-fn-coercion-code-label? (box #f))

  (: comp-fn-coercion-code-label? (Boxof (Option (Code-Label Uid))))
  (define comp-fn-coercion-code-label? (box #f))

  
  ;; Allocate without forgetting to lift evaluating subterms first
  ;; this prevents evaluating terms which may cause more allocation
  ;; will initializing the values of an allocation
  ;; it essentially produces expression of the form:
  ;; (let ((t* e*) ...) (let ((tmp (alloc ...))) (begin (set tmp i t*) ... (binary-or tmp tag))))
  ;; though it does eliminate any form that it can based on it's input
  (: sr-alloc (String Fixnum (Listof (Pair String D0-Expr)) -> D0-Expr))
  (define (sr-alloc name tag slots)
    ;; As long as this is used to initialize all the data I have
    ;; a faily stong guarentee that no other allocation could possibly occur.
    (: sr-alloc-init ((Var Uid) -> (Index (Var Uid) -> D0-Expr)))
    (define ((sr-alloc-init mem) offset value)
      (Op 'Array-set! (list mem (Quote offset) value)))
    ;; Take a list of variables and expressions paired with there names
    ;; make variables that are bound to the expressions and the bindings
    (: get-bnd/var ((Listof (Pair String D0-Expr)) -> (Values D0-Bnd* (Listof (Var Uid)))))
    (define (get-bnd/var b*)
      (if (null? b*)
          (values '() '())
          (let*-values ([(a  d)  (values (car b*) (cdr b*))]
                        [(b* v*) (get-bnd/var d)]
                        [(n  e)  (values (car a) (cdr a))])
            (if (Var? e)
                (values b* (cons e v*))
                (let* ([u (next-uid! n)])
                  (values `((,u . ,e) . ,b*) `(,(Var u) . ,v*)))))))
    (define result
      (let ([size (length slots)])
        (if (= size 0)
            (error 'specify-representation "Empty objects can not be allocated")
            (let*-values ([(bnd* var*) (get-bnd/var slots)]
                          [(ind*)      (range 0 size)]
                          [(alloc-id)  (next-uid! name)]
                          [(alloc-var) (Var alloc-id)]
                          [(alloc-bnd) `((,alloc-id . ,(Op 'Alloc `(,(Quote size)))))]
                          [(set*)       (map (sr-alloc-init alloc-var) ind* var*)]
                          [(tag-return) (if (= tag 0)
                                            alloc-var
                                            (Op 'binary-or (list alloc-var (Quote tag))))]
                          [(alloc-set-return) (Let alloc-bnd (Begin set* tag-return))])
              (if (null? bnd*)
                  alloc-set-return
                  (Let bnd* alloc-set-return))))))
    (logging sr-bnd (Vomit) "~a ~a ~a ==> ~a" name tag slots result)
    result)

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
         [else (sr-alloc "dynamic_boxed" #b000
                         `(("value" . ,e1)
                           ("type" . ,(sr-expr e2))))])]
      [else
       (let* ([val    (next-uid! "value")]
              [type   (next-uid! "type")]
              [tag    (next-uid! "tag")]
              [imm    (next-uid!  "imm")]
              [val-var (Var val)]
              [type-var (Var type)]
              [tag-var (Var tag)]
              [imm-var (Var imm)])
       (Let `((,val . ,e1)
              (,type . ,(sr-expr e2)))
        (Let `((,tag . ,(Op 'binary-and `(,type-var ,TYPE-TAG-MASK))))
         (If (Op '= `(,tag-var ,TYPE-ATOMIC-TAG))
             (Let `((,imm . ,(Op '%<< (list val-var DYN-IMDT-SHIFT))))
              (If (Op '= `(,type-var ,TYPE-INT-RT-VALUE))
                  (Op 'binary-or `(,imm-var ,DYN-INT-TAG))
                  (If (Op '= `(,type-var ,TYPE-BOOL-RT-VALUE))
                      (Op 'binary-or `(,imm-var ,DYN-BOOL-TAG))
                      (Op 'binary-or `(,imm-var ,DYN-UNIT-TAG)))))
             (sr-alloc "dynamic_boxed" #b000
                       `(("" . ,val-var)
                         ("" . ,type-var)))))))]))

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
      (define t1      (Var t1-u))
      (define t2      (Var t2-u))
      (define l       (Var l-u))
      (define c1      (Var c1-u))
      (define c2      (Var c2-u))
      (define i       (Var i-u))
      (define a       (Var a-u))
      (define mk-fn   (Code-Label mk-fn-u))
      (define mk-c    (Code-Label mk-crcn))
      ;; This code is carfully crafted so that the allocation occurs after
      ;; all of the coercion have been made this ensures that there isn't the
      ;; possibility of collecting while there is unitialized data in the heap.
      (define mk-fn-c : D0-Code
        (Code `(,t1-u ,t2-u ,l-u ,i-u ,a-u)
              (If (Op '= `(,i ,a))
                  (Let `((,t1r-u . ,(sr-tagged-array-ref t1 TYPE-FN-TAG FN-RETURN-INDEX))
                         (,t2r-u . ,(sr-tagged-array-ref t2 TYPE-FN-TAG FN-RETURN-INDEX)))
                       (Let `((,cr-u . ,(App-Code mk-c `(,(Var t1r-u) ,(Var t2r-u) ,l))))
                            (Let `((,c1-u . ,(Op 'Alloc (list (sr-plus a (Quote 2))))))
                                 (Begin
                                   `(,(sr-array-set! c1 FN-ARITY-INDEX a)
                                     ,(sr-array-set! c1 FN-RETURN-INDEX (Var cr-u)))
                                    (sr-tag-value c1 FN-COERCION-TAG)))))
                  (Let `((,t1a-u . ,(sr-tagged-array-ref t1 TYPE-FN-TAG (sr-plus FN-FMLS-OFFSET i)))
                         (,t2a-u . ,(sr-tagged-array-ref t2 TYPE-FN-TAG (sr-plus FN-FMLS-OFFSET i))))
                       (Let `((,ca-u . ,(App-Code mk-c `(,(Var t2a-u) ,(Var t1a-u) ,l))))
                            (Let `((,c2-u . ,(App-Code mk-fn `(,t1 ,t2 ,l ,(sr-plus (Quote 1) i) ,a))))
                                 (Begin
                                   `(,(sr-tagged-array-set! c2 FN-COERCION-TAG (sr-plus FN-FMLS-OFFSET i) (Var ca-u)))
                                   c2)))))))
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
      (define comp-fn   (Code-Label comp-fn-u))
      (define comp-c    (Code-Label comp-crcn))
      ;; This code is carfully crafted so that the allocation occurs after
      ;; all of the coercion have been made this ensures that there isn't the
      ;; possibility of collecting while there is unitialized data in the heap.
      (define comp-fn-c : D0-Code
        (Code `(,c1-u ,c2-u ,i-u ,a-u ,id?1-u)
              (If (Op '= `(,i ,a))
                  (Let `((,c1r-u . ,(sr-tagged-array-ref c1 FN-COERCION-TAG FN-RETURN-INDEX))
                         (,c2r-u . ,(sr-tagged-array-ref c2 FN-COERCION-TAG FN-RETURN-INDEX)))
                       (Let `((,cr-u . ,(App-Code comp-c `(,(Var c1r-u) ,(Var c2r-u)))))
                            (If (If id?1
                                    (sr-check-tag=? cr COERCION-TAG-MASK IDENTITY-COERCION-TAG)
                                    FALSE-IMDT)
                                IDENTITY-COERCION-IMDT
                                (Let `((,c3-u . ,(Op 'Alloc (list (sr-plus a (Quote 2))))))
                                 (Begin
                                   `(,(sr-array-set! c3 FN-RETURN-INDEX cr))
                                    (sr-tag-value c3 FN-COERCION-TAG))))))
                  (Let `((,c1a-u . ,(sr-tagged-array-ref c1 FN-COERCION-TAG (sr-plus FN-FMLS-OFFSET i)))
                         (,c2a-u . ,(sr-tagged-array-ref c2 FN-COERCION-TAG (sr-plus FN-FMLS-OFFSET i))))
                       (Let `((,ca-u . ,(App-Code comp-c `(,(Var c2a-u) ,(Var c1a-u)))))
                        (Let `((,id?2-u . ,(If id?1
                                               (sr-check-tag=? ca COERCION-TAG-MASK IDENTITY-COERCION-TAG)
                                               FALSE-IMDT)))
                         (Let `((,c4-u . ,(App-Code comp-fn `(,c1 ,c2 ,(sr-plus (Quote 1) i) ,a ,id?2))))
                          (If (sr-check-tag=? c4 COERCION-TAG-MASK IDENTITY-COERCION-TAG)
                              IDENTITY-COERCION-IMDT
                              (Begin
                                `(,(sr-tagged-array-set! c4 FN-COERCION-TAG (sr-plus FN-FMLS-OFFSET i) ca))
                                c4)))))))))
      (add-new-code! (cons comp-fn-u comp-fn-c))
      (set-box! comp-fn-coercion-code-label? comp-fn)
      comp-fn)
    (let ([cl? (unbox comp-fn-coercion-code-label?)])
      (or cl? (make-code! comp-crcn))))

  (: sr-type (Schml-Type -> D0-Expr))
  (define (sr-type t)
    (match t
      [(Int)  TYPE-INT-RT-VALUE]
      [(Bool) TYPE-BOOL-RT-VALUE]
      [(Dyn)  TYPE-DYN-RT-VALUE]
      [(Unit) TYPE-UNIT-RT-VALUE]
      ;; abstract over allocating so that all allocations
      ;; are shorter than this.
      [(GRef t) (sr-alloc "GRefT" l:TYPE-GREF-TAG `(("type" . ,(sr-type t))))
       
       #|
       (do (bind-state : (State Nat D0-Expr))
       (gref-u : Uid <- (uid-state "GRefT"))
       (ty-u   : Uid <- (uid-state "ty"))
       (ty-e : D0-Expr <- (sr-type t))
       (let* ([gref-v : D0-Expr (Var gref-u)]
       [ty-v   : D0-Expr (Var ty-u)]
       [alloc  : D0-Expr
       (Op 'Alloc (list TYPE-GREF-SIZE-VALUE))]
       [stmt   : D0-Expr
       (Op 'Array-set! (list gref-v GREF-TO-INDEX ty-v))]
       [ty-bnd : D0-Bnd (cons ty-u ty-e)]
       [gref-bnd : D0-Bnd (cons gref-u alloc)])
       (return-state
       (Let (list ty-bnd)
       (Let (list gref-bnd)
       (Begin (list stmt)
       (Op 'binary-or (list gref-v TYPE-GREF-TAG-VALUE))))))))
       |#]
      ;; TODO: parametrize over the logic of GRef and GVect
      
      [(GVect t) (sr-alloc "GVect_Type" l:TYPE-GVECT-TAG `(("type" . ,(sr-type t))))
       #|
       (do (bind-state : (State Nat D0-Expr))
       (gvect-u : Uid <- (uid-state "GVectT"))
       (ty-u   : Uid <- (uid-state "ty"))
       (ty-e : D0-Expr <- (sr-type t))
       (let* ([gvect-v : D0-Expr (Var gvect-u)]
       [ty-v   : D0-Expr (Var ty-u)]
       [alloc  : D0-Expr
       (Op 'Alloc (list TYPE-GVECT-SIZE-VALUE))]
       [stmt   : D0-Expr
       (Op 'Array-set! (list gvect-v GVECT-TO-INDEX ty-v))]
       [ty-bnd : D0-Bnd (cons ty-u ty-e)]
       [gvect-bnd : D0-Bnd (cons gvect-u alloc)])
       (return-state
       (Let (list ty-bnd)
       (Let (list gvect-bnd)
       (Begin (list stmt)
       (Op 'binary-or (list gvect-v TYPE-GVECT-TAG-VALUE))))))))
       |#]
      
      [(Fn a f* r)
       (sr-alloc "Fun_Type" l:TYPE-FN-TAG
                 `(("arity" . ,(Quote a))
                   ("return" . ,(sr-type r)) .
                   ,(map (lambda ([t : Schml-Type])
                           (cons "argument" (sr-type t)))
                         f*)))
       #|
       (do (bind-state : (State Nat D0-Expr))
       (tmp : Uid <- (uid-state "FunT"))
       (f*  : D0-Expr* <- (map-state sr-type f*))
       (r   : D0-Expr  <- (sr-type r))
       (let* ([tmp-var (Var tmp)]
       [bnd
       (cons tmp (Op 'Alloc (list (Quote (+ a FN-FMLS-OFFSET)))))]
       [bnd*
       (list bnd)]
       [stmt1
       (Op 'Array-set! (list tmp-var (Quote FN-ARITY-INDEX) (Quote a)))]
       [stmt2
       (Op 'Array-set! (list tmp-var (Quote FN-RETURN-INDEX) r))]
       [stmt*
       (array-set* tmp-var FN-FMLS-OFFSET f*)])
       (return-state
       (Let bnd*
       (Begin (cons stmt1 (cons stmt2 stmt*)) tmp-var)))))
       |#]
      [else (TODO implement reference code around here)])


    ;; TODO Get rid of these extra definitions once we know this code works
  #|
  (: array-set (-> D0-Expr Integer D0-Expr D0-Expr))
  (define ((array-set a) i f*)
    (Op 'Array-set! (list a (Quote i) (car f*))))
  (: array-set (-> D0-Expr Integer D0-Expr* D0-Expr*))
  (define (array-set* a i f*)
    (map (array-set a) (build-list (length f*) values) f*))
    |#)

  (: sr-coercion (Schml-Coercion -> D0-Expr))
  (define (sr-coercion c)
    (match c
      [(Identity) IDENTITY-COERCION-IMDT]
      [(Project (app sr-type t) l)
       (sr-alloc "project_coercion" l:PROJECT-COERCION-TAG
                 `(("type" . ,t) ("label" . ,(Quote l))))]
      [(Inject (app sr-type t))
       (sr-alloc "inject-coercion" l:INJECT-COERCION-TAG
                 `(("type" . ,t)))]
      [(Sequence (app sr-coercion f) (app sr-coercion s))
       (sr-alloc "sequence_coecion" l:SEQUENCE-COERCION-TAG
                 `(("first" . ,f) (,"second" . ,s)))]
      [(Fn _ a* (app sr-coercion r))
       (sr-alloc "fn_coercion" l:FN-COERCION-TAG
                 `(("arity"  . ,(Quote (length a*)))
                   ("return" . ,r) .
                   ,(map (lambda ([a : Schml-Coercion])
                           (cons "argument" (sr-coercion a)))
                         a*)))]
      [(Ref (app sr-coercion r) (app sr-coercion w))
       (sr-alloc "ref-coercion" l:REF-COERCION-TAG
                 `(("read-coercion" . ,r)
                   ("write-coercion" . ,w)))]
      [(Failed l)
       (sr-alloc "failed-coercion" l:FAILED-COERCION-TAG
                 `(("label" . ,(Quote l))))]))

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
                [env (extend* env id* (map var id*))])
           (Let b* (recur/env e env cenv)))]
        [(If (app recur t) (app recur c) (app recur a))
         (If t c a)]
        [(Op p (app recur* e*))
         (cond
           [(uil-prim-value? p) (Op p e*)]
           [(uil-prim-effect? p) (Op p e*)]
           [else (error 'specify-representation/Op "~a" p)])]
        [(Quote k)
         (cond
           [(null? k)  UNIT-IMDT]
           [(boolean? k) (if k TRUE-IMDT FALSE-IMDT)]
           [(fixnum? k) (Quote k)]
           [(string? k) (Quote k)]
           [else (error 'specify-representation/quote "given ~a" k)])]
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
                   (let-values ([(b* s*) (sr-bndc* recur c*)])
                     (Labels p* (Let b* (Begin s* (recur e))))))) 
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
        [(Type t) (sr-type t)]
        [(Type-Fn-Huh (app recur e))
         (sr-check-tag=? e TYPE-TAG-MASK TYPE-FN-TAG)]
        [(Type-Fn-arity (app recur e))
         (Op 'Array-ref (list e FN-ARITY-INDEX))]
        [(Type-Fn-return (app recur e))
         (Op 'Array-ref (list e FN-RETURN-INDEX))]
        [(Type-Fn-arg (app recur e1) (app recur e2))
         (define e2^
           (match e2
             [(Quote (? number? k)) (Quote (+ l:FN-FMLS-OFFSET k))]
             [otherwiths (Op '+ (list e2 FN-FMLS-OFFSET))]))
         (Op 'Array-ref (list e1 e2^))]
        [(Type-GRef-Huh (app recur e))
         (sr-check-tag=? e TYPE-TAG-MASK TYPE-GREF-TAG)]
        [(Type-GRef-Of (app recur e))
         (define arg : D0-Expr
           (Op 'binary-xor (list e TYPE-GREF-TAG)))
         (Op 'Array-ref (list arg GREF-TO-INDEX))]
        [(Type-GVect-Huh (app recur e))
         (sr-check-tag=? e TYPE-TAG-MASK TYPE-GVECT-TAG)]
        [(Type-GVect-Of (app recur e))
         (define arg : D0-Expr
           (Op 'binary-xor (list e TYPE-GVECT-TAG)))
         (Op 'Array-ref (list arg GVECT-TO-INDEX))]
        [(Type-Dyn-Huh (app recur e))
         (Op '= (list TYPE-DYN-RT-VALUE e))]
       
        [(Type-Tag (app recur e))
         (Op 'binary-and (list e TYPE-TAG-MASK))]
        ;; Coercions
        ;; Projection Coercions
        [(Quote-Coercion c) (sr-coercion c)]
        [(Project-Coercion (app recur t) (app recur l))
         (sr-alloc "project_coercion" l:PROJECT-COERCION-TAG
                   `(("type" . ,t) ("label" . ,l)))]
        [(Project-Coercion-Huh (app recur e))
         (sr-check-tag=?
          e COERCION-TAG-MASK PROJECT-COERCION-TAG)]
        [(Project-Coercion-Type (app recur e))
         (sr-tagged-array-ref e PROJECT-COERCION-TAG PROJECT-COERCION-TYPE-INDEX)]
        [(Project-Coercion-Label (app recur e))
         (sr-tagged-array-ref e PROJECT-COERCION-TAG PROJECT-COERCION-LABEL-INDEX)]
        ;; Injection Coercions
        [(Inject-Coercion (app recur t))
         (sr-alloc "inject_coercion" l:INJECT-COERCION-TAG `(("type" . ,t)))]
        [(Inject-Coercion-Huh (app recur e))
         (sr-check-tag=? e COERCION-TAG-MASK INJECT-COERCION-TAG)]
        [(Inject-Coercion-Type (app recur e))
         (sr-tagged-array-ref e INJECT-COERCION-TAG INJECT-COERCION-TYPE-INDEX)]
        ;; Sequence Coercions
        [(Sequence-Coercion (app recur f) (app recur s))
         (sr-alloc "sequence_coercion" l:SEQUENCE-COERCION-TAG
                   `(("first" . ,f) (,"second" . ,s)))]
        [(Sequence-Coercion-Huh (app recur e))
         (sr-check-tag=? e COERCION-TAG-MASK SEQUENCE-COERCION-TAG)]
        [(Sequence-Coercion-Fst (app recur e))
         (sr-tagged-array-ref e SEQUENCE-COERCION-TAG SEQUENCE-COERCION-FST-INDEX)]
        [(Sequence-Coercion-Snd (app recur e))
         (sr-tagged-array-ref e SEQUENCE-COERCION-TAG SEQUENCE-COERCION-SND-INDEX)]
        ;; Identity Coercions can only be created by coercion quoting
        ;; But  their representation is just (Quote ID-COERCION-TAG)
        [(Id-Coercion-Huh (app recur e))
         ;;(Op '= (list e ID-COERCION))
         (sr-check-tag=? e COERCION-TAG-MASK IDENTITY-COERCION-TAG)]
        ;; Function Coercions
        [(Fn-Coercion-Huh (app recur e))
         (sr-check-tag=? e COERCION-TAG-MASK FN-COERCION-TAG)]
        [(Fn-Coercion-Arity (app recur e))
         (sr-tagged-array-ref e FN-COERCION-TAG FN-ARITY-INDEX)]
        [(Fn-Coercion-Arg (app recur e) (app recur i))
         (sr-tagged-array-ref e FN-COERCION-TAG (sr-plus FN-FMLS-OFFSET i))]
        [(Fn-Coercion-Return (app recur e))
         (sr-tagged-array-ref e FN-COERCION-TAG FN-RETURN-INDEX)]
        ;; TODO either repurpose or get rid of the arrity field
        ;; One could immagine that we use it to dynamically dispatch on compose
        [(Fn-Coercion (app recur* e*) (app recur e))
         (sr-alloc "fn_coercion" l:FN-COERCION-TAG
                   `(("arity" . ,(Quote (length e*)))
                     ("return" . ,e) .
                     ,(map (lambda ([e : D0-Expr])
                             (cons "argument" e))
                           e*)))]
        [(Make-Fn-Coercion mk-crcn (app recur t1) (app recur t2) (app recur l))
         (: invoke-mk-fn-crcn ((Code-Label Uid) (Var Uid) D0-Expr D0-Expr -> D0-Expr))
         (define (invoke-mk-fn-crcn mk-fn t1 t2 l)
           (App-Code
            mk-fn
            (list t1 t2 l (Quote 0)
                  (sr-tagged-array-ref t1 TYPE-FN-TAG FN-ARITY-INDEX))))
         (let ([mk-fn-crcn (get-mk-fn-crcn! mk-crcn)])
           (if (Var? t1)
               (invoke-mk-fn-crcn mk-fn-crcn t1 t2 l)
               (let ([u (next-uid! "fn_type1")])
                 (Let `((,u . ,t1))
                      (invoke-mk-fn-crcn mk-fn-crcn (Var u) t2 l)))))]
        [(Compose-Fn-Coercion compose (app recur c1) (app recur c2))
         (: invoke-comp ((Code-Label Uid) (Var Uid) D0-Expr -> D0-Expr))
         (define (invoke-comp comp-fn c1 c2)
           (App-Code
            comp-fn
            (list c1 c2
                  (Quote 0)
                  (sr-tagged-array-ref c1 FN-COERCION-TAG FN-ARITY-INDEX)
                  TRUE-IMDT)))
         (let ([mk-fn-crcn (get-comp-fn-crcn! compose)])
           (if (Var? c1)
               (invoke-comp mk-fn-crcn c1 c2)
               (let ([u (next-uid! "fn_coercion1")])
                 (Let `((,u . ,c1))
                      (invoke-comp mk-fn-crcn (Var u) c2)))))]

        [(Ref-Coercion-Huh (app recur e))
         (sr-check-tag=? e COERCION-TAG-MASK REF-COERCION-TAG)]
        [(Ref-Coercion (app recur r) (app recur w))
         (sr-alloc "ref-coercion" l:REF-COERCION-TAG
                   `(("read-coercion" . ,r)
                     ("write-coercion" . ,w)))]
        [(Ref-Coercion-Read (app recur e))
         (sr-tagged-array-ref e REF-COERCION-TAG REF-COERCION-READ-INDEX)]
        [(Ref-Coercion-Write (app recur e))
         (sr-tagged-array-ref e REF-COERCION-TAG REF-COERCION-WRITE-INDEX)]
        [(Failed-Coercion-Huh (app recur e))
         (sr-check-tag=? e COERCION-TAG-MASK FAILED-COERCION-TAG)]
        ;; For now I am allocating the blame label in a box.
        ;; Make this cheaper by ensuring that the string pointer is alligned and
        ;; tagging it.
        [(Failed-Coercion (app recur l))
         (sr-alloc "failed-coercion" l:FAILED-COERCION-TAG `(("label" . ,l)))]
        [(Failed-Coercion-Label (app recur e))
         (sr-tagged-array-ref e FAILED-COERCION-TAG FAILED-COERCION-LABEL-INDEX)]
        ;; FN-Proxy Stuff
        [(Fn-Proxy i (app recur clos) (app recur crcn))
         (sr-alloc "fn-proxy" l:FN-PROXY-TAG
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
         (sr-alloc "hybrid-proxy" l:FN-PROXY-TAG
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
         (Let `((,tmp . ,e))
              (Let `((,tag . ,(Op 'binary-and `(,tmpv ,DYN-TAG-MASK))))
                   (If (Op '= `(,tagv ,DYN-BOXED-TAG))
                       (Op 'Array-ref (list tmpv DYN-TYPE-INDEX))
                       (If (Op '= `(,tagv ,DYN-INT-TAG))
                           TYPE-INT-RT-VALUE
                           (If (Op '= `(,tagv ,DYN-BOOL-TAG))
                               TYPE-BOOL-RT-VALUE
                               TYPE-UNIT-RT-VALUE)))))]
        [(Dyn-value (app recur e))
         (define tmp (next-uid! "dyn_value_tmp"))
         (define tag (next-uid! "dyn_value_tag"))
         (define tmp-var (Var tmp))
         (define tag-var (Var tag))
         (Let `((,tmp . ,e))
              (Let `((,tag . ,(Op 'binary-and `(,tmp-var ,DYN-TAG-MASK))))
                   (If (Op '= (list tag-var DYN-BOXED-TAG))
                       (Op 'Array-ref (list tmp-var DYN-VALUE-INDEX))
                       (Op '%>> (list tmp-var DYN-IMDT-SHIFT)))))]
        ;; Observable Results Representation
        [(Blame (app recur e))
         (Begin
           (list (Op 'Print (list e))
                 (Op 'Exit  (list (Quote -1))))
           UNDEF-IMDT)]
        [(Observe (app recur e) t) (sr-observe next-uid! e t)]
        ;; References Representation
        [(Begin (app recur* e*) (app recur e))
         (Begin e* e)]
        [(Repeat i (app recur e1) (app recur e2) e3)
         (Repeat i e1 e2 (recur/env e3 (extend env i (Var i)) cenv))]
        ;; Guarded
        [(Unguarded-Box (app recur e))
         (sr-alloc "unguarded_box" l:UGBOX-TAG (list (cons "init_value" e)))]
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
         (define tmp1-var (Var tmp1))
         (define tmp2-var (Var tmp2))
         (define tmp3-var (var tmp3))
         (define tmp4-var (var tmp4))
         (define tl       (Op '+ (list tmp1-var UGVECT-OFFSET)))
         (define alloc    (Op 'Alloc (list tmp4-var)))
         (define set      (Repeat i UGVECT-OFFSET tmp4-var
                                  (Op 'Array-set! (list tmp3-var (Var i) tmp2-var))))
         (define set-n    (Op 'Array-set! (list tmp3-var UGVECT-SIZE-INDEX tmp1-var)))  
         (Let `((,tmp1 . ,e1)
                (,tmp2 . ,e2))
              (Let `((,tmp4 . ,tl))
                   (Let `((,tmp3 . ,alloc))
                        (Begin `(,set-n ,set) tmp3-var))))]
        [(Unguarded-Vect-Ref (app recur e1) (app recur e2))
         (define ind  (next-uid! "index"))
         (define tmp1 (next-uid! "e"))
         (define zro (Quote 0))
         (define tmp1-var (Var tmp1))
         (define ind-var (Var ind))
         (Let (list (cons ind e2) (cons tmp1 e1))
              (If (Op '>= (list ind-var zro)) ;; vectors indices starts from 0
                  (If (Op '< (list ind-var (Op 'Array-ref (list tmp1-var zro))))
                      (Op 'Array-ref (list tmp1-var (Op '+ (list ind-var UGVECT-OFFSET))))
                      (Begin
                        (list (Op 'Printf (list (Quote "index out of bound %l\n") ind-var)))
                        (Op 'Exit (list (Quote -1)))))
                  (Begin
                    (list (Op 'Printf (list (Quote "index out of bound %l\n") ind-var)))
                    (Op 'Exit (list (Quote -1))))))]
        [(Unguarded-Vect-Set! (app recur e1) (app recur e2) (app recur e3))
         (define ind  (next-uid! "index"))
         (define tmp1 (next-uid! "ugvect"))
         (define zro (Quote 0))
         (define tmp1-var (Var tmp1))
         (define ind-var (Var ind))
         (Let (list (cons ind e2))
              (If (Op '>= (list ind-var zro)) ;; vectors indices starts from 0
                  (Let (list (cons tmp1 e1))
                       (If (Op '< (list ind-var (Op 'Array-ref (list tmp1-var zro))))
                           (Op 'Array-set! (list tmp1-var (Op '+ (list ind-var UGVECT-OFFSET)) e3))
                           (Begin
                             (list (Op 'Printf (list (Quote "index out of bound %l\n") ind-var)))
                             (Op 'Exit (list (Quote -1))))))
                  (Begin
                    `(,(Op 'Printf (list (Quote "index out of bound %l\n") ind-var)))
                    (Op 'Exit (list (Quote -1))))))]
        [(Guarded-Proxy-Huh (app recur e))
         (Op '= `(,(Op 'binary-and (list e GREP-TAG-MASK))
                  ,GPROXY-TAG))]
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
        [other (error 'specify-representation "unmatched ~a" other)]))
    (recur exp))
  (recur/env exp env cenv))

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
  (Let `((,ref . ,ref-e) (,src . ,src-e)
         (,tar . ,tar-e) (,lbl . ,lbl-e))
   (Let `((,proxy . ,(Op 'Alloc (list GPROXY/TWOSOME-SIZE))))
    (Begin
      (list
       (Op 'Array-set! (list var GPROXY-FOR-INDEX    (Var ref)))
       (Op 'Array-set! (list var GPROXY-FROM-INDEX   (Var src)))
       (Op 'Array-set! (list var GPROXY-TO-INDEX     (Var tar)))
       (Op 'Array-set! (list var GPROXY-BLAMES-INDEX (Var lbl))))
      (Op 'binary-or (list var GPROXY-TAG))))))

(: alloc-tag-set-gproxy/coercion
   ((String -> Uid) D0-Expr D0-Expr -> D0-Expr))
(define (alloc-tag-set-gproxy/coercion uid! ref-e crcn-e)
  (define proxy (uid! "guarded_proxy"))
  (define ref   (uid! "guarded_ref"))
  (define crcn  (uid! "coercion"))
  (define var   (Var proxy))
  (Let `((,ref . ,ref-e) (,crcn . ,crcn-e))
   (Let `((,proxy . ,(Op 'Alloc (list GPROXY/COERCION-SIZE))))
    (Begin
      (list
       (Op 'Array-set! (list var GPROXY-FOR-INDEX      (Var ref)))
       (Op 'Array-set! (list var GPROXY-COERCION-INDEX (Var crcn))))
      (Op 'binary-or (list var GPROXY-TAG))))))


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
      [(Int? t) (Op 'Printf (list (Quote "Int : %d\n") (Var id)))]
      [(Unit? t) (Op 'Printf (list (Quote "Unit : ()\n")))]
      [(Bool? t) (If (Var id)
                     (Op 'Print (list (Quote "Bool : #t\n")))
                     (Op 'Print (list (Quote "Bool : #f\n"))))]
      [(Fn? t) (Op 'Print (list (Quote "Function : ?\n")))]
      [(GRef? t) (Op 'Print (list (Quote "GReference : ?\n")))]
      [(GVect? t) (Op 'Print (list (Quote "GVector : ?\n")))]
      [(Dyn? t) (Op 'Print (list (Quote "Dynamic : ?\n")))]
      [else (TODO implement thing for reference types)]))
  (let* ([res (uid! "result")])
    (Let (list (cons res e))
      (Begin (list (generate-print res t)) (Success)))))

#;(TODO GET RID OF TAGS IN THE COMPILER)
(: sr-tag (Tag-Symbol -> (Quote Integer)))
(define (sr-tag t)
  (case t
    [(Int)    DYN-INT-TAG]
    [(Bool)   DYN-BOOL-TAG]
    [(Unit)   DYN-UNIT-TAG]
    [(Atomic) TYPE-ATOMIC-TAG]
    [(Fn)     TYPE-FN-TAG]
    [(GRef)   TYPE-GREF-TAG]
    [(GVect)  TYPE-GVECT-TAG]
    [(Boxed)  DYN-BOXED-TAG]))



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


(: sr-bndc* ((CoC6-Expr -> D0-Expr) CoC6-Bnd-Closure* -> (Values D0-Bnd* D0-Expr*)))
(define (sr-bndc* sr-expr b*)
  (: sr-bndc (CoC6-Bnd-Closure -> (Pair D0-Bnd D0-Expr*)))
  (define (sr-bndc bnd)
    (match-let ([(cons uid (Closure-Data lbl ctr? free*)) bnd])
      (let* ([lbl   (sr-expr lbl)]
             [free* (map sr-expr free*)]
             [data  (cons lbl (if ctr? (cons (sr-expr ctr?) free*) free*))]
             [size  (length data)]
             [clos  (Var uid)]
             [bnd   (cons uid (Op 'Alloc `(,(Quote size))))]
             [set*  (for/list : (Listof D0-Expr)
                              ([d : D0-Expr data]
                               [i : Integer (in-naturals)])
                      (Op 'Array-set! (list clos (Quote i) d)))])
        (cons bnd set*))))
  (let* ([b.e*  (map sr-bndc b*)]
         [b*    (map (inst car D0-Bnd Any) b.e*)]
         [e*    (append-map (inst cdr Any D0-Expr*) b.e*)])
    ;; This code implies that the tag of a closure is #b000
    (values b* e*)))



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
