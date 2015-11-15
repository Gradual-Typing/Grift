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
         "../language/data-representation.rkt")

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

;; Only allocate each of these once
(define FN-ARITY-INDEX-VALUE        : D0-Expr (Quote FN-ARITY-INDEX))
(define FN-RETURN-INDEX-VALUE       : D0-Expr (Quote FN-RETURN-INDEX))
(define FN-FMLS-OFFSET-VALUE        : D0-Expr (Quote FN-FMLS-OFFSET))
                                   
(define TYPE-TAG-MASK-VALUE         : D0-Expr (Quote TYPE-TAG-MASK))
(define TYPE-FN-TAG-VALUE           : D0-Expr (Quote TYPE-FN-TAG))
(define TYPE-ATOMIC-TAG-VALUE       : D0-Expr (Quote TYPE-ATOMIC-TAG))
(define TYPE-DYN-RT-VALUE-VALUE     : D0-Expr (Quote TYPE-DYN-RT-VALUE))
(define TYPE-INT-RT-VALUE-VALUE     : D0-Expr (Quote TYPE-INT-RT-VALUE))
(define TYPE-BOOL-RT-VALUE-VALUE    : D0-Expr (Quote TYPE-BOOL-RT-VALUE))
(define TYPE-UNIT-RT-VALUE-VALUE    : D0-Expr (Quote TYPE-UNIT-RT-VALUE))
(define TYPE-GREF-SIZE-VALUE        : D0-Expr (Quote TYPE-GREF-SIZE))
(define TYPE-GREF-TAG-VALUE         : D0-Expr (Quote TYPE-GREF-TAG))
(define TYPE-GVECT-SIZE-VALUE       : D0-Expr (Quote TYPE-GVECT-SIZE))
(define TYPE-GVECT-TAG-VALUE        : D0-Expr (Quote TYPE-GVECT-TAG))
(define DYN-TAG-MASK-VALUE          : D0-Expr (Quote DYN-TAG-MASK))
(define DYN-BOXED-TAG-VALUE         : D0-Expr (Quote DYN-BOXED-TAG))
(define DYN-INT-TAG-VALUE           : D0-Expr (Quote DYN-INT-TAG))
(define DYN-BOOL-TAG-VALUE          : D0-Expr (Quote DYN-BOOL-TAG))
(define DYN-UNIT-TAG-VALUE          : D0-Expr (Quote DYN-UNIT-TAG))
(define DYN-IMDT-SHIFT-VALUE        : D0-Expr (Quote DYN-IMDT-SHIFT))
(define DYN-BOX-SIZE-VALUE          : D0-Expr (Quote DYN-BOX-SIZE))
(define DYN-VALUE-INDEX-VALUE       : D0-Expr (Quote DYN-VALUE-INDEX))
(define DYN-TYPE-INDEX-VALUE        : D0-Expr (Quote DYN-TYPE-INDEX))
(define FALSE-IMDT-VALUE            : D0-Expr (Quote FALSE-IMDT))
(define TRUE-IMDT-VALUE             : D0-Expr (Quote TRUE-IMDT))
(define UNDEF-IMDT-VALUE            : D0-Expr (Quote UNDEF-IMDT))
(define UNIT-IMDT-VALUE             : D0-Expr (Quote UNIT-IMDT))
(define GREP-TAG-MASK-VALUE         : D0-Expr (Quote GREP-TAG-MASK))
(define UGBOX-SIZE-VALUE            : D0-Expr (Quote UGBOX-SIZE))
(define UGBOX-VALUE-INDEX-VALUE     : D0-Expr (Quote UGBOX-VALUE-INDEX))
(define UGVECT-SIZE-INDEX-VALUE     : D0-Expr (Quote UGVECT-SIZE-INDEX))
(define UGVECT-OFFSET-VALUE         : D0-Expr (Quote UGVECT-OFFSET))
(define GPROXY-TAG-VALUE            : D0-Expr (Quote GPROXY-TAG))
(define GPROXY/TWOSOME-SIZE-VALUE   : D0-Expr (Quote GPROXY/TWOSOME-SIZE))
(define GPROXY/COERCION-SIZE-VALUE  : D0-Expr (Quote GPROXY/COERCION-SIZE))
(define GPROXY-COERCION-INDEX-VALUE : D0-Expr (Quote GPROXY-COERCION-INDEX))
(define GPROXY-FOR-INDEX-VALUE      : D0-Expr (Quote GPROXY-FOR-INDEX))
(define GPROXY-FROM-INDEX-VALUE     : D0-Expr (Quote GPROXY-FROM-INDEX))
(define GPROXY-TO-INDEX-VALUE       : D0-Expr (Quote GPROXY-TO-INDEX))
(define GPROXY-BLAMES-INDEX-VALUE   : D0-Expr (Quote GPROXY-BLAMES-INDEX))
(define CLOS-CODE-INDEX-VALUE       : D0-Expr (Quote CLOS-CODE-INDEX))
(define CLOS-CSTR-INDEX-VALUE       : D0-Expr (Quote CLOS-CSTR-INDEX))
(define CLOS-FVAR-OFFSET-VALUE      : D0-Expr (Quote CLOS-FVAR-OFFSET))
(define GREF-TO-INDEX-VALUE         : D0-Expr (Quote GREF-TO-INDEX))
(define GVECT-TO-INDEX-VALUE        : D0-Expr (Quote GVECT-TO-INDEX))

(: specify-representation (Cast-or-Coerce6-Lang Config -> Data0-Lang))
(trace-define (specify-representation prgm comp-config)
  (match-let ([(Prog (list name next type) exp) prgm])
    (let* ([next : (Boxof Nat) (box next)]
           [exp (sr-expr next (hash) empty-index-map exp)]
           [next : Nat (unbox next)])
      (Prog (list name next type) exp))))

;; Env must be maintained as a mapping from uids to how to access those
;; values. This is important because uid references to variable inside a
;; closure must turn into memory loads.

(define-type IndexMap (Uid Uid -> Nat))

(: sr-expr ((Boxof Nat) Env IndexMap CoC6-Expr -> D0-Expr))
(define (sr-expr next env cenv exp)
  ;; This is the only piece of code that should touch the unique counter
  (: next-uid! (String -> Uid))
  (define (next-uid! x)
    (let ([n (unbox next)])
      (set-box! next (add1 n))
      (Uid x n)))

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
          (Op '+ (list (Op '%<< (list e1 DYN-IMDT-SHIFT-VALUE))
                       DYN-INT-TAG-VALUE))]
         [(Type (Bool))
          (Op '+ (list (Op '%<< (list e1 DYN-IMDT-SHIFT-VALUE))
                       DYN-BOOL-TAG-VALUE))]
         [(Type (Unit))
          (Op '+ (list (Op '%<< (list e1 DYN-IMDT-SHIFT-VALUE))
                       DYN-UNIT-TAG-VALUE))]
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
        (Let `((,tag . ,(Op 'binary-and `(,type-var ,TYPE-TAG-MASK-VALUE))))
         (If (Op '= `(,tag-var ,TYPE-ATOMIC-TAG-VALUE))
             (Let `((,imm . ,(Op '%<< (list val-var DYN-IMDT-SHIFT-VALUE))))
              (If (Op '= `(,type-var ,TYPE-INT-RT-VALUE-VALUE))
                  (Op 'binary-or `(,imm-var ,DYN-INT-TAG-VALUE))
                  (If (Op '= `(,type-var ,TYPE-BOOL-RT-VALUE-VALUE))
                      (Op 'binary-or `(,imm-var ,DYN-BOOL-TAG-VALUE))
                      (Op 'binary-or `(,imm-var ,DYN-UNIT-TAG-VALUE)))))
             (sr-alloc "dynamic_boxed" #b000
                       `(("" . ,val-var)
                         ("" . ,type-var)))))))]))
  
  (: sr-type (Schml-Type -> D0-Expr))
  (define (sr-type t)
        (match t
      [(Int)  (Quote TYPE-INT-RT-VALUE)]
      [(Bool) (Quote TYPE-BOOL-RT-VALUE)]
      [(Dyn)  (Quote TYPE-DYN-RT-VALUE)]
      [(Unit) (Quote TYPE-UNIT-RT-VALUE)]
      ;; abstract over allocating so that all allocations
      ;; are shorter than this.
      [(GRef t) (sr-alloc "GRefT" TYPE-GREF-TAG `(("type" . ,(sr-type t))))
       
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
       (Op 'Array-set! (list gref-v GREF-TO-INDEX-VALUE ty-v))]
       [ty-bnd : D0-Bnd (cons ty-u ty-e)]
       [gref-bnd : D0-Bnd (cons gref-u alloc)])
       (return-state
       (Let (list ty-bnd)
       (Let (list gref-bnd)
       (Begin (list stmt)
       (Op 'binary-or (list gref-v TYPE-GREF-TAG-VALUE))))))))
       |#]
      ;; TODO: parametrize over the logic of GRef and GVect
      
      [(GVect t) (sr-alloc "GVect_Type" TYPE-GVECT-TAG `(("type" . ,(sr-type t))))
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
       (Op 'Array-set! (list gvect-v GVECT-TO-INDEX-VALUE ty-v))]
       [ty-bnd : D0-Bnd (cons ty-u ty-e)]
       [gvect-bnd : D0-Bnd (cons gvect-u alloc)])
       (return-state
       (Let (list ty-bnd)
       (Let (list gvect-bnd)
       (Begin (list stmt)
       (Op 'binary-or (list gvect-v TYPE-GVECT-TAG-VALUE))))))))
       |#]
      
      [(Fn a f* r)
       (sr-alloc "Fun_Type" TYPE-FN-TAG
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
           [(null? k)  UNIT-IMDT-VALUE]
           [(boolean? k) (if k TRUE-IMDT-VALUE FALSE-IMDT-VALUE)]
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
         (Op 'Array-ref (list e CLOS-CSTR-INDEX-VALUE))]
        [(Closure-code (app recur e))
         (Op 'Array-ref (list e CLOS-CODE-INDEX-VALUE))]
        [(Closure-ref clos fvar)
         (Op 'Array-ref (list (Var clos) (Quote (cenv clos fvar))))]
        [(Var i) (lookup env i)]
        [(Labels (app (sr-bnd-code* recur/env) b*) e)
         (let* ([u* (map (inst car Uid Any) b*)]
                [l* (map label u*)])
           (Labels b* (recur/env e (extend* env u* l*) cenv)))]
        [(App-Code e e*) (App-Code (recur e) (recur* e*))]
        [(Code-Label u) (Code-Label u)]
        ;; Type Representation
        [(Type t) (sr-type t)]
        [(Type-Fn-arity (app recur e))
         (Op 'Array-ref (list e FN-ARITY-INDEX-VALUE))]
        [(Type-Fn-return (app recur e))
         (Op 'Array-ref (list e FN-RETURN-INDEX-VALUE))]
        [(Type-Fn-arg (app recur e1) (app recur e2))
         (define e2^
           (match e2
             [(Quote (? number? k)) (Quote (+ FN-FMLS-OFFSET k))]
             [otherwiths (Op '+ (list e2 FN-FMLS-OFFSET-VALUE))]))
         (Op 'Array-ref (list e1 e2^))]
        [(Type-GRef-Of (app recur e))
         (define arg : D0-Expr
           (Op 'binary-xor (list e TYPE-GREF-TAG-VALUE)))
         (Op 'Array-ref (list arg GREF-TO-INDEX-VALUE))]
        [(Type-GVect-Of (app recur e))
         (define arg : D0-Expr
           (Op 'binary-xor (list e TYPE-GVECT-TAG-VALUE)))
         (Op 'Array-ref (list arg GVECT-TO-INDEX-VALUE))]
        [(Type-Tag (app recur e))
         (Op 'binary-and (list e TYPE-TAG-MASK-VALUE))]
        ;; Fix me I should never have been exposed
        [(Tag t) (sr-tag t)]
        ;; Dynamic Values Representation
        [(Dyn-tag (app recur e))
         (Op 'binary-and (list e DYN-TAG-MASK-VALUE))]
        [(Dyn-immediate (app recur e))
         (Op '%>> (list e DYN-IMDT-SHIFT-VALUE))]
        [(Dyn-make (app recur e1) e2)
         (sr-dyn-make recur e1 e2)]
        [(Dyn-type (app recur e))
         (define tmp (next-uid! "tmp"))
         (define tag (next-uid! "tag"))
         (define tagv (Var tag))
         (define tmpv (Var tmp))
         (Let `((,tmp . ,e))
          (Let `((,tag . ,(Op 'binary-and `(,tmpv ,DYN-TAG-MASK-VALUE))))
           (If (Op '= `(,tagv ,DYN-BOXED-TAG-VALUE))
               (Op 'Array-ref (list tmpv DYN-TYPE-INDEX-VALUE))
               (If (Op '= `(,tagv ,DYN-INT-TAG-VALUE))
                   TYPE-INT-RT-VALUE-VALUE
                   (If (Op '= `(,tagv ,DYN-BOOL-TAG-VALUE))
                       TYPE-BOOL-RT-VALUE-VALUE
                       TYPE-UNIT-RT-VALUE-VALUE)))))]
        [(Dyn-value (app recur e))
         (define tmp (next-uid! "dyn_value_tmp"))
         (define tag (next-uid! "dyn_value_tag"))
         (define tmp-var (Var tmp))
         (define tag-var (Var tag))
         (Let `((,tmp . ,e))
          (Let `((,tag . ,(Op 'binary-and `(,tmp-var ,DYN-TAG-MASK-VALUE))))
           (If (Op '= (list tag-var DYN-BOXED-TAG-VALUE))
               (Op 'Array-ref (list tmp-var DYN-VALUE-INDEX-VALUE))
               (Op '%>> (list tmp-var DYN-IMDT-SHIFT-VALUE)))))]
        ;; Observable Results Representation
        [(Blame (app recur e))
         (Begin
           (list (Op 'Print (list e))
                 (Op 'Exit  (list (Quote -1))))
           UNDEF-IMDT-VALUE)]
        [(Observe (app recur e) t) (sr-observe next-uid! e t)]
        ;; References Representation
        [(Begin (app recur* e*) (app recur e))
         (Begin e* e)]
        [(Repeat i (app recur e1) (app recur e2) e3)
         (Repeat i e1 e2 (recur/env e3 (extend env i (Var i)) cenv))]
        ;; Guarded
        [(Unguarded-Box (app recur e))
         (sr-alloc "unguarded_box" UGBOX-TAG (list (cons "init_value" e)))]
        [(Unguarded-Box-Ref (app recur e))
         (Op 'Array-ref (list e UGBOX-VALUE-INDEX-VALUE))]
        [(Unguarded-Box-Set! (app recur e1) (app recur e2))
         (Op 'Array-set! (list e1 UGBOX-VALUE-INDEX-VALUE e2))]
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
         (define tl       (Op '+ (list tmp1-var UGVECT-OFFSET-VALUE)))
         (define alloc    (Op 'Alloc (list tmp4-var)))
         (define set      (Repeat i UGVECT-OFFSET-VALUE tmp4-var
                                  (Op 'Array-set! (list tmp3-var (Var i) tmp2-var))))
         (define set-n    (Op 'Array-set! (list tmp3-var UGVECT-SIZE-INDEX-VALUE tmp1-var)))  
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
                  (Op 'Array-ref (list tmp1-var (Op '+ (list ind-var UGVECT-OFFSET-VALUE))))
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
                   (Op 'Array-set! (list tmp1-var (Op '+ (list ind-var UGVECT-OFFSET-VALUE)) e3))
                   (Begin
                     (list (Op 'Printf (list (Quote "index out of bound %l\n") ind-var)))
                     (Op 'Exit (list (Quote -1))))))
              (Begin
                `(,(Op 'Printf (list (Quote "index out of bound %l\n") ind-var)))
                (Op 'Exit (list (Quote -1))))))]
        [(Guarded-Proxy-Huh (app recur e))
         (Op '= `(,(Op 'binary-and (list e GREP-TAG-MASK-VALUE))
                  ,GPROXY-TAG-VALUE))]
        [(Guarded-Proxy (app recur e) r)
         ;; Consider using sr-alloc here
         (match r
           [(Twosome (app recur t1) (app recur t2) (app recur l))
            (alloc-tag-set-gproxy/twosome next-uid! e t1 t2 l)]
           [(Coercion (app recur c))
            (alloc-tag-set-gproxy/coercion next-uid! e c)])]
        [(Guarded-Proxy-Ref (app recur e))
         ((untag-deref-gproxy GPROXY-FOR-INDEX-VALUE) e)]
        [(Guarded-Proxy-Source (app recur e))
         ((untag-deref-gproxy GPROXY-FROM-INDEX-VALUE) e)]
        [(Guarded-Proxy-Target (app recur e))
         ((untag-deref-gproxy GPROXY-TO-INDEX-VALUE) e)]
        [(Guarded-Proxy-Blames (app recur e))
         ((untag-deref-gproxy GPROXY-BLAMES-INDEX-VALUE) e)]
        [(Guarded-Proxy-Coercion (app recur e))
         ((untag-deref-gproxy GPROXY-COERCION-INDEX-VALUE) e)]
        [other (error 'specify-representation "unmatched ~a" other)]))
    (recur exp))
  (recur/env exp env cenv))

(: untag-deref-gproxy (-> D0-Expr (-> D0-Expr D0-Expr)))
(define ((untag-deref-gproxy index) proxy)
  (Op 'Array-ref
      (list (Op 'binary-xor (list proxy GPROXY-TAG-VALUE))
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
   (Let `((,proxy . ,(Op 'Alloc (list GPROXY/TWOSOME-SIZE-VALUE))))
    (Begin
      (list
       (Op 'Array-set! (list var GPROXY-FOR-INDEX-VALUE    (Var ref)))
       (Op 'Array-set! (list var GPROXY-FROM-INDEX-VALUE   (Var src)))
       (Op 'Array-set! (list var GPROXY-TO-INDEX-VALUE     (Var tar)))
       (Op 'Array-set! (list var GPROXY-BLAMES-INDEX-VALUE (Var lbl))))
      (Op 'binary-or (list var GPROXY-TAG-VALUE))))))

(: alloc-tag-set-gproxy/coercion
   ((String -> Uid) D0-Expr D0-Expr -> D0-Expr))
(define (alloc-tag-set-gproxy/coercion uid! ref-e crcn-e)
  (define proxy (uid! "guarded_proxy"))
  (define ref   (uid! "guarded_ref"))
  (define crcn  (uid! "coercion"))
  (define var   (Var proxy))
  (Let `((,ref . ,ref-e) (,crcn . ,crcn-e))
   (Let `((,proxy . ,(Op 'Alloc (list GPROXY/COERCION-SIZE-VALUE))))
    (Begin
      (list
       (Op 'Array-set! (list var GPROXY-FOR-INDEX-VALUE      (Var ref)))
       (Op 'Array-set! (list var GPROXY-COERCION-INDEX-VALUE (Var crcn))))
      (Op 'binary-or (list var GPROXY-TAG-VALUE))))))


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
    [(Int)    (Quote DYN-INT-TAG)]
    [(Bool)   (Quote DYN-BOOL-TAG)]
    [(Unit)   (Quote DYN-UNIT-TAG)]
    [(Atomic) (Quote TYPE-ATOMIC-TAG)]
    [(Fn)     (Quote TYPE-FN-TAG)]
    [(GRef)   (Quote TYPE-GREF-TAG)]
    [(GVect)  (Quote TYPE-GVECT-TAG)]
    [(Boxed)  (Quote DYN-BOXED-TAG)]))



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
  (Op 'Array-ref  (list clos CLOS-CODE-INDEX-VALUE)))

(: sr-clos-ref-caster (-> D0-Expr D0-Expr))
(define (sr-clos-ref-caster clos)
  (Op 'Array-ref  (list clos CLOS-CSTR-INDEX-VALUE)))


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

(: cref  (-> Uid Natural D0-Expr))
(define (cref clos index)
  (Op 'Array-ref (list (Var clos) (Quote (+ index CLOS-FVAR-OFFSET)))))

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
