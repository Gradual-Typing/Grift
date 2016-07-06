#lang typed/racket/base
#|-----------------------------------------------------------------------------+
|Pass: src/casts/interpret-casts                                          |
+------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                             |
+------------------------------------------------------------------------------+
|Description:
- This pass creates a cast-interpreter that can cast arbitrary values at
runtime.
- Converts runtime casts to applications of the cast-interpreter.
- Specializes casts with known types, at this point represented with the cast
form, to the shortest branch of the cast tree that is relevant.
- Replaces types that are structure with a psuedo constructor call
- Replaces tags atomic types with the Type form.
- Introduces the short lived dynamic type manipulation forms
+------------------------------------------------------------------------------+
|Input Grammar Cast2-Language                                                  |
|Output Grammar Cast3-Language                                                 |
+-----------------------------------------------------------------------------|#
;; The define-pass syntax
(require
 racket/match
 racket/format
 "../helpers.rkt"
 "../errors.rkt"
 "../configuration.rkt"
 "../language/cast-or-coerce2.rkt"
 "../language/cast-or-coerce3.rkt"
 "./interpret-casts-help.rkt")

(provide
 interpret-casts/twosomes
 (all-from-out
  "../language/cast-or-coerce2.rkt"
  "../language/cast-or-coerce3.rkt"))

;; Unfortanately this pass delegates most of the work to two other files.
;;;; This occured because typed racket started running out of memory while
;;;; type-checking. Part of the excessive memory consumption is due to
;;;; Typed Racket inturning alot of information on a per file basis.
;;;; Splitting a file therefore causes a dump of this information before
;;;; continuing to typecheck.
;;;; Perhaps we should suggest generational interning.
;; The two files to look for are:
;; interpret-casts/twosomes.rkt
;; interpret-casts/coercions.rkt
;; The common code for these files is stored in:
;; interpret-casts/help.rkt



(: interpret-casts/twosomes
   (Cast-or-Coerce2-Lang Config  ->  Cast-or-Coerce3-Lang))
(define (interpret-casts/twosomes prgm config)
  ;; Configuration options that determine how code is generated
  (: cast-rep Cast-Representation)
  (define cast-rep (Config-cast-rep config))
  (: specialize-casts? (Parameterof Boolean))
  (define specialize-casts? (make-parameter #f))
  (: recursive-dyn-cast? (Parameterof Boolean))
  (define recursive-dyn-cast? (make-parameter #t))

  ;; Desugaring the input program into its constituents 
  (match-define (Prog (list prgm-name prgm-next prgm-type) prgm-exp)
    prgm)

  ;; Just a tad of mutable state to generate unique identifiers
  (define next-unique-number : (Boxof Nat) (box prgm-next))
  ;; And syntax for introducing bindings when needed
  (define-syntax-let$* let$* next-unique-number)
  ;; And unconditional binding introduction
  (: next-uid! (String -> Uid))
  (define (next-uid! x)
    (let ([n (unbox next-unique-number)])
      (set-box! next-unique-number (add1 n))
      (Uid x n)))

  ;; The uid for the runtime interpreted cast
  (define interp-cast-uid : Uid (next-uid! "interp_cast"))
  
  ;; These templates are used to build the code that performs
  ;; casting at runtime.
  ;; In my opinion they currently do too much manual specialization
  ;; and we should make them simpler but implement a specialization
  ;; pass that derives code with similar efficiency. AMK 2015-12-17
  (define-type Cast-Rule/Cast-Rule
    ((CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr)
     CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr
     -> CoC3-Expr))
  
  ;; How to cast any type to some other type
  (: cast-any/twosome Cast-Rule/Cast-Rule)
  (define (cast-any/twosome cast-undyned v t1 t2 lbl)
    (let$* ([type1 t1] [type2 t2])
     (cond$
      [(op=? type1 type2) v]
      [(op=? type1 (Type DYN-TYPE))
       (cast-dyn/twosome cast-undyned v type1 type2 lbl)]
      [else (cast-ground/twosome v type1 type2 lbl)])))

  ;; How to cast any Injectable type to any other type
  (: cast-ground/twosome (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
  (define (cast-ground/twosome v t1 t2 lbl)
    (let$* ([value v] [type1 t1] [type2 t2])
     (cond$
      [(op=? type1 (Type INT-TYPE))
       (if$ (op=? (Type DYN-TYPE) type2)
            (Dyn-make value (Type INT-TYPE))
            (Blame lbl))]
      [(op=? type1 (Type BOOL-TYPE))
       (if$ (op=? (Type DYN-TYPE) type2)
            (Dyn-make value (Type BOOL-TYPE))
            (Blame lbl))]
      [(op=? type1 (Type UNIT-TYPE))
       (if$ (op=? (Type DYN-TYPE) type2)
            (Dyn-make value (Type UNIT-TYPE))
            (Blame lbl))]
      [else
       (let$* ([tag1 (type-tag type1)])
        (cond$
         [(op=? (Tag 'Fn) tag1)
          (cast-fn/twosome value type1 type2 lbl)]
         [(op=? (Tag 'GRef) tag1)
          (cast-gref/twosome value type1 type2 lbl)]
         [(op=? (Tag 'GVect) tag1)
          (cast-gvect/twosome value type1 type2 lbl)]
         [(op=? (Tag 'STuple) tag1)
          (cast-tuple/twosome value type1 type2 lbl)]
         [else (Blame (Quote "Unexpected Type1 in cast tree"))]))])))

  ;; How to cast a Guarded Reference to some other type
  (: cast-gref/twosome (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
  (define (cast-gref/twosome v t1 t2 lbl)
    (: gref-arg (CoC3-Expr -> CoC3-Expr))
    (define (gref-arg t)
      (if (Type? t)
          (let ([t (Type-type t)])
            (if (GRef? t)
                (Type (GRef-arg t))
                (error 'interpret-casts "unexpected type in gref-arg")))
          (Type-GRef-Of t)))
    (: proxy-gref (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
    (define (proxy-gref val type1 type2 lbl)
      (let$* ([tag_gref (type-tag type2)])
       (if$ (op=? tag_gref (Tag 'GRef))
            (let$* ([g1 (gref-arg type1)]
                    [g2 (gref-arg type2)])
              (Guarded-Proxy val (Twosome g1 g2 lbl)))
            (Blame lbl))))
    (let$* ([val v] [type1 t1] [type2 t2] [label lbl])
           (if$ (op=? (Type DYN-TYPE) type2)
                (Dyn-make val type1)
                (proxy-gref val type1 type2 label))))

  ;; How to Cast a Guarded Vector to some other type
  (: cast-gvect/twosome (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
  (define (cast-gvect/twosome v t1 t2 lbl)
    ;; Get the vector type argument either by compile time reflection
    ;; or generating runtime code.
    (: gvect-arg (CoC3-Expr -> CoC3-Expr))
    (define (gvect-arg t)
      (if (Type? t)
          (let ([t (Type-type t)])
            (if (GVect? t)
                (Type (GVect-arg t))
                (error 'interpret-casts "unexpected type in gvect-arg")))
          (Type-GVect-Of t)))
    ;; Generate the code to create a proxy if the target type is
    ;; actually a Gvector
    (: proxy-gvect (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
    (define (proxy-gvect val type1 type2 lbl)
      (let$* ([tag_gvect (type-tag type2)])
             (if$ (op=? tag_gvect (Tag 'GVect))
                  (let$* ([g1 (gvect-arg type1)]
                          [g2 (gvect-arg type2)])
                         (Guarded-Proxy val (Twosome g1 g2 lbl)))
                  (Blame lbl))))
    ;; Check to see if we are casting to dyn, if so box the value.
    ;; Otherwise either cast to a gvector type or blame the label.
    (let$* ([val v] [type1 t1] [type2 t2] [label lbl])
           (if$ (op=? (Type DYN-TYPE) type2)
                (Dyn-make val type1)
                (proxy-gvect val type1 type2 label))))

  ;; How to Cast a Function to some other type
  (: cast-fn/twosome (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
  (define (cast-fn/twosome v t1 t2 lbl)
    (let$* ([value v] [type1 t1] [type2 t2] [label lbl])
     (if$ (op=? (Type DYN-TYPE) type2)
          (Dyn-make value type1)
          (let$* ([tag2 (type-tag type2)])
           (if$ (op=? tag2 (Tag 'Fn))
                (let$* ([type1_arity (type-fn-arity type1)]
                        [type2_arity (type-fn-arity type2)])
                 (if$ (op=? type1_arity type2_arity)
                      (App-Code (Fn-Caster value)
                                (list value t1 type2 lbl))
                      (Blame lbl)))
                (Blame lbl))))))

  (: cast-tuple/twosome (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
  (define (cast-tuple/twosome v t1 t2 lbl)
    (let$* ([value v] [type1 t1] [type2 t2] [label lbl])
           (if$ (op=? (Type DYN-TYPE) type2)
                (Dyn-make value type1)
                (if (and (Type? t1) (Type? t2))
                    (let ([t1t (Type-type t1)]
                          [t2t (Type-type t2)])
                      (if (and (STuple? t1t) (STuple? t2t))
                          (let-values ([(bnd* arg*)
                                        (for/fold ([b* : CoC3-Bnd* '()]
                                                   [arg* : Uid* '()])
                                                  ([i (in-range (STuple-num t2t))])
                                          (unless (index? i)
                                            (error 'interpret-casts-with-coercions "bad index"))
                                          (let ([a (next-uid! "item_type")])
                                            (values (cons (cons a (let$* ([item (Tuple-proj v i)]
                                                                          [new-t1t (tuple-type-arg$ t1 i)]
                                                                          [new-t2t (tuple-type-arg$ t2 i)])
                                                                         (interpret-cast item new-t1t new-t2t lbl))) b*)
                                                    (cons a arg*))))])
                            (Let bnd* (Create-tuple (map (inst Var Uid) arg*))))
                          (error 'cast-tuple/twosome)))
                    (let$* ([tag2 (type-tag type2)])
                           (if$ (op=? tag2 (Tag 'STuple))
                                (let$* ([type1_num (type-tuple-num type1)]
                                        [type2_num (type-tuple-num type2)])
                                       (if$ (op<=? type2_num type1_num)
                                            (Cast-Tuple interp-cast-uid v t1 t2 lbl)
                                            (Blame type2_num)))
                                (Blame lbl)))))))

  ;; How to extract a dynamic value
  (: cast-dyn/twosome Cast-Rule/Cast-Rule)
  (define (cast-dyn/twosome cast-undyned v t1 t2 lbl)
    #| This should be:
       (typecase v
        [n : t1 (cast-undyned v^ t1^ t2 lbl)])
       ==> 
       (case v
        [(Dynamic v^ t1^) ...])
       ==>
       (
       Efficient Implementation will require remove-let before
       specify representation. TODO
       For now I am going to implement this
       (dyn-destruct v (v^ t1)
        (cast-undyned v^ t1^ t2 lbl))
    |#
    (let$* ([val v] [tag (Dyn-tag val)])
     (cond$
      [(op=? (Tag 'Int) tag)
       (cast-undyned (Dyn-immediate val) (Type INT-TYPE) t2 lbl)]
      [(op=? (Tag 'Bool) tag)
       (cast-undyned (Dyn-immediate val) (Type BOOL-TYPE) t2 lbl)]
      [(op=? (Tag 'Unit) tag)
       (cast-undyned (Quote '()) (Type UNIT-TYPE) t2 lbl)]
      [(op=? (Tag 'Boxed) tag)
       (cast-undyned (Dyn-value val) (Dyn-type val) t2 lbl)]
      [else (Blame (Quote "Unexpected value in cast tree"))])))

  ;; How to cast a non dynamic value to another type
  ;; Notice this is used in conjuction with interpret cast based
  ;; on the setting of the parameter recursive-dyn-cast
  ;; when set to #t the cast used is a call to the interpret
  ;; cast runtime routine instead of unfolding the cast tree even
  ;; I am pretty sure this is the desired behavior
  ;; There is an invarient that there will only ever be one level of dyn
  (: spec-cast-undyned (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
  (define (spec-cast-undyned v t1 t2 l)
    (let$* ([value v] [type1 t1] [type2 t2] [label l])
      ;; If unboxed at the exact type then just return the value
      (if (op=? type1 type2)
          value
          ;; Otherwise it was either unboxed at the wrong type
          ;; or it is an inter structured type cast.
          ;; Either way we build a cast tree specific to this type.
          (cast-ground/twosome value type1 type2 label))))


  ;; Build a call to the runtime interpreted cast code
  (: interpret-cast (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
  (define (interpret-cast v t1 t2 l)
    (App-Code (Code-Label interp-cast-uid) (list v t1 t2 l)))

  ;; Build a specialized cast for the given code
  (: specialize-cast (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> CoC3-Expr))
  (define (specialize-cast v t1 t2 l)
    (if (recursive-dyn-cast?)
        (cast-any/twosome interpret-cast v t1 t2 l)
        (cast-any/twosome spec-cast-undyned v t1 t2 l)))
  
  (define interp-cast-binding : CoC3-Bnd-Code
    (let* ([v  (next-uid! "value")]
           [t1 (next-uid! "type1")]
           [t2 (next-uid! "type2")]
           [l  (next-uid! "label")])
      `(,interp-cast-uid
        .
        ,(Code (list v t1 t2 l)
               (specialize-cast (Var v) (Var t1) (Var t2) (Var l))))))
  
  ;; ic-expr maps over the expressions lowering function cast
  ;; into calls to the runtime cast interpreter and direct manipulation
  ;; of the representation for each type and value.
  (: ic-expr (CoC2-Expr -> CoC3-Expr))
  (define (ic-expr exp)
    (match exp
      ;; Interesting Transformations
      [(Interpreted-Cast
        (app ic-expr v)
        (Twosome (app ic-expr t1) (app ic-expr t2) (app ic-expr l)))
       (if (and (specialize-casts?)
                (or (Type? t1) (Type? t2)))
           (specialize-cast v t1 t2 l)
           (interpret-cast v t1 t2 l))]
      [(Cast (app ic-expr v) (Twosome t1 t2 l))
       (if (specialize-casts?)
           (specialize-cast v (Type t1) (Type t2) (Quote l))
           (interpret-cast v (Type t1) (Type t2) (Quote l)))]
      [(Fn-Proxy i e1 e2)
       (error 'ic "function proxies not yet implemented")]
      [(App/Fn-Proxy-Huh e e*)
       (error 'ic "function proxy apply not yet implemented")]
      [(Fn-Proxy-Huh e)
       (error 'ic "function proxies not yet implemented")]
      [(Fn-Proxy-Closure e)
       (error 'ic "function proxies not yet implemented")]
      ;; Uniteresting Recursion till the end
      [(Code-Label u)
       (Code-Label u)]
      [(Labels c* e)
       (Labels (map ic-bndc c*) (ic-expr e))]
      [(App-Code e e*)
       (App-Code (ic-expr e) (map ic-expr e*)) ]
      [(Lambda f* (Castable ctr e))
       (Lambda f* (Castable ctr (ic-expr e)))]
      [(App-Fn e e*)
       (App-Fn (ic-expr e) (map ic-expr e*))]
      [(Letrec b* e)
       (Letrec (map ic-bnd b*) (ic-expr e))]
      [(Let b* e)
       (Let (map ic-bnd b*) (ic-expr e))]
      [(Op p e*)
       (Op p (map ic-expr e*))]
      [(Fn-Caster e)
       (Fn-Caster (ic-expr e))]
      ;; Type manipulation
      [(Type-Fn-arg e i)
       (Type-Fn-arg (ic-expr e) (ic-expr i))]
      [(Type-Fn-return e)
       (Type-Fn-return (ic-expr e))]
      [(Type-Fn-arity e)
       (Type-Fn-arity (ic-expr e))]
      [(Blame e)
       (Blame (ic-expr e))]
      [(If tst csq alt)
       (If (ic-expr tst) (ic-expr csq) (ic-expr alt))]
      [(Var i) (Var i)]
      [(Type t) (Type t)]
      [(Quote k) (Quote k)]
      [(Begin e* e)
       (Begin (map ic-expr e*) (ic-expr e))]
      [(Repeat i e1 e2 e3)
       (Repeat i (ic-expr e1) (ic-expr e2) (ic-expr e3))]
      [(Unguarded-Box e)
       (Unguarded-Box (ic-expr e))]
      [(Unguarded-Box-Ref e)
       (Unguarded-Box-Ref (ic-expr e))]
      [(Unguarded-Box-Set! e1 e2)
       (Unguarded-Box-Set! (ic-expr e1) (ic-expr e2))]
      [(Unguarded-Vect e1 e2)
       (Unguarded-Vect (ic-expr e1) (ic-expr e2))]
      [(Unguarded-Vect-Ref e1 e2)
       (Unguarded-Vect-Ref (ic-expr e1) (ic-expr e2))]
      [(Unguarded-Vect-Set! e1 e2 e3)
       (Unguarded-Vect-Set! (ic-expr e1) (ic-expr e2) (ic-expr e3))]
      [(Guarded-Proxy-Huh e)
       (Guarded-Proxy-Huh (ic-expr e))]
      [(Guarded-Proxy e (Twosome t1 t2 l))
       (Guarded-Proxy
        (ic-expr e)
        (Twosome (ic-expr t1) (ic-expr t2) (ic-expr l)))]
      [(Guarded-Proxy-Ref e)
       (Guarded-Proxy-Ref (ic-expr e))]
      [(Guarded-Proxy-Source e)
       (Guarded-Proxy-Source (ic-expr e))]
      [(Guarded-Proxy-Target e)
       (Guarded-Proxy-Target (ic-expr e))]
      [(Guarded-Proxy-Blames e)
       (Guarded-Proxy-Blames (ic-expr e))]
      [(Create-tuple e*)
       (Create-tuple (map ic-expr e*))]
      [(Tuple-proj e i) (Tuple-proj (ic-expr e) i)]
      ;; Coercions manipulation
      [other
       (if (or (Compose-Coercions? other)
               (Id-Coercion-Huh? other)
               (Fn-Coercion? other)
               (Fn-Coercion-Arg? other)
               (Fn-Coercion-Return? other)
               (Ref-Coercion? other)
               (Ref-Coercion-Read? other)
               (Ref-Coercion-Write? other)
               (Quote-Coercion? other)
               (Guarded-Proxy? other)
               (Fn-Proxy-Coercion? other)
               (Guarded-Proxy-Coercion? other))
           (error 'ic-expr "coercion code in twosome pass: ~a" other)
           (error 'ic-expr "umatched ~a" other))]))

  ;; map through a code binding 
  (: ic-bndc (CoC2-Bnd-Code -> CoC3-Bnd-Code))
  (define (ic-bndc b)
    (match-let ([(cons u (Code u* e)) b])
      (cons u (Code u* (ic-expr e)))))

  ;; map through a data binding 
  (: ic-bnd (CoC2-Bnd -> CoC3-Bnd))
  (define (ic-bnd b)
    (cons (car b) (ic-expr (cdr b))))

  ;; body of interpret-casts
  (let* ([exp (ic-expr prgm-exp)]
         [next (unbox next-unique-number)])
    (Prog (list prgm-name next prgm-type)
          (Labels (list interp-cast-binding)
                  (Observe exp prgm-type)))))






