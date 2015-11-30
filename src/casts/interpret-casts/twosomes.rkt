#lang typed/racket
(require
 racket/match
 "../../helpers.rkt"
 "../../language/cast-or-coerce3.rkt"
 "./help.rkt")

(provide get-twosome-helpers ComposeT ApplyT Cast-Rule Fn-ProxyT)

(: specialize-casts? (Parameterof Boolean))
(define specialize-casts?
  (make-parameter #f))

(: recursive-dyn-cast? (Parameterof Boolean))
(define recursive-dyn-cast?
  (make-parameter #t))

(: get-twosome-helpers Get-Helpers-Type)
(define (get-twosome-helpers fn-proxy-rep)
  (do (bind-state : (State Nat (List Cast-Rule ComposeT ApplyT Fn-ProxyT CoC3-Bnd-Code*)))
         (interp-cast : Uid <- (uid-state "interp_cast"))
       (let* ([interp
               : Cast-Rule
               ;; This cast rule will delegate casts to the
               ;; routine that represents the runtimes interp
               ;; cast functioninterp : Cast-Rule
               (lambda (v c)
                 (match c
                   [(Twosome t1 t2 l)
                    (return-state
                     (App-Code (Code-Label interp-cast) (list v t1 t2 l)))]
                   [other (error 'ic/interp-cast-rule "~a" c)]))]
              [spec
               : Cast-Rule
               ;; This cast rule will specialize casts base on
               ;; compile time informations of the types involved
               ;; in the cast.
               ;; The different versions differ in what to do
               ;; when casting from dyn
               (if (recursive-dyn-cast?)
                   (lambda (v c)
                     (match c
                       [(Twosome t1 t2 l) (cast-any/twosome interp v t1 t2 l)]
                       [other (error 'ic/spec-cast-rule "~a" c)]))
                   (lambda (v c)
                     (match c
                       [(Twosome t1 t2 l)
                        (cast-any/twosome spec-cast-undyned v t1 t2 l)]
                       [other (error 'ic/spec-cast-rule "~a" c)])))]
              [judge
               : Cast-Rule
               ;; This cast rule will specialize only specialize
               ;; if the input doesn't require additional computation
               (if (specialize-casts?)
                   ;; Specializing on immediate values in uneeded
                   ;; because we would have their source type if
                   ;; they were an immediate values
                   (lambda (v c)
                     (match c
                       [(Twosome t1 t2 _)
                        (if (or (Type? t1) (Type? t2))                        
                            (spec v c)
                            (interp v c))]
                       [other (error 'ic/judge-cast-rule "~a" c)]))
                   interp)]
              ;; The twosome representation doesn't compose casts
              ;; so this function just raises an error saying so.
              [compose
               : ComposeT
               (lambda (c1 c2)
                 (error 'ic/compose "called using twosome ~a ~a" c1 c2))]
              [apply : ApplyT
               (if (eq? 'Data fn-proxy-rep)
                   (error 'interp-cast/twosomes/apply
                          "data representation not yet implemented")
                   (lambda (e e*)
                     (error 'interp-cast/twosome/apply
                            "app/fn-proxy-huh node shouldn't exist")))]
              [proxy : Fn-ProxyT
                     (lambda (i e1 e2)
                       (error 'interp-cast/twosome/proxy))])
         ;; This is a little weird but leads to code reuse.
         ;; Notice invoking specialize on vars will cause the
         ;; entire cast decision tree to be built.
         ;; We then wrap up this entire tree as C function to
         ;; create the runtime routine that can cast any function
         (v  : Uid <- (uid-state "val"))
         (t1 : Uid <- (uid-state "type1"))
         (t2 : Uid <- (uid-state "type2"))
         (l  : Uid <- (uid-state "label"))
         (cast-tree : CoC3-Expr
          <- (spec (Var v) (Twosome (Var t1) (Var t2) (Var l))))
         (let* ([interp-cast-code (Code `(,v ,t1 ,t2 ,l) cast-tree)]
                [interp-cast-bnd* : CoC3-Bnd-Code*
                                  `((,interp-cast . ,interp-cast-code))])
           (return-state (list judge compose apply proxy interp-cast-bnd*))))))




;; TODO we might be able to help the type checker a little by reducing
;; the size of these unions to Trivial
(define-type Cast-Prim-Rule
  (Cast-Rule CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> (State Nat CoC3-Expr)))

(define-type Cast-Aux-Rule
  (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> (State Nat CoC3-Expr)))

;; How to cast any type to some other type
(: cast-any/twosome Cast-Prim-Rule)
(define (cast-any/twosome cast-undyned v t1 t2 lbl)
  (let$* ([type1 t1] [type2 t2])
         (cond$
          [(op=? type1 type2) (return-state v)]
          [(op=? type1 (Type DYN-TYPE)) (cast-dyn/twosome cast-undyned v type1 type2 lbl)]
          [else (cast-ground/twosome v type1 type2 lbl)])))

;; How to extract a dynamic value
(: cast-dyn/twosome Cast-Prim-Rule)
(define (cast-dyn/twosome cast-undyned v t1 t2 lbl)
  (let$* ([val v] [tag (Dyn-tag val)])
         (cond$
          [(op=? (Tag 'Int) tag)
           (cast-undyned (Dyn-immediate val) (Twosome (Type INT-TYPE) t2 lbl))]
          [(op=? (Tag 'Bool) tag)
           (cast-undyned (Dyn-immediate val) (Twosome (Type BOOL-TYPE) t2 lbl))]
          [(op=? (Tag 'Unit) tag)
           (cast-undyned (Quote '()) (Twosome (Type UNIT-TYPE) t2 lbl))]
          [(op=? (Tag 'Boxed) tag)
           (cast-undyned (Dyn-value val) (Twosome (Dyn-type val) t2 lbl))]
          [else (return-state (Blame (Quote "Unexpected value in cast tree")))])))

;; How to cast any Injectable type to any other type
(: cast-ground/twosome Cast-Aux-Rule)
(define (cast-ground/twosome v t1 t2 lbl)
  (let$* ([type1 t1])
   (cond$
    [(op=? type1 (Type INT-TYPE))  (cast-int/twosome v type1 t2 lbl)]
    [(op=? type1 (Type BOOL-TYPE)) (cast-bool/twosome v type1 t2 lbl)]
    [(op=? type1 (Type UNIT-TYPE)) (cast-unit/twosome v type1 t2 lbl)]
    [else
     (let$* ([tag1 (type-tag type1)])
            (cond$
             [(op=? (Tag 'Fn) tag1) (cast-fn/twosome v type1 t2 lbl)]
             [(op=? (Tag 'GRef) tag1) (cast-gref/twosome v type1 t2 lbl)]
             [(op=? (Tag 'GVect) tag1) (cast-gvect/twosome v type1 t2 lbl)]
             [else
              (return-state
               (Blame (Quote "Unexpected Type1 in cast tree")))]))])))

;; How to cast an Int to Any type
(: cast-int/twosome Cast-Aux-Rule)
(define (cast-int/twosome v t1 t2 lbl)
  (let$* ([val v][type2 t2])
         (cond$
          [(op=? (Type DYN-TYPE) type2) (return-state (Dyn-make val (Type INT-TYPE)))]
          [else (return-state (Blame lbl))])))

;; How to Cast Unit to some other type
(: cast-unit/twosome Cast-Aux-Rule)
(define (cast-unit/twosome v t1 t2 lbl)
  (let$* ([val v][type2 t2])
         (cond$
          [(op=? (Type DYN-TYPE) type2)
           (return-state (Dyn-make val (Type UNIT-TYPE)))]
          [else (return-state (Blame lbl))])))

;; How to Cast Bool to some other type
(: cast-bool/twosome Cast-Aux-Rule)
(define (cast-bool/twosome v t1 t2 lbl)
  (let$* ([val v][type2 t2])
         (cond$
          [(op=? (Type DYN-TYPE) type2)
           (return-state (Dyn-make val (Type BOOL-TYPE)))]
          [else (return-state (Blame lbl))])))

;; How to cast a Guarded Reference to some other type
(: cast-gref/twosome Cast-Aux-Rule)
(define (cast-gref/twosome v t1 t2 lbl)
  (: gref-arg (CoC3-Expr -> CoC3-Expr))
  (define (gref-arg t)
    (match t
      [(Type (GRef a)) (Type a)]
      [other (Type-GRef-Of t)]))
  (: proxy-gref Cast-Aux-Rule)
  (define (proxy-gref val type1 type2 lbl)
    (let$* ([tag_gref (type-tag type2)])
           (if$ (op=? tag_gref (Tag 'GRef))
                (let$* ([g1 (gref-arg type1)]
                        [g2 (gref-arg type2)])
                       (return-state (Guarded-Proxy val (Twosome g1 g2 lbl))))
                (return-state (Blame lbl)))))
  (let$* ([val v] [type1 t1] [type2 t2])
         (if$ (op=? (Type DYN-TYPE) type2)
              (return-state (Dyn-make val t1))
              (proxy-gref val type1 type2 lbl))))

;; How to Cast a Guarded Vector to some other type
(: cast-gvect/twosome Cast-Aux-Rule)
(define (cast-gvect/twosome v t1 t2 lbl)
  (: gvect-arg (CoC3-Expr -> CoC3-Expr))
  (define (gvect-arg t)
    (match t
      [(Type (GVect a)) (Type a)]
      [other (Type-GVect-Of t)]))
  (: proxy-gvect Cast-Aux-Rule)
  (define (proxy-gvect val type1 type2 lbl)
    (let$* ([tag_gvect (type-tag type2)])
           (if$ (op=? tag_gvect (Tag 'GVect))
                (let$* ([g1 (gvect-arg type1)]
                        [g2 (gvect-arg type2)])
                       (return-state (Guarded-Proxy val (Twosome g1 g2 lbl))))
                (return-state (Blame lbl)))))
  (let$* ([val v] [type1 t1] [type2 t2])
         (if$ (op=? (Type DYN-TYPE) type2)
              (return-state (Dyn-make val t1))
              (proxy-gvect val type1 type2 lbl))))

;; How to Cast a Function to some other type
(: cast-fn/twosome Cast-Aux-Rule)
(define (cast-fn/twosome v t1 t2 lbl)
  (let$* ([type1 t1]
          [type2 t2])
   (if$ (op=? (Type DYN-TYPE) type2)
        (return-state (Dyn-make v type1))
        (let$* ([tag2 (type-tag type2)])
         (if$ (op=? tag2 (Tag 'Fn))
              (let$* ([type1_arity (type-fn-arity type1)]
                      [type2_arity (type-fn-arity type2)]
                      [value v])
               (if$ (op=? type1_arity type2_arity)
                    (return-state
                     (App-Code (Fn-Caster value)
                               (list value t1 type2 lbl)))
                    (return-state (Blame lbl))))
              (return-state (Blame lbl)))))))

;; How to cast a non dynamic value to another type
;; Notice this is used in conjuction with interpret cast based
;; on the setting of the parameter recursive-dyn-cast
;; when set to #t the cast used is a call to the interpret
;; cast runtime routine instead of unfolding the cast tree even
;; I am pretty sure this is the desired behavior
;; There is an invarient that there will only ever be one level of dyn
(: spec-cast-undyned Cast-Rule)
(define (spec-cast-undyned v c)
  (match-let ([(Twosome t1 t2 l) c])
    (let$* ([type1 t1] [type2 t2])
     (cond$
      [(op=? type1 type2) (return-state v)]
      ;; TODO
      ;; We should try using t2 instead which could lead to better
      ;; cast specialization in the case that we just
      ;; extracted a nested type
      [else (cast-ground/twosome v type1 type2 l)]))))
