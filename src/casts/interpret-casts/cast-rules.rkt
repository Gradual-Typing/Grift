#lang typed/racket/base

(require
 racket/match
 racket/format
 "../helpers.rkt"
 "../errors.rkt"
 "../configuration.rkt"
 "../language/cast-or-coerce2.rkt"
 "../language/cast-or-coerce3.rkt"
 "./interpret-casts/cast-rules.rkt")


(: specialize-casts? (Parameterof Boolean))
(define specialize-casts?
  (make-parameter #f))

(: recursive-dyn-cast? (Parameterof Boolean))
(define recursive-dyn-cast?
  (make-parameter #t))


(define-type ComposeT (CoC3-Expr CoC3-Expr -> (State Nat CoC3-Expr)))

;; A Cast rule is part of the decision tree for allowed versus
;; not allowed casts. They use a few macros that keep invariants
;; managable and allow literals to prune the tree to only possible
;; needed branches
(define-type Cast-Rule
  (CoC3-Expr (U (Twosome CoC3-Expr CoC3-Expr CoC3-Expr)
                (Coercion CoC3-Expr))
             -> (State Nat CoC3-Expr)))



(: get-cast-helpers
   (Cast-Representation
    ->
    (State Nat (List Cast-Rule ComposeT CoC3-Bnd-Code*))))
(define (get-cast-helpers rep)
  (cond
    [(eq? rep 'Coercions)
     (do (bind-state : (State Nat (List Cast-Rule ComposeT CoC3-Bnd-Code*)))
         (interp-uid  : Uid <- (uid-state "interp_cast"))
         (compose-uid : Uid <- (uid-state "compose_coercion"))
         (make-uid    : Uid <- (uid-state "make_coercion"))
         (let* ([interp
                 : Cast-Rule
                 (lambda (v c)
                   (match c
                     [(Coercion c)
                      (return-state
                       (App-Code (Code-Label interp-uid) (list v c)))]
                     [other (error 'ic/interp-cast-rule "~a" c)]))]
                [interp_
                 : Cast/Coercion-Type
                 (lambda (v c)
                   (return-state (App-Code (Code-Label interp-uid) (list v c)))
                   (interp v (Coercion c)))]
                [compose
                 : ComposeT
                 ;; This is more general than Compose-Coercion-Type
                 (lambda (c1 c2)
                   (return-state
                    (App-Code (Code-Label compose-uid) (list c1 c2))))]
                [make
                 : Make-Coercion-Type
                 (lambda (t1 t2 l)
                   (return-state
                    (App-Code (Code-Label make-uid) (list t1 t2 l))))])
           ;; This is a little weird but leads to code reuse.
           ;; Notice invoking specialize on vars will cause the
           ;; entire cast decision tree to be built.
           ;; We then wrap up this entire tree as C function to
           ;; create the runtime routine that can cast any function
           (v  : Uid <- (uid-state "value"))
           (c  : Uid <- (uid-state "coercion"))
           (c1 : Uid <- (uid-state "coercion1"))
           (c2 : Uid <- (uid-state "coercion2"))
           (t1 : Uid <- (uid-state "type1"))
           (t2 : Uid <- (uid-state "type2"))
           (l  : Uid <- (uid-state "label"))
           (cast-tree : CoC3-Expr
            <- ((cast/coercion interp_ make compose) (Var v) (Var c)))
           (compose-tree : CoC3-Expr
            <- ((compose-coercions compose compose-uid make)
                (Var c1) (Var c2)))
           (make-tree  : CoC3-Expr
            <- ((make-coercion make make-uid) (Var t1) (Var t2) (Var l)))
           (let* ([interp-code  (Code `(,v ,c) cast-tree)]
                  [compose-code (Code `(,c1 ,c2) compose-tree)]
                  [make-code    (Code `(,t1 ,t2 ,l) make-tree)]
                  [bnd* : CoC3-Bnd-Code*
                        `((,interp-uid  . ,interp-code)
                          (,compose-uid . ,compose-code)
                          (,make-uid   . ,make-code))])
             (return-state (list interp compose bnd*)))))]
    #|[(eq? rep 'Twosomes)
     (do (bind-state : (State Nat (List Cast-Rule ComposeT CoC3-Bnd-Code*)))
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
                 (error 'ic/compose "called using twosome ~a ~a" c1 c2))])
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
           (return-state (list judge compose interp-cast-bnd*)))))]|#
    [else (error 'ic/get-cast-helpers "~a" rep)]))
