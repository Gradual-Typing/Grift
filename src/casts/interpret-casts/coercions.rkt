#lang typed/racket/base

(require
 racket/match
 "../../helpers.rkt"
 "../../language/cast-or-coerce3.rkt"
 "../interpret-casts/help.rkt")

(provide get-coercion-helpers ComposeT ApplyT Cast-Rule Fn-ProxyT)

(: get-coercion-helpers Get-Helpers-Type)
(define (get-coercion-helpers fn-proxy-rep) 
  (do (bind-state : (State Nat (List Cast-Rule ComposeT ApplyT Fn-ProxyT CoC3-Bnd-Code*)))
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
                    (App-Code (Code-Label make-uid) (list t1 t2 l))))]
                ;; This is basically a huge hack to keep the compiler
                ;; Going until we get to closure conversion
                [apply : ApplyT
                 (lambda (e e*)
                   (return-state (App-Fn-or-Proxy interp-uid e e*)))]
                [proxy : Fn-ProxyT
                 (lambda (i e1 e2)
                   (return-state (Fn-Proxy (list i interp-uid) e1 e2)))])
           
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
             (return-state (list interp compose apply proxy bnd*))))))









(define-type Cast/Coercion-Type
  (CoC3-Trivial CoC3-Trivial -> (State Nat CoC3-Expr)))

(: cast/coercion (Cast/Coercion-Type
                  Make-Coercion-Type
                  Compose-Coercions-Type ->
                  Cast/Coercion-Type))
(define ((cast/coercion cast mk-crcn comp-crcn) v c)
  ;; apply-cast is normally specified in terms of inspecting the
  ;; value. But in a world were value reflection on values is limited
  ;; the coercions can drive the casting process.
  (cond$
   [(id?$ c) (return-state v)]
   [(seq?$ c)
    ;; I think that something better could be done here
    ;; why recur there are only so many things that the underlying
    ;; sequence could be?
    (let$* ([seq_fst (seq-fst$ c)]
           [seq_snd (seq-snd$ c)])
     (bind-state
      (cast v seq_fst)
      (lambda ([e : CoC3-Expr])
        (let$* ([v_fst_cast e])
         (cast v_fst_cast seq_snd)))))]
   ;; By the typing rules of coercions we know v must be a dyn value
   [(prj?$ c)
    (let$* ([prj_type  (prj-type$ c)]
           [prj_label (prj-label$ c)]
           ;; The current implementation of the dyn interface is not a good one
           ;; this line is going to have to recover the type from dyn twice
           [dyn_value (Dyn-value v)]
           [dyn_type  (Dyn-type v)])
     (bind-state
      ;; Maybe this line should be a call to compose?
      ;; which would do the same thing but wouldn't duplicate
      ;; this line across two functions
      ;; I did it this way because I know this code won't build
      ;; the injection coercion
      (mk-crcn dyn_type prj_type prj_label)
      (lambda ([e : CoC3-Expr])
        (let$* ([projected_type e])
              (cast dyn_value projected_type)))))]
   [(inj?$ c) (return-state (Dyn-make v (inj-type$ c)))]
   [(fnC?$ c)
    (return-state
     (If (Fn-Proxy-Huh v)
         (App-Code (Fn-Caster (Fn-Proxy-Closure v)) (list v c))
         (App-Code (Fn-Caster v) (list v c))))]
   [(ref?$ c)
    (if$ (Guarded-Proxy-Huh v)
         (let$* ([old_crcn (Guarded-Proxy-Coercion v)])
          (bind-state
           (comp-crcn old_crcn c)
           (lambda ([comp_crcn : CoC3-Expr])
             : (State Nat CoC3-Expr)
             (let$* ([composed_crcn comp_crcn])
              (if$ (id?$ composed_crcn)
                   (return-state v)
                   (return-state
                    (Guarded-Proxy
                     (Guarded-Proxy-Ref v)
                     (Coercion composed_crcn))))))))
         (return-state (Guarded-Proxy v (Coercion c))))]
   ;; the coercion must be failure
   [else (return-state (Blame (fail-label$ c)))]))

(define-type Make-Coercion-Type
  (CoC3-Trivial CoC3-Trivial CoC3-Trivial -> (State Nat CoC3-Expr)))

(: make-coercion (Make-Coercion-Type Uid -> Make-Coercion-Type))
(define ((make-coercion mk-crcn mk-crcn-uid) t1 t2 lbl)
  (cond$
   [(op=? t1 t2) (return-state (Quote-Coercion (Identity)))]
   [(and$ (fnT?$ t1) (fnT?$ t2))
    ;; This line is a little tricky because we
    ;; unless we have actual types for this at
    ;; compile time we have to generate code
    ;; that can handle arbitry fn-arity.
    ;; We delegate this task to specify representation because
    ;; it involves safely allocating a object whos size cannot
    ;; be determined until run-time.
    (if$ (op=? (fnT-arity$ t1) (fnT-arity$ t2))
         ((mk-fn-crcn$ mk-crcn mk-crcn-uid) t1 t2 lbl)
         (return-state (Blame lbl)))]
   [(and$ (gvect?$ t1) (gvect?$ t2))
    (let$* ([gv1_of (gvect-of$ t1)]
            [gv2_of (gvect-of$ t2)])
     (do (bind-state : (State Nat CoC3-Expr))
         (r : CoC3-Expr <- (mk-crcn gv1_of gv2_of lbl))
         (w : CoC3-Expr <- (mk-crcn gv2_of gv1_of lbl))
         (let$* ([read_crcn r]
                 [write_crcn w])
          (return-state (ref$ read_crcn write_crcn)))))]
   [(and$ (gref?$ t1) (gref?$ t2))
    (let$* ([gr1_of (gref-of$ t1)]
            [gr2_of (gref-of$ t2)])
     (do (bind-state : (State Nat CoC3-Expr))
         (r : CoC3-Expr <- (mk-crcn gr1_of gr2_of lbl))
         (w : CoC3-Expr <- (mk-crcn gr2_of gr1_of lbl))
         (let$* ([read_crcn r]
                 [write_crcn w])
          (return-state (ref$ read_crcn write_crcn)))))]
   ;; This is absolutly necisarry
   ;; While Injections and Projections are never made by
   ;; source code coercions composition can create new
   ;; projections and injections.
   [(dyn?$ t1)
    (return-state
     (seq$ (prj$ t2 lbl) (Quote-Coercion (Identity))))]
   [(dyn?$ t2)
    (return-state
     (seq$ (Quote-Coercion (Identity)) (inj$ t1)))]
   [else (return-state (fail$ lbl))]))

(: mk-fn-crcn$ (Make-Coercion-Type Uid -> Make-Coercion-Type))
(define ((mk-fn-crcn$  mk-crcn uid) t1 t2 lbl)
  (: help-comp (Make-Coercion-Type CoC3-Trivial ->
                (Schml-Type Schml-Type -> (State Nat Schml-Coercion))))
  (define ((help-comp mk-crcn lbl) t g)
    (bind-state
     (mk-crcn (Type t) (Type g) lbl)
     (lambda ([c : CoC3-Expr])
       : (State Nat Schml-Coercion)
       (if (Quote-Coercion? c)
           (return-state (Quote-Coercion-const c))
          ;; This should always be possible if all the code is correct
           (error 'mk-fn-crcn$/help-comp)))))
  (cond
    [(and (Type? t1) (Type? t2))
     (let* ([t1 (Type-type t1)]
            [t2 (Type-type t2)])
       (unless (and (Fn? t1) (Fn? t2))
         (error 'mk-fn-crcn$))
       (let* ([t1-args (Fn-fmls t1)]
              [t2-args (Fn-fmls t2)]
              [t1-ret  (Fn-ret  t1)]
              [t2-ret  (Fn-ret  t2)])
         (do (bind-state : (State Nat CoC3-Expr))
             (arg* : (Listof Schml-Coercion)
                   <- (map-state2 (help-comp mk-crcn lbl) t2-args t1-args))
           (ret  : Schml-Coercion <- ((help-comp mk-crcn lbl) t1-ret t2-ret))
           (return-state (Quote-Coercion (Fn (Fn-arity t1) arg* ret))))))]
    ;; In the two cases below we could speculatively generate
    ;; the code for a coercion with known arity
    ;;[(Type? t1)]
    ;;[(Type? t2)]
    [else (return-state (Make-Fn-Coercion uid t1 t2 lbl))]))

(define-type Compose-Coercions-Type
  (CoC3-Trivial CoC3-Trivial -> (State Nat CoC3-Expr)))

(: compose-coercions (Compose-Coercions-Type
                      Uid
                      Make-Coercion-Type ->
                      Compose-Coercions-Type))
(define ((compose-coercions compose compose-uid mk-crcn) c1 c2)
  (cond$
   ;; Eliminating the Identities cuts down on the number of branches
   ;; and should be fast
   [(id?$ c1) (return-state c1)]
   [(id?$ c2) (return-state c2)]
   ;; We could elminate failure on the left next, but we choose to make
   ;; success as fast as possible even if it introduces more branches overall.
   [(seq?$ c1)
    (let$* ([seq_fst (seq-fst$ c1)]
           [seq_snd (seq-snd$ c1)])
     (cond$
      [(prj?$ seq_fst)
       (bind-state
        (compose seq_snd c2)
        (lambda ([crcn : CoC3-Expr])
          : (State Nat CoC3-Expr)
          (let$* ([comp_seq_snd crcn])
           (return-state (seq$ seq_fst comp_seq_snd)))))]
      ;; We have to priority failure on the right over injection
      [(fail?$ c2) (return-state c2)]
      ;; Because of the typeing rule for coercions we know that
      ;; c2 must be a (I?;i) aka projection sequence because
      ;; it must have the type dyn => I.
      [else ;;  (inj?$ seq_snd) only thing that is left in normal form
       (let$* ([proj  (seq-fst$ c2)]
               [final (seq-snd$ c2)]
               [t1    (inj-type$ seq_snd)]
               [t2    (prj-type$ proj)]
               [lbl   (prj-label$ proj)])
       (bind-state
        (mk-crcn t1 t2 lbl)
        (lambda ([crcn : CoC3-Expr])
          : (State Nat CoC3-Expr)
          (let$* ([comp_prj_inj crcn])
             (bind-state
              (compose seq_fst comp_prj_inj)
              (lambda ([crcn : CoC3-Expr])
                : (State Nat CoC3-Expr)
                (let$* ([comp_fst_pi crcn])
                 (compose comp_fst_pi final))))))))]))]
   [(seq?$ c2)
    (cond$
     [(fail?$ c1) (return-state c2)]
     [else
      ;; must be c1 & (g;I?)
      (let$* ([seq_fst (seq-fst$ c2)]
             [seq_snd (seq-snd$ c2)])
       (bind-state
        (compose c1 seq_fst)
        (lambda ([crcn : CoC3-Expr])
          : (State Nat CoC3-Expr)
          (let$* ([comp_c1_final crcn])
            (return-state (seq$ comp_c1_final seq_snd))))))])]
   ;; All Branches from here out will have to check if c2 is failure
   ;; so we do it once to eliminate the possibility
   [(fail?$ c2)
    (if$ (fail?$ c1) (return-state c1)  (return-state c2))]
   [(fnC?$ c1) ;; c2 must be a Function Coercion
    ((compose-fn-crcn$
      (compose-coercions compose compose-uid mk-crcn)
      compose-uid) c1 c2)]
   [(ref?$ c1) ;; c2 must also be a reference coercion
    (let$* ([ref1_read  (ref-read$  c1)]
            [ref1_write (ref-write$ c1)]
            [ref2_read  (ref-read$  c2)]
            [ref2_write (ref-write$ c2)])
     (do (bind-state : (State Nat CoC3-Expr))
         (r : CoC3-Expr <- (compose ref1_read  ref2_read))
         (w : CoC3-Expr <- (compose ref2_write ref1_write))
         (let$* ([read  r]
                 [write w])
          (if$ (and$ (id?$ read) (id?$ write))
               (return-state (Quote-Coercion (Identity)))
               (return-state (ref$ read write))))))]
   ;; C1 must be a failed coercion
   [else (return-state (Blame (fail-label$ c1)))]))

(: compose-fn-crcn$ (Compose-Coercions-Type Uid -> Compose-Coercions-Type))
(define ((compose-fn-crcn$ compose compose-uid) c1 c2)
  (: help-comp (Compose-Coercions-Type
                -> (Schml-Coercion Schml-Coercion
                     -> (State Nat Schml-Coercion))))
  (define ((help-comp compose) s t)
    (do (bind-state : (State Nat Schml-Coercion))
        (c : CoC3-Expr <- (compose (Quote-Coercion s) (Quote-Coercion t)))
      (if (Quote-Coercion? c)
          (return-state (Quote-Coercion-const c))
          (error 'compose-fn-coercn/help))))
  (cond
    [(and (Quote-Coercion? c1) (Quote-Coercion? c2))
     (let* ([c1 (Quote-Coercion-const c1)]
            [c2 (Quote-Coercion-const c2)])
       (unless (and (Fn? c1) (Fn? c2))
         (error 'compose-fn-crcn$ "given ~a ~a" c1 c2))
  (let* ([c1-args (Fn-fmls c1)]
         [c2-args (Fn-fmls c2)]
         [c1-ret  (Fn-ret  c1)]
         [c2-ret  (Fn-ret  c2)])
    (do (bind-state : (State Nat CoC3-Expr))
        (arg* : (Listof Schml-Coercion)
              <- (map-state2 (help-comp compose) c2-args c1-args))
      (ret  : Schml-Coercion <- ((help-comp compose) c1-ret c2-ret))
      (return-state (Quote-Coercion (Fn (Fn-arity c1) arg* ret))))))]
;; TODO Consider the case were we have one but not the other type
[else (return-state (Compose-Fn-Coercion compose-uid c1 c2))]))


