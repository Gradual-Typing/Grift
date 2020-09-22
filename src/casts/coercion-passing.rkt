#lang typed/racket/base/no-check
;; Translation of expressions into Coercion-Passing Style
;; Assumed: (cast-representation) is (Coercion)

(require
 racket/match
 "../language/forms.rkt"  ;; IDENTITY, ID-EXPR, ZERO-EXPR
 "../language/syntax.rkt"
 "../language/syntax-with-constants.rkt"
 "./interpret-casts-common.rkt"  ;; apply-code
 )
;; next-uid! from ../unique-identifiers.rkt

(provide coercion-passing-trans)


(: coercion-passing-trans : Uid Uid -> (CoC3-Expr -> CoC3-Expr))
(define (coercion-passing-trans #:apply-coercion-uid apply-coercion-uid
                                #:compose-coercions-uid compose-coercions-uid)

  ;; cf. interpret-casts/coercions
  (define (apply-coercion [v : CoC3-Expr]
                          [c : CoC3-Expr]
                          [top-level? : CoC3-Expr (Quote #t)])
    : CoC3-Expr
    (apply-code apply-coercion-uid v c top-level?))

  ;; coercion composition
  ;; cf. make-compose-coercions in interpret-casts/coercions
  (: compose-coercions/id/fvs Compose-Coercions-Type)
  (define (compose-coercions/id/fvs c1 c2)
    ;; We pass ZERO-EXPR as 3rd/4th arguments. They are dummy, will not
    ;; be used and should be read as NULL pointer.
    (apply-code compose-coercions-uid c1 c2 ZERO-EXPR ZERO-EXPR))

  ;; Remove applications of identity coercions
  (define (apply-coercion-opt [exp : CoC3-Expr] [cont : CoC3-Expr]) : CoC3-Expr
    ;; If cont K is identity coercion...
    (if (equal? cont ID-EXPR)
        exp
        (apply-coercion exp cont)))


  ;; Translate bindings:
  (: trans-bnd* (CoC3-Bnd* CoC3-Expr -> CoC3-Bnd*))
  (define (trans-bnd* bnd* cont)
    (map (lambda ([b : CoC3-Bnd])
           (cons (car b)
                 (trans-exp (cdr b) cont)))
         bnd*))

  ;; Translate values
  ;; Input ----- V (value)
  ;; Output ---- Psi(V)
  (: trans-val (CoC3-Expr -> CoC3-Expr))
  (define (trans-val val)
    (match val
      [(Var _) val]
      [(Quote _) val]  ;; Literal
      [(Lambda param* (Castable uid body))
       ;; next uid for kappa
       (let ([kappa (next-uid! "kappa")])
         (Lambda (cons kappa param*)
                 (Castable uid (trans-exp body (Var kappa)))))]
      [else (error 'unexpected-val)]))


  ;; Translate expressions:
  ;; Input1 ---- M (expression)
  ;; Input2 ---- K (continuation coercion)
  ;; Output ---- M:K (colon trans result)
  (: trans-exp (CoC3-Expr CoC3-Expr -> CoC3-Expr))
  (define (trans-exp exp cont)
    (match exp
      ;; uncoerced values:
      [(or (Var _)
           (Quote _)
           (Lambda _ (Castable _ _)))
       (apply-coercion-opt (trans-val exp) cont)]

      ;; App:
      [(App-Fn-or-Proxy cast compose e0 e*)
       (App-Fn-or-Proxy
        cast
        compose
        (trans-exp e0 ID-EXPR)
        (cons cont
              (map (lambda (e) (trans-exp e ID-EXPR)) e*)))]

      [(Op operator e*)
       (apply-coercion-opt
        (Op operator
            (map (lambda (e) (trans-exp e ID-EXPR)) e*))
        cont)]

      [(Letrec bnd* body)
       (Letrec (trans-bnd* bnd* ID-EXPR)
               (trans-exp body cont))]
      [(Let bnd* body)
       (Let (trans-bnd* bnd* ID-EXPR)
            (trans-exp body cont))]

      ;; observe whether e1 has the given type
      [(Observe e1 ty) (Observe (trans-exp e1 ID-EXPR) ty)]

      ;; Control-Flow-Forms:
      [(If e1 e2 e3)
       (If (trans-exp e1 ID-EXPR)
           (trans-exp e2 cont)
           (trans-exp e3 cont))]
      [(Switch e0 e1 e2)
       (Switch (trans-exp e0 cont)
               (for/list [(e e1)]
                 (cons (car e) (trans-exp (cdr e) cont)))
               (trans-exp e2 cont))]
      [(Begin e* e1)
       (Begin (map (lambda (e) (trans-exp e ID-EXPR)) e*)
              (trans-exp e1 cont))]
      [(Repeat var start end acc ini body)
       (apply-coercion-opt
        (Repeat var
                (trans-exp start ID-EXPR)
                (trans-exp end ID-EXPR)
                acc
                (trans-exp ini ID-EXPR)
                (trans-exp body ID-EXPR))
        cont)]
      [(No-Op)
       (No-Op)]

      [(Assign name e) ;; name --> variable name string?
       (apply-coercion-opt
        (Assign name
                (trans-exp e ID-EXPR))
        cont)]
      [(Global name)
       (Global name)]

      [(App-Code e0 e*)
       (match e0
         [(Code-Label (Uid "apply-coercion" u))
          ;; e0 ---> (Code-Label (Uid "apply-coercion" 160))
          ;; e* ---> 1st: target, 2nd: coercion expr
          (let ([e1 (car e*)]    ;; target exp
                [c1 (cadr e*)])  ;; coercion
            (if (equal? cont ID-EXPR)
                ;; case: cont is identity coercion
                (trans-exp e1 c1)
                ;; otherwise:
                (trans-exp e1 (compose-coercions/id/fvs c1 cont))))]
         [(Code-Label (Uid _ _))
          (apply-coercion-opt
           (App-Code e0 (for/list ([v e*])
                          (trans-exp v ID-EXPR)))
           cont)]
         [_ (error "e0 is not Code-Label" (pretty-format exp))]
         )]
      [(Dyn-Immediate-Value v)
       (apply-coercion-opt
        (Dyn-Immediate-Value (trans-exp v ID-EXPR))
        cont)]
      [(Dyn-Immediate-Tag=Huh v type)
       (apply-coercion-opt
        (Dyn-Immediate-Tag=Huh (trans-exp v ID-EXPR) type)
        cont)]
      [(Dyn-Box-Value v)
       (apply-coercion-opt
        (Dyn-Box-Value (trans-exp v ID-EXPR))
        cont)]
      [(Dyn-Box-Type v)
       (apply-coercion-opt
        (Dyn-Box-Type (trans-exp v ID-EXPR))
        cont)]

      ;; Could reify this type into an injection coercion
      ;; but this pass is going to be moved.
      [(Dyn-Object v t)
       (apply-coercion-opt
        (Dyn-Object (trans-exp v ID-EXPR) t)
        cont)]
      
      [(Type-Mu-Huh v)
       (apply-coercion-opt
        (Type-Mu-Huh (trans-exp v ID-EXPR))
        cont)]
      [(Type-Mu-Body v)
       (apply-coercion-opt
        (Type-Mu-Body (trans-exp v ID-EXPR))
        cont)]

      [(Type-Fn-Huh v)
       (apply-coercion-opt
        (Type-Fn-Huh (trans-exp v ID-EXPR))
        cont)]
      [(Type-Fn-arity v)
       (apply-coercion-opt
        (Type-Fn-arity (trans-exp v ID-EXPR))
        cont)]
      [(Type-Fn-arg v1 v2)
       (apply-coercion-opt
        (Type-Fn-arg (trans-exp v1 ID-EXPR)
                     (trans-exp v2 ID-EXPR))
        cont)]
      [(Type-Fn-return v)
       (apply-coercion-opt
        (Type-Fn-return (trans-exp v ID-EXPR))
        cont)]

      [(Blame v)
       (apply-coercion-opt
        (Blame (trans-exp v ID-EXPR))
        cont)]

      ;; box (references):
      [(Unguarded-Box v)
       (apply-coercion-opt
        (Unguarded-Box (trans-exp v ID-EXPR))
        cont)]
      [(Unguarded-Box-Ref box)
       (apply-coercion-opt
        (Unguarded-Box-Ref (trans-exp box ID-EXPR))
        cont)]
      [(Unguarded-Box-Set! box v)
       (apply-coercion-opt
        (Unguarded-Box-Set! (trans-exp box ID-EXPR) (trans-exp v ID-EXPR))
        cont)]

      ;; monobox:
      [(Mbox v ty)
       (apply-coercion-opt
        (Mbox (trans-exp v ID-EXPR) ty)
        cont)]
      [(Mbox-val-ref e)
       (apply-coercion-opt
        (Mbox-val-ref (trans-exp e ID-EXPR))
        cont)]
      [(Mbox-val-set! e1 e2)
       (apply-coercion-opt
        (Mbox-val-set! (trans-exp e1 ID-EXPR) (trans-exp e2 ID-EXPR))
        cont)]
      [(Mbox-rtti-ref e)
       (apply-coercion-opt
        (Mbox-rtti-ref (trans-exp e ID-EXPR))
        cont)]

      ;; vectors:
      [(Unguarded-Vect e1 e2)
       (apply-coercion-opt
        (Unguarded-Vect (trans-exp e1 ID-EXPR) (trans-exp e2 ID-EXPR))
        cont)]
      [(Unguarded-Vect-Ref e1 e2)
       (apply-coercion-opt
        (Unguarded-Vect-Ref (trans-exp e1 ID-EXPR) (trans-exp e2 ID-EXPR))
        cont)]
      [(Unguarded-Vect-Set! v1 v2 v3)
       (apply-coercion-opt
        (Unguarded-Vect-Set! (trans-exp v1 ID-EXPR) (trans-exp v2 ID-EXPR) (trans-exp v3 ID-EXPR))
        cont)]
      [(Unguarded-Vect-length v)
       (apply-coercion-opt
        (Unguarded-Vect-length (trans-exp v ID-EXPR))
        cont)]

      ;; monovector:
      [(Mvector e1 e2 ty)
       (apply-coercion-opt
        (Mvector (trans-exp e1 ID-EXPR) (trans-exp e2 ID-EXPR) ty)
        cont)]
      [(Mvector-val-ref e1 e2 guarded?)
       (apply-coercion-opt
        (Mvector-val-ref (trans-exp e1 ID-EXPR) (trans-exp e2 ID-EXPR) guarded?)
        cont)]
      [(Mvector-rtti-ref e)
       (apply-coercion-opt
        (Mvector-rtti-ref (trans-exp e ID-EXPR))
        cont)]
      [(Mvector-val-set! e1 e2 e3 guarded?)
       (apply-coercion-opt
        (Mvector-val-set! (trans-exp e1 ID-EXPR) (trans-exp e2 ID-EXPR) (trans-exp e3 ID-EXPR) guarded?)
        cont)]
      [(Mvector-length e)
       (apply-coercion-opt
        (Mvector-length (trans-exp e ID-EXPR))
        cont)]

      ;; proxied reference coercions:
      [(Ref-Coercion-Read e)
       (apply-coercion-opt
        (Ref-Coercion-Read (trans-exp e ID-EXPR))
        cont)]
      [(Ref-Coercion-Write e)
       (apply-coercion-opt
        (Ref-Coercion-Write (trans-exp e ID-EXPR))
        cont)]

      ;; tuples:
      [(Create-tuple e*)
       (apply-coercion-opt
        (Create-tuple
         (for/list ([a e*])
           (trans-exp a ID-EXPR)))
        cont)]
      [(Tuple-proj e1 e2)
       (apply-coercion-opt
        (Tuple-proj (trans-exp e1 ID-EXPR) (trans-exp e2 ID-EXPR))
        cont)]

      ;; proxies:
      [(Guarded-Proxy-Huh v)
       (apply-coercion-opt
        (Guarded-Proxy-Huh (trans-exp v ID-EXPR))
        cont)]
      [(Guarded-Proxy-Ref v)
       (apply-coercion-opt
        (Guarded-Proxy-Ref (trans-exp v ID-EXPR))
        cont)]
      [(Guarded-Proxy-Coercion v)
       (apply-coercion-opt
        (Guarded-Proxy-Coercion (trans-exp v ID-EXPR))
        cont)]
      ;; This case handles a bunch of forms that make no sense to
      ;; traverse because this pass is in the wrong place.
      [other other]))

  ;; Translate toplevel expressions with identity continuation coercion:
  (lambda (exp) (trans-exp exp ID-EXPR)))
