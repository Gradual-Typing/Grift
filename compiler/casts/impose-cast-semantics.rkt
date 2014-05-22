#lang racket
#|------------------------------------------------------------------------------+
|Pass: compiler/casts/impose-cast-semantics                                     |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
|Discription:                                                                   |
|                                                                               |
+-------------------------------------------------------------------------------+
|Input Grammar "Explicitly-Typed-Core-Forms" found in Schml/languages/core.rkt  |
|Prog   = (Prog File-Name {TExpr} Type)                                         |
|TExpr  = {Expr}                                                                |
|Expr   = (Lambda {uvar}* {Type} {Expr} {Src})                                  |
|       | (Var {Uvar} {Type} {Src})                                             |
|       | (App {Expr} {Expr}* {Type} {Src})                                     |
|       | (Op Prim {Expr}* {Type} {Src})                                        |
|       | (Cast {Expr} {Type} {Type} {Blame})                                   |
|       | (If {Expr} {Expr} {Expr} {Type} {Src})                                |
|       | (Let {BndSet} {Expr} {Type} {Src})                                    |
|       | (Const {Imdt} {Type})                                                 |
|BndSet = ({Uvar} {Expr})                                                       |
|Fml    = {Uvar} | ({Uvar} {Type})                                              |
|Src    = (Src File Line Col Span)                                              |
|Blame  = {Src} | {String}                                                      |
|Uvar   = A Symbol with the format 'original$uniqueness                         |
|Imdt   = Fixnums and Booleans                                                  |
|Prim   = op:fix:* | op:fix:+ | op:fix:- | op:fix:and | op:fix:or | op:fix:>>   |
|       | op:fix:<< | relop:fix:< | relop:fix:<= | relop:fix:=  | relop:fix:>=  |
|       | relop:fix:>                                                           |
|Type   = Fix | Bool | Dyn | ({Type}* -> {Type})                                |
+-------------------------------------------------------------------------------+
|Output Grammar                                                                 |
|                                                                               |
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require Schml/framework/build-compiler
         Schml/framework/helpers
         Schml/framework/errors)
;; The constuctors of the core language
(require Schml/language/core)
;; Only the pass is provided by this module
(provide impose-cast-semantics)

(define-pass (impose-cast-semantics prgm comp-config)
  (explicit-core? -> (lambda (ast) #t))

  (define (make-cast t1 t2 src exp)
    exp)
  
  (define (ics-expr exp)
    (match exp
      [(Lambda src t-lambda fmls (app ics-expr (and body (app Expr-type t-body))))
       (Lambda src t-lambda fmls (make-cast (Function-to t-lambda) t-body src body))]
      [(Let src t-let (list (Fml i t (app ics-expr
                                      (and e (app Expr-type t)))) ...)
            body)
       (let-values ([(body t-body) (tyck-expr body (env-extend* env id* ty*))])
         (values (Let src t-body (map Bnd id* ty* rhs*) body) t-body))]
      ;; Variables are typed at the type associated with it in environment
      ;; If it isn't found then there must be a mistake in the
      ;; compiler because all unbound variable should be caught during parsing.
      [(Var src _ id)
       (let ((ty (env-lookup env id (var-not-found-error src id 'tyck-expr-var))))
         (values (Var src ty id) ty))]
      [(Cast src t-cast (app recur exp t-exp) label)
       (let ((ty-casted-exp (cast-type-rule t-exp t-cast label)))
         (values (Cast src ty-casted-exp exp label)))]
      [(If src _ (app recur tst t-tst) (app recur csq t-csq) (app recur alt t-alt))
       (let ((t-if (if-type-rule t-tst t-csq t-alt src)))
         (values (If src t-if tst csq alt) t-if))]
      [(Const src _ (and const (app const-type-rule t-const)))
       (values (Const src t-const const) t-const)]
      [(App src _ (app recur exp t-exp) (list (app recur exp* t-exp*) ...))
       (let ((t-app (application-type-rule t-exp t-exp* src)))
         (values (App src t-app exp exp*) t-app))]
      [(and (Prim _ _) p-exp) (tyck-prim p-exp env)]
      [e (match-pass-error pass 'tyck-expr e)]))

  ;; Type checks a primitive expression with the given environment 
  (define (tyck-prim prim-exp env)
    (define (tyck-arg e) (tyck-expr e env))
    ;; While it looks like more could be abstacted out these
    ;; primitives are fundementally different there is nothing that
    ;; says a new class of primitives that are ternary could not be
    ;; added it is unfortunate that there is not abstract way to
    ;; extract the constructor for these types.
    (match prim-exp
      [(Prim:Bin:Int src _ (app tyck-arg fst t-fst) (app tyck-arg snd t-snd))
       (let ((ty (application-type-rule Binop-Int-Type `(,t-fst ,t-snd) src)))
         (values
          ((match prim-exp
             [(struct Prim:Bin:Int:* _) Prim:Bin:Int:*]
             [(struct Prim:Bin:Int:+ _) Prim:Bin:Int:+]
             [(struct Prim:Bin:Int:- _) Prim:Bin:Int:-]
             [(struct Prim:Bin:Int:and _)  Prim:Bin:Int:and]
             [(struct Prim:Bin:Int:or _)  Prim:Bin:Int:or]
             [(struct Prim:Bin:Int:>> _)  Prim:Bin:Int:>>]
             [(struct Prim:Bin:Int:<< _)  Prim:Bin:Int:<<])
           src ty fst snd)
          ty))]
      [(Prim:Rel:Int src _ (app tyck-arg fst t-fst) (app tyck-arg snd t-snd))
       (let ((ty (application-type-rule Binop-Int-Type `(,t-fst ,t-snd) src)))
         (values
          ((match prim-exp
             [(struct Prim:Rel:Int:< _) Prim:Rel:Int:<]
             [(struct Prim:Rel:Int:> _) Prim:Rel:Int:>]
             [(struct Prim:Rel:Int:= _) Prim:Rel:Int:=]
             [(struct Prim:Rel:Int:<= _) Prim:Rel:Int:<=]
             [(struct Prim:Rel:Int:>= _) Prim:Rel:Int:>=])
           src ty fst snd)
          ty))]
      [otherwise (match-pass-error pass 'tyck-prim prim-exp)]))

  ;; Type checks the rhs to be consistent with type annotation if
  ;; provided the resulting type is the type of the annotation.
  (define (tyck-binding src env)
    (lambda (bnd)
      (let-values ([(exp t-exp) (tyck-expr (Bnd-expr bnd) env)])
        (values (Bnd-ident bnd) t-exp exp))))

  ;; This is the body of the type-check
  (match prgm
    [(Prog n e*) (Prog n (for/list ([e (in-list e*)])
                           (let-values ([(e t) (tyck-expr e (empty-env))])
                             e)))]
    [otherwise (match-pass-error pass pass prgm)]))




