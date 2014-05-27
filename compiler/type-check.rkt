#lang racket
#|------------------------------------------------------------------------------+
|Pass: compiler/casts/typecheck                                                  |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
|Discription: This is a first attempt at typechecking for the GTLC core         |
|language.  It is currently limited to just the grammar noted below but with the|
|with the addition of a unification algorithm is could be generalize to mutually|
|recursive functions. There is a major way in which this algorithm is different |
|from that of the STLC. The difference is that instead of checking for type     |
|equality the algorithm is checking for type consitency.                        |
|                                                                               |
|Essence of the consistency relationship:                                       |
|Two type are consistent any of the following relationships are true.           |
|   1: One of them is dyn.                                                      |
|   2: They are structurally equal.                                             |
|   3: They both share the same super-structure                                 |
|      and their sub-structures are consistent.                                 |
|                                                                               |
+-------------------------------------------------------------------------------+
|Input Grammar "Implicitly-Typed-Core-Forms" found in Schml/languages/core.rkt  |
|Prog   = (Prog File-Name {TExpr})                                              |
|TExpr  = {Expr}                                                                |
|Expr   = (Lambda {Fml}* {Expr} Src)                                            |
|       | (Var {Uvar} Src)                                                      |
|       | (App {Expr} {Expr}* Src)                                              |
|       | (Op Prim {Expr}* Src)                                                 |
|       | (Cast {Expr} {Type} Src)                                              |
|       | (If {Expr} {Expr} {Expr} Src)                                         |
|       | (Let {BndSet} {Expr} Src)                                             |
|       | (Const {Imdt})                                                        |
|BndSet = ({Fml} {Expr})*                                                       |
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
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require Schml/framework/build-compiler
         Schml/framework/helpers
         Schml/framework/errors)
;; The constuctors of the core language
(require Schml/language/core
         Schml/language/types
         Schml/language/shared)
;; Only the pass is provided by this module
(provide type-check)

(define-pass (type-check prgm comp-config)
  (implicit-core? -> explicit-core?)
;;; Procedures that are used to throw errors
  ;; The error that occurs when a variable is not found. It is an internal
  ;; error because it is a syntax error to have an unbound variable.
  (define (var-not-found-error src id site)
    (th (pass-error pass
                    "Var ~a:~a was not in env during lookup at ~a."
                    (srcloc->string src) id site)))

;;; The type rules for core forms that have interesting type rules
  (define (lambda-type-rule src ty* t-body t-ann)
    (cond
      [(not t-ann) (Function ty* t-body)]
      [(consistent? t-body t-ann) (Function ty* t-ann)]
      [else  (lambda/inconsistent-types-error src t-body t-ann)]))
  
  ;; The type of a cast is the cast-type if the expression type and
  ;; the cast type are consistent.
  (define (cast-type-rule ty-exp ty-cast label)
    (when (not (consistent? ty-exp ty-cast))
        (cast/inconsistent-types-error label ty-exp ty-cast))
    ty-cast)

  ;; The type of an if is the meet of the consequence and alternative
  ;; types if the type of the branches are consistent and the test is
  ;; consistent with Bool.
  (define (if-type-rule t-tst t-csq t-alt src)
    (cond
      [(not (consistent? t-tst Bool-Type))
       (if/inconsistent-test-error src t-tst)]
      [(not (consistent? t-csq t-alt))
       (if/inconsistent-branches-error src t-csq t-alt)]
      [else (meet t-csq t-alt)]))

  ;; The type of literal constants are staticly known
  (define (const-type-rule c)
    (cond
      [(boolean? c) Bool-Type]
      [(integer? c) Int-Type]
      [else (cond-pass-error pass 'const-type-rule c)]))

  ;; The type of an application is the return type of the applied
  ;; procedure given that the arguments are consistent with the
  ;; arguments types of the proceedure.
  (define (application-type-rule t-rator t-rand* src)
    (match t-rator
      [(Function t-arg* return)
       (if (not (andmap/length= consistent? t-arg* t-rand*))
           (app-inconsistent-error src t-rator t-rand*)
           return)]
      [(Dyn) Dyn-Type]
      [otherwise (app-non-function-error src t-rator)]))

;;; Procedures that destructure and restructure the ast
  ;; tyck-expr : (Struct Expr) * Env -> (values (Struct Expr) Type)
  (define (tyck-expr exp env)
    (define (recur e) (tyck-expr e env))
    (match exp
      [(Lambda src t-ann (and fml* (list (Fml id* ty*) ...)) body)
       (let-values (((body t-body) (tyck-expr body (env-extend* env id* ty*))))
         (let ([ty-lambda (lambda-type-rule src ty* t-body t-ann)])
           (values (Lambda src ty-lambda fml* body) ty-lambda)))]
      ;; Let is unconditionally typed at the type of its body
      [(Let src _ (list (app (tyck-binding (Expr-src exp) env) id* ty* rhs*) ...) body)
       (let-values ([(body t-body) (tyck-expr body (env-extend* env id* ty*))])
         (values (Let src t-body (map Bnd id* ty* rhs*) body) t-body))]
      ;; Variables are typed at the type associated with it in environment
      ;; If it isn't found then there must be a mistake in the
      ;; compiler because all unbound variable should be caught during parsing.
      [(Var src _ id)
       (let ((ty (env-lookup env id (var-not-found-error src id 'tyck-expr-var))))
         (values (Var src ty id) ty))]
      [(Cast src t-cast (app recur exp t-exp) _ label)
       (let ((ty-casted (cast-type-rule t-exp t-cast label)))
         (values (Cast src ty-casted exp t-exp label) ty-casted))]
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
    
    (match prim-exp
      [(Prim:Bin:Int src _ (app tyck-arg fst t-fst) (app tyck-arg snd t-snd))
       (let ((ty (application-type-rule Binop-Int-Type `(,t-fst ,t-snd) src)))
         (values ((mk-struct prim-exp) src ty fst snd) ty))]
      [(Prim:Rel:Int src _ (app tyck-arg fst t-fst) (app tyck-arg snd t-snd))
       (let ((ty (application-type-rule Relop-Int-Type `(,t-fst ,t-snd) src)))
         (values ((mk-struct prim-exp) src ty fst snd) ty))]
      [otherwise (match-pass-error pass 'tyck-prim prim-exp)]))

  ;; Type checks the rhs to be consistent with type annotation if
  ;; provided the resulting type is the type of the annotation.
  (define (tyck-binding src env)
    (lambda (bnd)
      (let-values ([(exp t-exp) (tyck-expr (Bnd-expr bnd) env)])
        (values (Bnd-ident bnd) t-exp exp))))
  
  ;; This is the body of the type-check
  (match prgm
    [(Prog n e) (let-values (((e t) (tyck-expr e (empty-env))))
                  (Prog n e))]
    [otherwise (match-pass-error pass pass prgm)]))

