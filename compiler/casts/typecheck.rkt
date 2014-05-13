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
|Input Grammar "Implicitly-Typed-Core-Forms"                                            |
|Prog   = (Prog File-Name {TExpr})                                              |
|TExpr  = {Expr}                                                                |
|Expr   = (Lambda {Fml}* {Expr} Src)                                                 |
|       | (Var {Uvar} Src)                                                      |
|       | (App {Expr} {Expr}* Src)                                              |
|       | (Op Prim {Expr}* Src)                                                 |
|       | (Cast {Expr} {Type} Src)                                          |
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
|Input Grammar "Explicitly-Typed-Core-Forms"                                            |
|Prog   = (Prog File-Name {TExpr} Type)                                         |
|TExpr  = {Expr}                                                                |
|Expr   = (Lambda {uvar}* {Type} {Expr} {Src})                                       |
|       | (Var {Uvar} {Type} {Src})                                             |
|       | (App {Expr} {Expr}* {Type} {Src})                                     |
|       | (Op Prim {Expr}* {Type} {Src})                                        |
|       | (Cast {Expr} {Type} {Type} {Blame})                                   |
|       | (If {Expr} {Expr} {Expr} {Type} {Src})                                |
|       | (Let {BndSet} {Expr} {Type} {Src})                                    |
|       | (Const {Imdt} {Type})                                           |
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
;; The constuctors of the ILs qualified to prevent name conflicts
(require (prefix-in ic: Schml/language/implicit-core)
         (prefix-in ec: Schml/language/explicit-core)
         (prefix-in ty: Schml/language/types)
         (prefix-in sh: Schml/language/shared))
;; Only the pass is provided by this module
(provide typecheck)

(define-pass (typecheck prgm comp-config)
  (ic:implicit-core? -> ec:explicit-core?)
  ;; Common variable names that may be too short
  ;; s -> Source-location
  ;; t -> Expression-type
  ;; i -> identifier or index

  ;; Cata tagged function destructure and restructure the ast passing
  ;; along all information that is necissary.
  (define (cata-top-level decl env)
    ((cata-expr env) decl))
  (define (cata-expr env)
    (define (cata-fml* fml*)
      (define (help fml)
        (match fml
          [(ic:Typed-Fml s t i) (values (ec:Fml s t i) i t)]
          [(ic:Untyped-Fml s i) (values (ec:Fml s (ty:Dyn) i) i (ty:Dyn))]
          [o (match-pass-error pass 'cata-fml fml)]))
      (vector-map/values help fml* 3))
    (define (cata-let-binding* bs*)
      (define (help bs)
        (match bs
          [(ic:Untyped-Binding s i (app cata-expr/env e t))
           (values (ec:Binding s t i e) i t)]
          [(ic:Typed-Binding s i t1 (app cata-expr/env e t2))
           (let ((t (type-binding t1 t2 s)))
             (values (ec:Binding s t i e) i t))]
          [o (match-pass-error pass 'cata-let-binding bs)]))
      (vector-map/values help bs* 3))
    (define (cata-expr*/env expr*)
      (map/values cata-expr/env expr* '() '()))
    (define (cata-expr/env expr)
      (match expr
        [(ic:Lambda (app cata-fml* fml* i* t*) exp src)
         (let-values (((exp t-exp) (cata-expr exp (ext-env* env i* t*))))
           (let ([t (ty:Function t* t-exp)])
             (values (ec:Lambda fml* t exp src) t)))]
        [(ic:Let s (app cata-let-binding* b* i* t*) exp)
         (let-values (((exp t) (cata-expr exp (ext-env* env i* t*))))
           (values (ec:Let s t b* exp) t))]
        [(ic:App s (app cata-expr/env exp t-exp)
                   (app cata-expr*/env exp* t-exp*))
         (let ((t (type-app t-exp t-exp* s)))
           (values (ec:App s t exp exp*) t))]
        [(ic:Op s prim (app cata-expr*/env exp* t-exp*))
         (let ((t (type-app (type-prim prim) t-exp* s)))
           (values (ec:Op s t prim exp*) t))]
        [(ic:Var s ident)
         (let ((t (env-ref env ident)))
           (values (ec:Var s t ident) t))]
        [(ic:Cast (app cata-expr/env exp exp-t) cast-t src)
         (let ((t (type-cast exp-t cast-t)))
           (values (ec:Cast exp exp-t t src) t))]
        [(ic:If (app cata-expr/env tst tst-t)
                (app cata-expr/env csq csq-t)
                (app cata-expr/env alt alt-t)
                src)
         (let ((t (type-if tst-t csq-t alt-t src)))
           (values (ec:If tst csq alt t src) t))]
        [(ic:Const (app type-const k t) src) (values (ec:Const k t src) t)]
        [e (match-pass-error pass 'cata-expr/env e)]))
    cata-expr/env)

  ;; Type tagged functions perform the type rules for
  ;; the basic components of the ast
  (define (type-binding decl infr src)
    (if (ty:consistent? decl infr)
        decl
        (type-error 'binding decl infr src)))
  (define (type-prim p src)
    (match p
      [(sh:Prim:Rel:Int) ty:Relop-Int-Type]
      [(sh:Prim:Bin:Int) ty:Binop-Int-Type]
      [p (match-pass-error pass 'tyck-prim p)]))
  (define (type-app rator rands src app)
    (match rator
      [(ty:Function args result)
       (let ([al (vector-length args)]
             [rl (vector-length rands)])
         (unless (= al rl)
           (general-type-error 'application  "arrity mismatch" src))
         (for ([a args] [r rands])
              (unless (ty:consistent? a r)
                (general-type-error
                 'application "Arguments not consistent" src)))
         (values args result))]
      ;;TODO add a case for The dynamic type
      [o (general-type-error
          'application
          "Type of operator is not consistent with function" src)]))
  (define (type-cast exp cast src)
    (if (ty:consistent? exp cast) cast (type-error 'cast exp cast src)))    
  (define (type-if tst csq alt src)
    (cond
      [(not (ty:consistent? tst ty:Bool-Type))
       (type-error 'if tst ty:Bool-Type src)]
      [(ty:consistent? csq alt)
       (ty:meet csq alt (lambda (msg) (pass-error pass 'type-if msg csq alt)))]
      [else (pass-error pass 'type-if "Unexpected branch reached" csq alt)]))
  (define (type-const k)
    (cond
      [(boolean? k) (values k ty:Bool-Type)]
      [(fixnum? k)  (values k ty:Int-Type)]
      [else (match-pass-error pass 'type-const k)]))
  (define (type-error check exp type src)
    (let* ((fmt "~a:Ill typed ~a: ~a is not consistent with type ~a")
           (msg (format fmt src check exp type)))
      (error pass msg)))
  (define (general-type-error check msg src)
    (error pass (format "~a:~a:~a" check src exp)))
  ;; The following are helpers for managing the type enviroment
  (define (ext-env* env i* t*)
    (if (and (vector? i*) (vector? t*) (hash? env))
        (if (= (vector-length i*) (vector-length t*))
            (for/fold ([env env]) ([ident i*] [type t*])
              (hash-set env ident type))
            (pass-error pass 'ext-env* "argument lengths differ" i* t*))
        (pass-error pass 'ext-env* "Ill typed arguments" env i* t*)))
  (define (env-ref env ident)
    (define (err) (pass-error pass 'type-var "Unbound identifier" ident env))
    (hash-ref env ident err))
  (define base-env (hasheq))
  (match prgm
    [(ic:Prog f e) (cata-top-level e base-env)]))





