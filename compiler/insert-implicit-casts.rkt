#lang racket
#|------------------------------------------------------------------------------+
|Pass: compiler/casts/insert-implicit-casts                                     |
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
(require Schml/language/core)
;; Only the pass is provided by this module
(provide insert-implicit-casts)

(define-pass (insert-implicit-casts prgm comp-config)
  (explicit-core? -> explicit-core?)
  (define (mk-cast s l e t1 t2)
    (if (equal? t1 t2) e (Cast s t2 e t1 l)))
  (define (mk-label pos src)
    (format "Implicit cast in ~a on expression at ~a"
            pos (srcloc->string src)))
  (define (mk-app-label exp arg)
      (format "Implicit cast of argument at ~a in expression ~a"
              (srcloc->string arg) (srcloc->string exp)))
  (define (iic-expr exp)
    (match exp
      [(Lambda src (and (Function from-ty* to-ty) ty)
               fmls (and (Expr b-src b-ty) (app iic-expr body)))
       ;;This seems to be unesissary now but the letrecs will need it
       (Lambda src ty fmls
               (mk-cast 'implicit (mk-label "lambda" b-src) body b-ty to-ty))]
      [(Let src ty (list (app iic-binding bnd*) ...) (app iic-expr body))
       (Let src ty bnd* body)]
      [(Var src ty id) exp]
      [(Cast src ty (and (Expr _ ty-exp) (app iic-expr exp)) _ label)
       (mk-cast src label exp ty-exp ty)]
      [(If src ty-if
           (and (Expr tst-src tst-ty) (app iic-expr tst))
           (and (Expr csq-src csq-ty) (app iic-expr csq))
           (and (Expr alt-src alt-ty) (app iic-expr alt)))
       (If src ty-if
           (mk-cast 'implicit (mk-label 'if tst-src) tst tst-ty Bool-Type)
           (mk-cast 'implicit (mk-label 'if csq-src) csq csq-ty ty-if)
           (mk-cast 'implicit (mk-label 'if alt-src) alt csq-ty ty-if))]
      [(Const src ty k) exp]
      [(App src ty-app (and (Expr src-exp ty-exp) (app iic-expr exp))
            (list (and (Expr src-exp* ty-exp*) (app iic-expr exp*)) ...))
       (match ty-exp
         [(Dyn)
          (App src ty-app
               (mk-cast 'implicit
                        (mk-label 'application src-exp)
                        exp Dyn-Type (Function ty-exp* Dyn-Type))
               exp*)]
         [(Function (list from ...) to)
          (App src ty-app exp
               (map (lambda (s e te tc)
                      (mk-cast 'implicit (mk-app-label src s) e te tc))
                    src-exp* exp* ty-exp* from))]
         [other (match-pass-error pass 'application other)])]
      [(Prim _ _) (iic-prim exp)]
      [e (match-pass-error pass 'tyck-expr e)]))

  ;; Type checks a primitive expression with the given environment 
  (define (iic-prim prim-exp)
    (match prim-exp
      [(Prim:Bin:Int src ty (and (Expr src-fst ty-fst) (app iic-expr fst))
                            (and (Expr src-snd ty-snd) (app iic-expr snd)))
       ((mk-struct prim-exp) src ty
        (mk-cast 'implicit (mk-app-label src src-fst) fst ty-fst Int-Type)
        (mk-cast 'implicit (mk-app-label src src-snd) snd ty-snd Int-Type))]
      [(Prim:Rel:Int src ty (and (Expr src-fst ty-fst) (app iic-expr fst))
                            (and (Expr src-snd ty-snd) (app iic-expr snd)))
       ((mk-struct prim-exp) src ty
        (mk-cast 'implicit (mk-app-label src src-fst) fst ty-fst Int-Type)
        (mk-cast 'implicit (mk-app-label src src-snd) snd ty-snd Int-Type))]
      [otherwise (match-pass-error pass 'iic-prim otherwise)]))
  
  (define/match (iic-binding bnd)
    [((Bnd id ty (app iic-expr (and rhs (Expr src-rhs ty-rhs)))))
     (Bnd id ty (mk-cast 'implicit (mk-label "let" src-rhs) rhs ty-rhs ty))]
    [(other) (match-pass-error pass 'iic-binding other)])
  
  ;; This is the body of the Insert Implicit Casts
  (match prgm
    [(Prog n e*) (Prog n (iic-expr e*))]
    [otherwise (match-pass-error pass 'body prgm)]))





 
                



