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
(require Schml/language/shared
         (prefix-in tc: Schml/language/typed-core)
         (prefix-in sc: Schml/language/sourceless-core))
;; Only the pass is provided by this module
(provide insert-implicit-casts)

(define-pass (insert-implicit-casts prgm comp-config)
  (define (mk-cast l-th e t1 t2)
    (if (equal? t1 t2) e (sc:Cast t2 e t1 (l-th))))
  (define (mk-label pos src)
    (string->label
     (format "Implicit cast in ~a on expression at ~a"
             pos (srcloc->string src))))
  (define (mk-app-label exp arg)
      (string->label
       (format "Implicit cast of argument at ~a in expression ~a"
               (srcloc->string arg) (srcloc->string exp))))
  (define (iic-expr exp)
    (match exp
      [(tc:Lambda src (and (Function from-ty* to-ty) ty)
                  fmls (and (tc:Expr b-src b-ty) (app iic-expr body)))
       ;;This seems to be unesissary now but the letrecs will need it
       (sc:Lambda ty fmls
                  (mk-cast (th (mk-label "lambda" b-src)) body b-ty to-ty))]
      [(tc:Let src ty (list (app iic-binding bnd*) ...) (app iic-expr body))
       (sc:Let ty bnd* body)]
      [(tc:Var src ty id) (sc:Var ty id)]
      [(tc:Cast src ty (and (tc:Expr _ ty-exp) (app iic-expr exp)) _ label)
       (mk-cast (th (or label (srcloc->string src))) exp ty-exp ty)]
      [(tc:If src ty-if
           (and (tc:Expr tst-src tst-ty) (app iic-expr tst))
           (and (tc:Expr csq-src csq-ty) (app iic-expr csq))
           (and (tc:Expr alt-src alt-ty) (app iic-expr alt)))
       (sc:If ty-if
              (mk-cast (th (mk-label 'if tst-src)) tst tst-ty Bool-Type)
              (mk-cast (th (mk-label 'if csq-src)) csq csq-ty ty-if)
              (mk-cast (th (mk-label 'if alt-src)) alt csq-ty ty-if))]
      [(tc:Const src ty k) (sc:Const ty k)]
      [(tc:App src ty-app (and (tc:Expr src-exp ty-exp) (app iic-expr exp))
               (list (and (tc:Expr src-exp* ty-exp*) (app iic-expr exp*)) ...))
       (match ty-exp
         [(Dyn) (sc:App ty-app
                        (mk-cast (th (mk-label 'application src-exp))
                                 exp Dyn-Type (Function ty-exp* Dyn-Type))
                        exp*)]
         [(Function (list from ...) to)
          (sc:App
           ty-app exp
           (map (lambda (s e te tc)
                  (mk-cast (th (mk-app-label src s)) e te tc))
                src-exp* exp* ty-exp* from))]
         [other (match-pass-error pass 'application other)])]
      [(tc:Prim s t pexp) (iic-prim s t pexp)]
      [e (match-pass-error pass 'tyck-expr e)]))

  ;; Type checks a primitive expression with the given environment 
  (define (iic-prim src typ pexp)
    (match pexp
      [(Op:IntxInt (and (tc:Expr src-fst ty-fst) (app iic-expr fst))
                   (and (tc:Expr src-snd ty-snd) (app iic-expr snd)))
       (sc:Prim
        typ
        (mk-op:int-int
         pexp
         (mk-cast (th (mk-app-label src src-fst)) fst ty-fst Int-Type)
         (mk-cast (th (mk-app-label src src-snd)) snd ty-snd Int-Type)))]
      [(Rel:IntxInt (and (tc:Expr src-fst ty-fst) (app iic-expr fst))
                    (and (tc:Expr src-snd ty-snd) (app iic-expr snd)))
       (sc:Prim
        typ
        (mk-rel:int-int
         pexp
         (mk-cast (th (mk-app-label src src-fst)) fst ty-fst Int-Type)
         (mk-cast (th (mk-app-label src src-snd)) snd ty-snd Int-Type)))]
      [otherwise (match-pass-error pass 'iic-prim otherwise)]))
  
  (define/match (iic-binding bnd)
    [((Bnd:Ty id (and (app iic-expr rhs) (tc:Expr src-rhs ty-rhs)) ty))
      (Bnd:Ty id (mk-cast (th (mk-label "let" src-rhs)) rhs ty-rhs ty) ty)]
    [(other) (match-pass-error pass 'iic-binding other)])
  
  ;; This is the body of the Insert Implicit Casts
  (match prgm
    [(tc:Prog n c t e) (sc:Prog n c t (iic-expr e))]
    [otherwise (match-pass-error pass 'body prgm)]))





 
                



