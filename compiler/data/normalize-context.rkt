#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: compiler/data/normalize-context                                          |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
| Description: This pass introduces the concept of keeping track
| of the context of an ast node.
+-------------------------------------------------------------------------------+
| Grammer:
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require schml/framework/build-compiler
         schml/framework/helpers
         schml/framework/errors
	 schml/compiler/language)

;; Only the pass is provided by this module
(provide normalize-context)

(: normalize-context (-> Data0-Lang Config Data1-Lang))
(define (normalize-context prgm comp-config)
  (match-let ([(Prog (list name count type) (Labels bnd-code* exp)) prgm])
    (let ([bnd-code* : D1-Bnd-Code* (nc-bnd-code* bnd-code*)]
          [tail : D1-Tail (nc-tail exp)])
      (Prog (list name count type) (Labels bnd-code* tail)))))

(: nc-tail (-> D0-Expr D1-Tail))
(define (nc-tail exp)
  (match exp
    [(Let bnd* exp) (Let (nc-bnd* bnd*) (nc-tail exp))]
    [(If t c a) (If (nc-pred t) (nc-tail c) (nc-tail a))]
    [(Begin stm* exp) (Begin (nc-stmt* stm*) (nc-tail exp))]
    [other (Return (nc-expr other))]))

(: nc-expr (-> D0-Expr D1-Expr))
(define (nc-expr exp)
  (match exp
    [(Let bnd* exp) (Let (nc-bnd* bnd*) (nc-expr exp))]
    [(If t c a) (If (nc-pred t) (nc-expr c) (nc-expr a))]
    [(Begin stm* exp) (Begin (nc-stmt* stm*) (nc-expr exp))]
    [(App exp exp*) (App (nc-expr exp) (nc-expr* exp*))]
    [(Op p exp*) (nc-expr-op p (nc-expr* exp*))]
    [(Halt) (Halt)]
    [(Var i) (Var i)]
    [(Code-Label i) (Code-Label i)]
    [(Quote k) (Quote k)]))

(: nc-expr-op (-> UIL-Prim D1-Expr* D1-Expr))
(define (nc-expr-op p exp*)
  (cond
   [(IntxInt->Bool-Prim? p)
    (match exp*
      [(list a b) (If (Relop p a b) (Quote TRUE-IMDT) (Quote FALSE-IMDT))]
      [otherwise (error 'nc-expr-op "Unmatched ~a" exp)])]
   [else (Op p exp*)]))

(: nc-pred (-> D0-Expr D1-Pred))
(define (nc-pred exp)
  (match exp
    [(Let bnd* exp) (Let (nc-bnd* bnd*) (nc-pred exp))]
    [(If t c a) (If (nc-pred t) (nc-pred c) (nc-pred a))]
    [(Begin stm* exp) (Begin (nc-stmt* stm*) (nc-pred exp))]
    [(Op p exp*)
     (let ([exp* (nc-expr* exp*)])
       (if (IntxInt->Bool-Prim? p)
           (match exp*
             [(list a b) (Relop p a b)] 
             [other (error 'nc-pred-op)])
           (Relop '= (Quote TRUE-IMDT) (nc-expr-op p exp*))))]
    [other (Relop '= (Quote TRUE-IMDT) (nc-expr exp))]))

(: nc-expr* (-> (Listof D0-Expr) (Listof D1-Expr)))
(define (nc-expr* exp*) (map nc-expr exp*))

(: nc-bnd (->  D0-Bnd D1-Bnd))
(define (nc-bnd bnd)
  (match-let ([(cons uid rhs) bnd]) (cons uid (nc-expr rhs))))

(: nc-bnd* (-> D0-Bnd* D1-Bnd*))
(define (nc-bnd* bnd*) (map nc-bnd bnd*))

(: nc-stmt (-> D0-Stmt D1-Stmt))
(define (nc-stmt stm)
  (match-let ([(Op p! exp*) stm])
    (let ([exp* (nc-expr* exp*)])
      (Op p! exp*))))

(: nc-stmt* (-> D0-Stmt* D1-Stmt*))
(define (nc-stmt* stm*) (map nc-stmt stm*))

(: nc-bnd-code* (-> D0-Bnd-Code* D1-Bnd-Code*))
(define (nc-bnd-code* bnd*) (map nc-bnd-code bnd*))

(: nc-bnd-code (-> D0-Bnd-Code D1-Bnd-Code))
(define (nc-bnd-code bnd)
  (match-let ([(cons uid (Code uid* exp)) bnd])
    (cons uid (Code uid* (nc-tail exp)))))

