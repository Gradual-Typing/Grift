#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: compiler/data/remove-let                                          |
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
(provide remove-let)

(: remove-let (-> Data1-Lang Config Data2-Lang))
(define (remove-let prgm comp-config)
  (match-let ([(Prog (list name count type) (Labels bnd-code* tail)) prgm])
    (let ([bnd-code* : D2-Bnd-Code* (rl-bnd-code* bnd-code*)]
          [body : D2-Body (rl-body tail)])
      (Prog (list name count type) (Labels bnd-code* body)))))


(: rl-bnd-code* (-> D1-Bnd-Code* D2-Bnd-Code*))
(define (rl-bnd-code* bnd*) (map rl-bnd-code bnd*))

(: rl-bnd-code (-> D1-Bnd-Code D2-Bnd-Code))
(define (rl-bnd-code bnd)
  (match-let ([(cons uid (Code uid* exp)) bnd])
    (cons uid (Code uid* (rl-body exp)))))

(: rl-body (-> D1-Tail D2-Body))
(define (rl-body tail)
  (let-values ([(tail local-vars) (rl-tail tail)]) 
    (Locals local-vars tail)))

(: rl-tail (-> D1-Tail (values D2-Tail Uid*)))
(define (rl-tail tail)
  (match tail
    [(Let bnd* tail) 
     (let*-values ([(tail lv*) (rl-tail tail)]
                   [(stm* lv*) (rl-bnd* bnd* lv*)])
       (values (Begin stm* tail) lv*))]
    [(If t c a)
     (let*-values ([(t t-lv*) (rl-pred t)]
                   [(c c-lv*) (rl-tail c)]
                   [(a a-lv*) (rl-tail a)])
       (values  (If t c a) (append t-lv* c-lv* a-lv*)))]
    [(Begin stm* tail)
     (let*-values ([(tail lv*) (rl-tail tail)]
                  [(stm* lv*) (rl-stmt* stm* lv*)])
       (values (Begin stm* tail) lv*))]
    [(Return exp) 
     (let-values ([(exp lv*) (rl-expr exp)])
       (values (Return exp) lv*))]))

(: rl-expr (-> D1-Expr (values D2-Expr Uid*)))
(define (rl-expr exp)
  (match exp
    [(Let bnd* exp)
     (let*-values ([(exp lv*) (rl-expr exp)]
                   [(stm* lv*) (rl-bnd* bnd* lv*)])
       (values (Begin stm* exp) lv*))]
    [(If t c a)
     (let*-values ([(t t-lv*) (rl-pred t)]
                   [(c c-lv*) (rl-expr c)]
                   [(a a-lv*) (rl-expr a)])
         (values (If t c a) (append t-lv* c-lv* a-lv*)))]
    [(Begin stm* exp)
     (let*-values ([(exp lv*) (rl-expr exp)]
                   [(stm* lv*) (rl-stmt* stm* lv*)])
       (values (Begin stm* exp) lv*))]
    [(App exp exp*) 
     (let*-values ([(exp lv*) (rl-expr exp)]
                   [(exp* lv*^) (rl-expr* exp*)])
       (values (App exp exp*) (append lv* lv*^)))]
    [(Op p exp*)
     (let*-values ([(exp* lv*) (rl-expr* exp*)])
       (values (Op p exp*) lv*))]
    [(Halt) (values (Halt) '())]
    [(Var i) (values (Var i) '())]
    [(Code-Label i) (values (Code-Label i) '())]
    [(Quote k) (values (Quote k) '())]))

(: rl-pred (-> D1-Pred (values D2-Pred Uid*)))
(define (rl-pred exp)
  (match exp
    [(Let bnd* pred)
     (let*-values ([(pred lv*) (rl-pred exp)]
                   [(stm* lv*) (rl-bnd* bnd* lv*)])
       (values (Begin stm* pred) lv*))]
    [(If t c a)
     (let*-values ([(t t-lv*) (rl-pred t)]
                   [(c c-lv*) (rl-pred c)]
                   [(a a-lv*) (rl-pred a)])
         (values (If t c a) (append t-lv* c-lv* a-lv*)))]
    [(Begin stm* pred)
     (let*-values ([(pred lv*) (rl-pred pred)]
                   [(stm* lv*) (rl-stmt* stm* lv*)])
       (values (Begin stm* pred) lv*))]
    [(Relop p e1 e2)
     (let-values ([(e1 e1-lv*) (rl-expr e1)]
                  [(e2 e2-lv*) (rl-expr e2)])
       (values (Relop p e1 e2) (append e1-lv* e2-lv*)))]))

(: rl-expr* (-> (Listof D1-Expr) (values (Listof D2-Expr) Uid*)))
(define (rl-expr* exp*) 
  (if (null? exp*)
      (values '() '())
      (let*-values ([(a d) (values (car exp*) (cdr exp*))]
                    [(e e-lv*) (rl-expr a)]
                    [(e* e*-lv*) (rl-expr* d)])
        (values (cons e e*) (append e-lv* e*-lv*)))))

(: rl-bnd (->  D1-Bnd Uid* (values D2-Stmt Uid*)))
(define (rl-bnd bnd lv*)
  (match-let ([(cons uid rhs) bnd]) 
    (let-values ([(rhs lv*^) (rl-expr rhs)])
      (values (Assign uid rhs) (cons uid (append lv*^ lv*))))))

(: rl-bnd* (->  D1-Bnd* Uid* (values D2-Stmt* Uid*)))
(define (rl-bnd* bnd* lv*) 
  (if (null? bnd*)
      (values bnd* lv*)
      (let*-values ([(a) (car bnd*)]
                    [(d) (cdr bnd*)]
                    [(a lv*) (rl-bnd a lv*)]
                    [(a* lv*) (rl-bnd* d lv*)])
        (values (cons a a*) lv*))))

(: rl-stmt (-> D1-Stmt Uid* (values D2-Stmt Uid*)))
(define (rl-stmt stm uid*)
  (match-let ([(Op p! exp*) stm])
    (let*-values ([(exp* lv*) (rl-expr* exp*)])
      (values (Op p! exp*) (append lv* uid*)))))

(: rl-stmt* (-> D1-Stmt* Uid* (values D2-Stmt* Uid*)))
(define (rl-stmt* stm* lv*)
  (if (null? stm*)
      (values stm* lv*)
      (let*-values ([(a) (car stm*)]
                    [(d) (cdr stm*)]
                    [(a lv*) (rl-stmt a lv*)]
                    [(a* lv*) (rl-stmt* d lv*)])
        (values (cons a a*) lv*))))


