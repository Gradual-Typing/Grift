#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: src/casts/build-structured-types
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
Description: This pass has to change the representation of anything that
is made an immediate. Kinda breaking the abstraction that I was hoping to create.


+-------------------------------------------------------------------------------+
| Source Grammar : Cast2
| Target Grammar : Lambda0
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require schml/src/helpers
         schml/src/errors
	 schml/src/language)
;; Only the pass is provided by this module
(provide specify-representation)

(: specify-representation (Cast2-Lang Config . -> . Lambda0-Lang))
(define (specify-representation prgm comp-config)
  (match-let ([(Prog (list name count type) exp) prgm])
    (let-values ([(exp count) (sr-expr exp count)])
      (Prog (list name count type) exp))))

(: sr-expr (-> C2-Expr Natural (values L0-Expr Natural)))
(define (sr-expr exp next)
  (: recur (-> C2-Expr (values L0-Expr Natural)))
  (define (recur exp)
    (match exp
      ;;Forms to be retained
      [(Lambda f* _  (Castable ctr (app recur exp next)))
       (values (Lambda f* #f (Castable ctr exp)) next)]
      [(Letrec b* (app recur exp next)) 
       (let-values ([(b* next) (sr-bnd* b* next)])
	 (values (Letrec b* exp) next))]
      [(Let b* (app recur exp next)) 
       (let-values ([(b* next) (sr-bnd* b* next)])
	 (values (Let b* exp) next))]
      [(If (app recur tst next) csq alt)
       (let*-values ([(csq next) (sr-expr csq next)]
		     [(alt next) (sr-expr alt next)])
	 (values (If tst csq alt) next))]
      [(App (app recur exp next) exp*)
       (let-values ([(exp* next) (sr-expr* exp* next)])
	 (values (App exp exp*) next))]
      [(Op p exp*)
       (let-values ([(exp* next) (sr-expr* exp* next)])
	 (values (Op p exp*) next))]
      [(Fn-Caster (app recur e next))
       (values (Fn-Caster e) next)]
      [(Var i) (values (Var i) next)]
      [(Quote k) 
       (values (if (boolean? k)
                   (Quote (if k TRUE-IMDT FALSE-IMDT)) 
                   (Quote k)) 
               next)]
      ;; eliminated forms
      [(Type-Fn-ref (app recur e next) k)
       (values
        (cond
          [(eq? 'arity k) 
           (Op 'Array-ref (list e (Quote FN-ARITY-INDEX)))] 
          [(eq? 'return k) 
           (Op 'Array-ref (list e (Quote FN-RETURN-INDEX)))]
          [else 
           (Op 'Array-ref (list e (Quote (+ FN-FMLS-OFFSET k))))])
        next)]
      [(Type-tag (app recur e next)) 
       (values (Op 'binary-and (list e (Quote TYPE-TAG-MASK))) next)]
      [(Dyn-tag (app recur e next))
       (values (Op 'binary-and (list e (Quote DYN-TAG-MASK))) next)]
      [(Dyn-immediate (app recur e next))
       (values (Op '%>> (list e (Quote DYN-IMDT-SHIFT))) next)]
      [(Dyn-ref (app recur e next) k)
       (values
        (case k
          [(value) (Op 'Array-ref (list e (Quote DYN-VALUE-INDEX)))] 
          [(type) (Op 'Array-ref (list e (Quote DYN-TYPE-INDEX)))])
        next)]
      [(Dyn-make (app recur e next) t)
       (match t
        [(Type (Int)) (sr-dyn-immediate e DYN-INT-TAG next)]
        [(Type (Bool)) (sr-dyn-immediate e DYN-BOOL-TAG next)]
        [otherwise 
         (let-values ([(t next) (sr-expr t next)])
           (sr-dyn-box e t next))])]
      [(Blame (app recur e next))
       (values (Begin (list (Op 'Print (list e))
                            (Op 'Exit '()))
                      (Quote UNDEF-IMDT)) next)]
      [(Observe (app recur e next) t) (sr-observe e t next)]
      [(Type t) (sr-type t next)]
      [(Tag t) (values (sr-tag t) next)]))
  (recur exp))

(: sr-expr* (-> (Listof C2-Expr) Natural (values (Listof L0-Expr) Natural)))
(define (sr-expr* e* n)
  (if (null? e*)
      (values '() n)
      (match-let ([(cons e e*) e*])
	(let*-values ([(e* n) (sr-expr* e* n)]
		      [(e n)  (sr-expr e n)])
	  (values (cons e e*) n)))))

(: sr-bnd* (-> C2-Bnd* Natural (values L0-Bnd* Natural)))
(define (sr-bnd* b* n)
  (if (null? b*)
      (values '() n)
      (match-let ([(cons (cons u e) b*) b*])
	(let*-values ([(b* n) (sr-bnd* b* n)]
		      [(e n)  (sr-expr e n)]
		      [(b) (cons u e)])
	  (values (cons b b*) n)))))

(: sr-type (-> Schml-Type Natural (values L0-Expr Natural)))
(define (sr-type t n)
  (: array-set* (-> L0-Expr Integer (Listof L0-Expr) (Listof L0-Stmt)))
  (define (array-set* a i f*)
    (if (null? f*)
        '()
        (cons
         (Op 'Array-set! (list a (Quote i) (car f*)))
         (array-set* a (add1 i) (cdr f*)))))
  (: sr-type* (-> (Listof Schml-Type) Natural (values (Listof L0-Expr) Natural)))
  (define (sr-type* t* n)
    (if (null? t*)
        (values '() n)
        (let*-values ([(t n) (sr-type (car t*) n)]
                      [(t* n) (sr-type* (cdr t*) n)])
          (values (cons t t*) n))))
  (cond
   [(Int? t)  (values (Quote TYPE-INT-RT-VALUE) n)]
   [(Bool? t) (values (Quote TYPE-BOOL-RT-VALUE) n)]
   [(Dyn? t)  (values (Quote TYPE-DYN-RT-VALUE) n)]
   [(Fn? t) 
    (match-let ([(Fn a f* r) t])
      (let*-values ([(tmp n) (next-uid "tmp" n)]
                    [(tmp-var) (Var tmp)]
                    [(f* n) (sr-type* f* n)]
                    [(r n) (sr-type r n)])
        (let* ([bnd (cons tmp (Op 'Alloc (list (Quote (+ a FN-FMLS-OFFSET)))))]
               [bnd* (list bnd)]
               [stmt1 (Op 'Array-set! (list tmp-var (Quote FN-ARITY-INDEX) (Quote a)))]
               [stmt2 (Op 'Array-set! (list tmp-var (Quote FN-RETURN-INDEX) r))]
               [stmt* (array-set* tmp-var FN-FMLS-OFFSET f*)])
          (values
           (Let bnd* (Begin (cons stmt1 (cons stmt2 stmt*)) tmp-var))
           n))))]))

(: sr-dyn-immediate (-> L0-Expr Integer Natural (values L0-Expr Natural)))
(define (sr-dyn-immediate exp tag next)
  (values (Op '+ (list (Op '%<< (list exp (Quote DYN-IMDT-SHIFT))) (Quote tag)))
          next))

(: sr-dyn-box (-> L0-Expr L0-Expr Natural (values L0-Expr Natural)))
(define (sr-dyn-box exp type next)
  (let*-values ([(tmp next) (next-uid "dyn_box" next)]
                [(tmp-var)  (Var tmp)])
    (values
     (Let (list (cons tmp (Op 'Alloc (list (Quote DYN-BOX-SIZE)))))
     (Begin
      (list
       (Op 'Array-set! (list tmp-var (Quote DYN-VALUE-INDEX) exp))
       (Op 'Array-set! (list tmp-var (Quote DYN-TYPE-INDEX) type)))
      tmp-var))
     next)))

(: sr-observe (-> L0-Expr Schml-Type Natural (values L0-Expr Natural)))
(define (sr-observe e t next)
  (values 
   (cond
    [(Int? t)
    (Begin 
     (list (Op 'Printf (list (Quote "Int : %d\n") e)))
     (Quote 0))]
   [(Bool? t)
    (If (Op '= (list (Quote TRUE-IMDT) e))
        (Begin (list (Op 'Print (list (Quote "Bool : #t\n")))) (Quote 0))
        (Begin (list (Op 'Print (list (Quote "Bool : #f\n")))) (Quote 0)))]
   [(Fn? t)
    (Begin (list (Op 'Print (list (Quote "Function : ?\n")))) (Quote 0))]
   [(Dyn? t)
    (Begin (list (Op 'Print (list (Quote "Dynamic : ?\n")))) (Quote 0))])
   next))

(: sr-tag (Tag-Symbol . -> . (Quote Integer)))
(define (sr-tag t)
  (case t
    [(Int)    (Quote DYN-INT-TAG)] 
    [(Bool)   (Quote DYN-BOOL-TAG)] 
    [(Atomic) (Quote TYPE-ATOMIC-TAG)] 
    [(Fn)     (Quote DYN-BOXED-TAG)] 
    [(Boxed)  (Quote TYPE-FN-TAG)]))
