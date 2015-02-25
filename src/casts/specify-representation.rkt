#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: src/casts/build-structured-types
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
Description: This pass has to change the representation of anything that
is made an immediate. Kinda breaking the abstraction that I was hoping to create.
+-------------------------------------------------------------------------------+
| Source Grammar : Cast3
| Target Grammar : Lambda0
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require schml/src/helpers
         schml/src/errors
	 schml/src/language)
;; Only the pass is provided by this module
(provide specify-representation)

(TODO Rewrite any allocating procedures in this pass.
      Remebering the lessons of the runtime class.
      No complex expressions should be able to be evaluated while
      Allocating and setting a chunk of memory.
      Otherwise you will end up with partially initiallized chucks
      of memory in the heap while you are executing other code.
      Write some test cases to test this.
      Is it possible to enforce this by require all constructors to only
      have variables as sub expressions.
      Perhaps one pass before this.
      member to force the evaluation of sub-arguments)

;; Only allocate each of these once
(define FN-ARITY-INDEX-VALUE (Quote FN-ARITY-INDEX))
(define FN-RETURN-INDEX-VALUE (Quote FN-RETURN-INDEX))
(define FN-FMLS-OFFSET-VALUE (Quote FN-FMLS-OFFSET))
(define TYPE-TAG-MASK-VALUE (Quote TYPE-TAG-MASK))
(define TYPE-FN-TAG-VALUE (Quote TYPE-FN-TAG))
(define TYPE-ATOMIC-TAG-VALUE (Quote TYPE-ATOMIC-TAG))
(define TYPE-DYN-RT-VALUE-VALUE (Quote TYPE-DYN-RT-VALUE))
(define TYPE-INT-RT-VALUE-VALUE (Quote TYPE-INT-RT-VALUE))
(define TYPE-BOOL-RT-VALUE-VALUE (Quote TYPE-BOOL-RT-VALUE))
(define DYN-TAG-MASK-VALUE (Quote DYN-TAG-MASK))
(define DYN-BOXED-TAG-VALUE (Quote DYN-BOXED-TAG))
(define DYN-INT-TAG-VALUE (Quote DYN-INT-TAG))
(define DYN-BOOL-TAG-VALUE (Quote DYN-BOOL-TAG))
(define DYN-IMDT-SHIFT-VALUE (Quote DYN-IMDT-SHIFT))
(define DYN-BOX-SIZE-VALUE (Quote DYN-BOX-SIZE))
(define DYN-VALUE-INDEX-VALUE (Quote DYN-VALUE-INDEX))
(define DYN-TYPE-INDEX-VALUE (Quote DYN-TYPE-INDEX))
(define FALSE-IMDT-VALUE (Quote FALSE-IMDT))
(define TRUE-IMDT-VALUE (Quote TRUE-IMDT))
(define UNDEF-IMDT-VALUE (Quote UNDEF-IMDT))
(define UBOX-SIZE-VALUE (Quote UBOX-SIZE))
(define UBOX-VALUE-INDEX-VALUE (Quote UBOX-VALUE-INDEX))
(define GPROXY-SIZE-VALUE (Quote GPROXY-SIZE))
(define GPROXY-FOR-INDEX-VALUE (Quote GPROXY-FOR-INDEX))
(define GPROXY-FROM-INDEX-VALUE (Quote GPROXY-FROM-INDEX))
(define GPROXY-TO-INDEX-VALUE (Quote GPROXY-TO-INDEX))
(define GPROXY-BLAMES-INDEX-VALUE (Quote GPROXY-BLAMES-INDEX))

(: specify-representation (Cast4-Lang Config . -> . Lambda0-Lang))
(define (specify-representation prgm comp-config)
  (match-let ([(Prog (list name next type) exp) prgm])
    (let-values ([(exp next) (run-state (sr-expr exp) next)])
      (Prog (list name next type) exp))))

(: sr-expr (-> C4-Value (State Natural L0-Expr)))
(define (sr-expr exp)
  (match exp
    ;;Forms to be retained
    [(Lambda f* (Castable c e))
     (do (bind-state : (State Natural L0-Expr))
         (e : L0-Expr <- (sr-expr e))
         (return-state (Lambda f* (Castable c e))))]
    [(Letrec b* e)
     (do (bind-state : (State Natural L0-Expr))
         (b* : L0-Bnd  <- (sr-bnd* b*))
         (e  : L0-Expr <- (sr-expr e))
         (return-state (Letrec b* e)))]
    [(Let b* e)
     (do (bind-state : (State Natural L0-Expr))
         (b* : L0-Bnd  <- (sr-bnd* b*))
         (e  : L0-Expr <- (sr-expr e))
         (return-state (Let b* e)))]
    [(If t c a)
     (do (bind-state : (State Natural L0-Expr))
         (t : L0-Expr <- (sr-expr t))
         (c : L0-Expr <- (sr-expr c))
         (a : L0-Expr <- (sr-expr a))
         (return-state (If t c a)))]
    [(App e e*)
     (do (bind-state : (State Natural L0-Expr))
         (e  : L0-Expr  <- (sr-expr e))
         (e* : L0-Expr* <- (sr-expr* e*))
         (return-state (App e e*)))]
    [(Op p exp*)
     (do (bind-state : (State Natural L0-Expr))
         (e* : L0-Expr* <- (sr-expr* e*))
         (return-state (Op p e*)))]
    [(Fn-Caster e) (lift-state (inst Fn-Caster L0-Expr) (sr-expr e))]
    [(Var i)       (return-state (Var i))]
    [(Quote k) (let ([k  (if (boolean? k)
                             (if k TRUE-IMDT FALSE-IMDT)
                             k)])
                 (return-state (Quote k)))]
    ;; eliminated forms
    [(Type-Fn-arity e)
     (do (bind-state : (State Natural L0-Expr))
         (e : L0-Expr <- (sr-value e))
         (return-state (Op 'Array-ref (list e FN-ARITY-INDEX-VALUE))))]
    [(Type-Fn-return e)
     (do (bind-state : (State Natural L0-Expr))
         (e : L0-Expr <- (sr-value e))
         (return-state (Op 'Array-ref (list e FN-RETURN-INDEX-VALUE))))]
    [(Type-Fn-arg e1 e2)
      (do (bind-state : (State Natural L0-Expr))
          (e1 : L0-Expr <- (sr-value e1))
          (e2 : L0-Expr <- (sr-value e2))
          (let ([e2^ (match e2
                       [(Quote (? number? k)) (+ FN-FMLS-OFFSET k)]
                       [otherwiths (Op '+ (list e2 FN-FMLS-VALUE))])])
            (return-state (Op 'Array-ref (list e1 e2^)))))]
    [(Type-tag e)
     (do (bind-state : (State Natural L0-Expr))
         (e : L0-Expr <- (sr-value e))
         (return-state (Op 'binary-and (list e TYPE-TAG-MASK-VALUE))))]
    [(Dyn-tag e)
     (do (bind-state : (State Natural L0-Expr))
         (e : L0-Expr <- (sr-value e))
         (return-state (Op 'binary-and (list e DYN-TAG-MASK-VALUE))))]
    [(Dyn-immediate e)
     (do (bind-state : (State Natural L0-Expr))
         (e : L0-Expr <- (sr-value e))
         (return-state (Op '%>> (list e DYN-IMDT-SHIFT-VALUE))))]
    [(Dyn-make e1 e2)
     (do (bind-state : (State Natural L0-Expr))
         (e1 : L0-Expr (sr-expr e1))
         #;(e2 : L0-Expr (sr-expr e2))
         (sr-dyn-make e1 e2))]
    [(Dyn-type e)
     (do (bind-state : (State Natural L0-Expr))
         (e : L0-Expr <- (sr-value e))
         (return-state (Op 'Array-ref (list e DYN-TYPE-INDEX-VALUE))))]
    [(Dyn-value e)
     (do (bind-state : (State Natural L0-Expr))
         (e : L0-Expr <- (sr-value e))
         (return-state (Op 'Array-ref (list e DYN-VALUE-INDEX-VALUE))))]
    [(Blame e)
     (do (bind-state : (State Natural L0-Expr))
         (e : L0-Expr <- (sr-value e))
         (return-state
          (Begin
            (list (Op 'Print (list e))
                  (Op 'Exit  (list (Quote -1))))
            UNDEF-IMDT-VALUE)))]
    [(Observe (app recur e next) t)
     (do (bind-state : (State Natural L0-Expr))
         (e : L0-Expr (sr-value e))
         (lambda ([n : Natural]) : (Values L0-Expr Natural)
            ;; This is a break in the monad abstraction that needs fixed
            ;; But I want to remove the Observe abstraction and implement
            ;; Entirely here and now -- Let's get really observations are
            ;; not expressions
            (sr-observe e t n)))]
    [(Type t) (lambda ([n : Natural]) (sr-type t n))]
    [(Tag t)  (return-state (sr-tag t))]
    [(Begin eff* exp)
     (do (bind-state : (State Natural L0-Expr))
         (eff* : L0-Effect <- (sr-effect* eff*))
         (exp  : L0-Expr   <- (sr-value   exp))
         (return-state (Begin eff* exp)))]
    [(UGbox e)
     (do (bind-state : (State Natural L0-Expr))
         (e : L0-Expr <- (sr-value e))
         (alloc-set-ugbox e))]
    [(UGbox-ref e)
     (do (bind-state : (State Natural L0-Expr))
         (e : L0-Expr <- (sr-value e))
         (return-state (Op 'Array-ref (list e UGBOX-VALUE-INDEX))))]
    [(UGbox-set! e1 e2)
     (do (bind-state : (State Natural L0-Expr))
         (e1 : L0-Expr <- (sr-value e1))
         (e2 : L0-Expr <- (sr-value e2))
         (return-state (Op 'Array-set! (list e1 UGBOX-VALUE-INDEX e2))))]
    [(GRep-proxied? (app recur/next exp next))
     (do (bind-state : (State Natural L0-Expr))
         (e : L0-Expr <- (sr-value e))
         (return-state (Op 'binary-and (list e GREP-TAG-MASK-VALUE))))]
    [(Gproxy for from to blames)
     (do (bind-state : (State Natural L0-Expr))
         (for     : L0-Expr <- (sr-value for))
         (from    : L0-Expr <- (sr-value from))
         (to      : L0-Expr <- (sr-value to))
         (blames  : L0-Expr <- (sr-value blames))
         (alloc-tag-set-gproxy for from to blames))]
    [(Gproxy-for e)
     (lift-state (untag-deref-gproxy GPROXY-FOR-INDEX-VALUE) (sr-expr e))]
    [(Gproxy-from e)
     (lift-state (untag-deref-gproxy GPROXY-FROM-INDEX-VALUE) (sr-expr e))]
    [(Gproxy-to e)
     (lift-state (untag-deref-gproxy GPROXY-TO-INDEX-VALUE) (sr-expr e))]
    [(Gproxy-blames e)
     (lift-state (untag-deref-gproxy GPROXY-BLAMES-INDEX-VALUE) (sr-expr e))]))

(: untag-deref-gproxy (-> L0-Expr (-> L0-Expr L0-Expr)))
(define (untag-deref-gproxy index-e)
  (lambda ((proxy-e : L0-Expr)) : L0-Expr
   (Op 'Array-ref (list (Op 'binary-xor (list proxy-e GREP-PTR-MASK-VALUE))
                        index-e))))

(: alloc-tag-set-gproxy
   (L0-Expr L0-Expr L0-Expr L0-Expr . -> . (State Natural L0-Expr)))
(define (alloc-tag-set-gproxy for from to blames)
  (do (bind-state : (State Natural L0-Expr))
      (tmp : Uid (uid-state "gproxy"))
      (let* ([tmp-var (Var tmp)]
             [alloc   (Op 'alloc (list GPROXY-SIZE-VALUE))]
             [set-for (Op 'Array-set!
                          (list tmp-var GPROXY-FOR-INDEX-VALUE for))]
             [set-from (Op 'Array-set!
                           (list tmp-var GPROXY-FROM-INDEX-VALUE from))]
             [set-to (Op 'Array-set! 
                         (list tmp-var GPROXY-TO-INDEX-VALUE to))]
             [set-blames (Op 'Array-set!
                             (list tmp-var GPROXY-BLAMES-INDEX-VALUE blames))])
        (return-state
         (Let (list (cons tmp alloc))
              (Begin (list set-for set-from set-to set-blames)
                     (Op 'binary-xor (list tmp-var GPROXY-TAG-VALUE))))))))

(define (alloc-set-ugbox e)
  (do (bind-state : (State Natural L0-Expr))
      (tmp : Uid (uid-state "gproxy"))
      (let* ([tmp-var (Var tmp)]
             [alloc   (Op 'alloc (list UGBOX-SIZE-VALUE))]
             [set     (Op 'Array-set! (list tmp-var UGBOX-VALUE-INDEX-VALUE e))])
        (return-state
         (Let (list (cons tmp alloc))
              (Begin (list set)
                     (Op 'binary-xor (list tmp-var UGBOX-TAG-VALUE)))))))))

(: sr-expr* (-> (Listof C4-Expr) Natural (values (Listof L0-Expr) Natural)))
(define (sr-expr* e* n)
  (if (null? e*)
      (values '() n)
      (match-let ([(cons e e*) e*])
	(let*-values ([(e* n) (sr-expr* e* n)]
		      [(e n)  (sr-expr e n)])
	  (values (cons e e*) n)))))

(: sr-bnd* (-> C4-Bnd* Natural (values L0-Bnd* Natural)))
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
           n))))]
   [else (TODO implement reference code around here)]))

(: sr-dyn-make (-> L0-Expr C4-Expr (State Natural L0-Expr)))
(define (sr-dyn-make e1 e2)
  (match e2
    [(Type (Int))
     (return-state
      (Op '+ (list (Op '%<< (list e1 DYN-IMDT-SHIFT-VALUE))
                   DYN-INT-TAG-VALUE)))]
    [(Type (Bool))
     (return-state
      (Op '+ (list (Op '%<< (list e1 DYN-IMDT-SHIFT-VALUE))
                   DYN-BOOL-TAG-VALUE)))]
    [otherwise
     (do (bind-state : (State Natural L0-Expr))
         (e2  : L0-Expr <- (sr-value e2))
         (tmp : Uid     <- (next-uid "dyn_box"))
         (let* ([tmp-var  (Var tmp)]
                [set-val  (Op 'Array-set!
                              (list tmp-var DYN-VALUE-INDEX-VALUE exp))]
                [set-type (Op 'Array-set!
                              (list tmp-var DYN-TYPE-INDEX-VALUE type))]
                [alloc    (Op 'Alloc (list DYN-BOX-SIZE-VALUE))])
           (return-state
            (Let (list (cons tmp alloc))
                 (Begin (list set-val set-type) tmp-var)))))]))

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
      (Begin (list (Op 'Print (list (Quote "Dynamic : ?\n")))) (Quote 0))]
     [else (TODO implement thing for reference types)])

   next))

(: sr-tag (Tag-Symbol . -> . (Quote Integer)))
(define (sr-tag t)
  (case t
    [(Int)    (Quote DYN-INT-TAG)]
    [(Bool)   (Quote DYN-BOOL-TAG)]
    [(Atomic) (Quote TYPE-ATOMIC-TAG)]
    [(Fn)     (Quote DYN-BOXED-TAG)]
    [(Boxed)  (Quote TYPE-FN-TAG)]))
