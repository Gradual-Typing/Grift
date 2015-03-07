#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: src/casts/build-structured-types
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
Description: This pass exposes the memory layout of aspects of the program.
After this pass language complexity decreases greatly! But all operations are
exposed as the effects that they truelly are.
+-------------------------------------------------------------------------------+
| Source Grammar : Cast4
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

(TODO Talk to jeremy about taging all heap values)


;; Only allocate each of these once
(define FN-ARITY-INDEX-VALUE      : D0-Expr (Quote FN-ARITY-INDEX))
(define FN-RETURN-INDEX-VALUE     : D0-Expr (Quote FN-RETURN-INDEX))
(define FN-FMLS-OFFSET-VALUE      : D0-Expr (Quote FN-FMLS-OFFSET))
(define TYPE-TAG-MASK-VALUE       : D0-Expr (Quote TYPE-TAG-MASK))
(define TYPE-FN-TAG-VALUE         : D0-Expr (Quote TYPE-FN-TAG))
(define TYPE-ATOMIC-TAG-VALUE     : D0-Expr (Quote TYPE-ATOMIC-TAG))
(define TYPE-DYN-RT-VALUE-VALUE   : D0-Expr (Quote TYPE-DYN-RT-VALUE))
(define TYPE-INT-RT-VALUE-VALUE   : D0-Expr (Quote TYPE-INT-RT-VALUE))
(define TYPE-BOOL-RT-VALUE-VALUE  : D0-Expr (Quote TYPE-BOOL-RT-VALUE))
(define DYN-TAG-MASK-VALUE        : D0-Expr (Quote DYN-TAG-MASK))
(define DYN-BOXED-TAG-VALUE       : D0-Expr (Quote DYN-BOXED-TAG))
(define DYN-INT-TAG-VALUE         : D0-Expr (Quote DYN-INT-TAG))
(define DYN-BOOL-TAG-VALUE        : D0-Expr (Quote DYN-BOOL-TAG))
(define DYN-IMDT-SHIFT-VALUE      : D0-Expr (Quote DYN-IMDT-SHIFT))
(define DYN-BOX-SIZE-VALUE        : D0-Expr (Quote DYN-BOX-SIZE))
(define DYN-VALUE-INDEX-VALUE     : D0-Expr (Quote DYN-VALUE-INDEX))
(define DYN-TYPE-INDEX-VALUE      : D0-Expr (Quote DYN-TYPE-INDEX))
(define FALSE-IMDT-VALUE          : D0-Expr (Quote FALSE-IMDT))
(define TRUE-IMDT-VALUE           : D0-Expr (Quote TRUE-IMDT))
(define UNDEF-IMDT-VALUE          : D0-Expr (Quote UNDEF-IMDT))
(define GREP-TAG-MASK-VALUE       : D0-Expr (Quote GREP-TAG-MASK))
(define UGBOX-SIZE-VALUE          : D0-Expr (Quote UGBOX-SIZE))
(define UGBOX-VALUE-INDEX-VALUE   : D0-Expr (Quote UGBOX-VALUE-INDEX))
(define GPROXY-TAG-VALUE          : D0-Expr (Quote GPROXY-TAG))
(define GPROXY-SIZE-VALUE         : D0-Expr (Quote GPROXY-SIZE))
(define GPROXY-FOR-INDEX-VALUE    : D0-Expr (Quote GPROXY-FOR-INDEX))
(define GPROXY-FROM-INDEX-VALUE   : D0-Expr (Quote GPROXY-FROM-INDEX))
(define GPROXY-TO-INDEX-VALUE     : D0-Expr (Quote GPROXY-TO-INDEX))
(define GPROXY-BLAMES-INDEX-VALUE : D0-Expr (Quote GPROXY-BLAMES-INDEX))

(struct SR ([next : Nat] [code* : D0-Bnd-Code*]))

(: sr-uid-state (-> String (State SR Uid)))
(define (sr-uid-state name)
  (do (bind-state : (State SR Uid))
      (s : SR <- get-state)
      (match-let ([(SR n c* f*) s])
        (let-values ([(u n) (next-uid str n)])
          ((put-state (SR n c* f*)) : (State SR Null))
          (return-state u)))))

(: sr-add-code (-> Uid D0-Code (State SR Null)))
(define (sr-add-code id code)
  (do (bind-state : (State SR Uid))
      (s : SR <- get-state)
      (match-let ([(SR n c* f*) s])
        (let* ([c  : D0-Code  (cons id code)]
               [c* : D0-Code* (cons c c*)])
          ((put-state (SR n c* f*)) : (State SR Null))
          (return-state '())))))

(: specify-representation (Cast7-Lang Config . -> . Data0-Lang))
(define (specify-representation prgm comp-config)
  (match-let ([(Prog (list name next type) val) prgm])
    (match-let-values
         ([(val (SR next bnd-code*)) (run-state ((sr-value (hash)) val) next)])
     (Prog (list name next type)
           (Labels bnd-code*
                   val)))))

(: sr-value (-> C4-Value (State SR D0-Expr)))
(define (sr-value env)
  (define (recur val)
    (match val
      [(Let b* e)
       (do (bind-state : (State SR D0-Expr))
           (b* : D0-Bnd*  <- (sr-bnd* b*))
           (let* ([id* : Uid* (map car b*)]
                  [env : Env  (extend* env id* (map var id*))])
             (e  : D0-Expr  <- ((sr-value env) e))
             (return-state (Let b* e))))]
      [(If t c a)
       (do (bind-state : (State SR D0-Expr))
           (t : D0-Expr <- (recur t))
           (c : D0-Expr <- (recur c))
           (a : D0-Expr <- (recur a))
           (return-state (If t c a)))]
      [(App (cons e e^) e*)
       (do (bind-state : (State SR D0-Expr))
           (e  : D0-Expr  <- (sr-value  e))
           (e^ : D0-Expr  <- (sr-value  e^))
           (e* : D0-Expr* <- (sr-value* e*))
           (return-state (App e (cons e^ e))))]
      [(Op p e*)
       (do (bind-state : (State SR D0-Expr))
           (e* : D0-Expr* <- (sr-value* e*))
           (return-state (Op p e*)))]
      [(Fn-Caster e) (lift-state (inst Fn-Caster D0-Expr) (sr-value e))]
      [(Var i)       (return-state (Var i))]
      [(Quote k) (return-state (if (boolean? k)
                                   (if k
                                       TRUE-IMDT-VALUE
                                       FALSE-IMDT-VALUE)
                                   (Quote k)))]
      ;; eliminated forms
      [(Type-Fn-arity e)
       (do (bind-state : (State SR D0-Expr))
           (e : D0-Expr <- (sr-value e))
         (return-state (Op 'Array-ref (list e FN-ARITY-INDEX-VALUE))))]
      [(Type-Fn-return e)
       (do (bind-state : (State SR D0-Expr))
           (e : D0-Expr <- (sr-value e))
         (return-state (Op 'Array-ref (list e FN-RETURN-INDEX-VALUE))))]
      [(Type-Fn-arg e1 e2)
       (do (bind-state : (State SR D0-Expr))
           (e1 : D0-Expr <- (sr-value e1))
         (e2 : D0-Expr <- (sr-value e2))
         (let ([e2^ (match e2
                      [(Quote (? number? k)) (Quote (+ FN-FMLS-OFFSET k))]
                      [otherwiths (Op '+ (list e2 FN-FMLS-OFFSET-VALUE))])])
           (return-state (Op 'Array-ref (list e1 e2^)))))]
      [(Type-tag e)
       (do (bind-state : (State SR D0-Expr))
           (e : D0-Expr <- (sr-value e))
         (return-state (Op 'binary-and (list e TYPE-TAG-MASK-VALUE))))]
      [(Dyn-tag e)
       (do (bind-state : (State SR D0-Expr))
           (e : D0-Expr <- (sr-value e))
         (return-state (Op 'binary-and (list e DYN-TAG-MASK-VALUE))))]
      [(Dyn-immediate e)
       (do (bind-state : (State SR D0-Expr))
           (e : D0-Expr <- (sr-value e))
         (return-state (Op '%>> (list e DYN-IMDT-SHIFT-VALUE))))]
      [(Dyn-make e1 e2)
       (do (bind-state : (State SR D0-Expr))
           (e1 : D0-Expr <- (sr-value e1))
         (sr-dyn-make e1 e2))]
      [(Dyn-type e)
       (do (bind-state : (State SR D0-Expr))
           (e : D0-Expr <- (sr-value e))
         (return-state (Op 'Array-ref (list e DYN-TYPE-INDEX-VALUE))))]
      [(Dyn-value e)
       (do (bind-state : (State SR D0-Expr))
           (e : D0-Expr <- (sr-value e))
         (return-state (Op 'Array-ref (list e DYN-VALUE-INDEX-VALUE))))]
      [(Blame e)
       (do (bind-state : (State SR D0-Expr))
           (e : D0-Expr <- (sr-value e))
         (return-state
          (Begin
            (list (Op 'Print (list e))
                  (Op 'Exit  (list (Quote -1))))
            UNDEF-IMDT-VALUE)))]
      [(Observe e t)
       (do (bind-state : (State SR D0-Expr))
           (e : D0-Expr <- (sr-value e))
         (lambda ([n : SR]) : (Values D0-Expr SR)
                 ;; This is a break in the monad abstraction that needs fixed
                 ;; But I want to remove the Observe abstraction and implement
                 ;; Entirely here and now -- Let's get really observations are
                 ;; not expressions
                 (sr-observe e t n)))]
      [(Type t) (lambda ([n : SR]) (sr-type t n))]
      [(Tag  t) (return-state (sr-tag t))]
      [(Begin eff* exp)
       (do (bind-state : (State SR D0-Expr))
           (eff* : D0-Effect* <- (sr-effect* eff*))
         (exp  : D0-Expr    <- (sr-value   exp))
         (return-state (Begin eff* exp)))]
      [(UGbox e)
       (do (bind-state : (State SR D0-Expr))
           (e : D0-Expr <- (sr-value e))
         (alloc-set-ugbox e))]
      [(UGbox-ref e)
       (do (bind-state : (State SR D0-Expr))
           (e : D0-Expr <- (sr-value e))
         (return-state (Op 'Array-ref (list e UGBOX-VALUE-INDEX-VALUE))))]
      [(GRep-proxied? e)
       (do (bind-state : (State SR D0-Expr))
           (e : D0-Expr <- (sr-value e))
         (return-state (Op 'binary-and (list e GREP-TAG-MASK-VALUE))))]
      [(Gproxy for from to blames)
       (do (bind-state : (State SR D0-Expr))
           (for     : D0-Expr <- (sr-value for))
         (from    : D0-Expr <- (sr-value from))
         (to      : D0-Expr <- (sr-value to))
         (blames  : D0-Expr <- (sr-value blames))
         (alloc-tag-set-gproxy for from to blames))]
      [(Gproxy-for e)
       (lift-state (untag-deref-gproxy GPROXY-FOR-INDEX-VALUE) (sr-value e))]
      [(Gproxy-from e)
       (lift-state (untag-deref-gproxy GPROXY-FROM-INDEX-VALUE) (sr-value e))]
      [(Gproxy-to e)
       (lift-state (untag-deref-gproxy GPROXY-TO-INDEX-VALUE) (sr-value e))]
      [(Gproxy-blames e)
       (lift-state (untag-deref-gproxy GPROXY-BLAMES-INDEX-VALUE) (sr-value e))]))
  sr-value/env)

(: untag-deref-gproxy (-> D0-Expr (-> D0-Expr D0-Expr)))
(define (untag-deref-gproxy index-e)
  (lambda ((proxy-e : D0-Expr)) : D0-Expr
   (Op 'Array-ref (list (Op 'binary-xor (list proxy-e GPROXY-TAG-VALUE))
                        index-e))))

(: alloc-tag-set-gproxy
   (D0-Expr D0-Expr D0-Expr D0-Expr . -> . (State SR D0-Expr)))
(define (alloc-tag-set-gproxy for from to blames)
  (do (bind-state : (State SR D0-Expr))
      (tmp : Uid <- (uid-state "gproxy"))
      (let* ([tmp-var (Var tmp)]
             [alloc   (Op 'Alloc (list GPROXY-SIZE-VALUE))]
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

(: alloc-set-ugbox (D0-Expr . -> . (State SR D0-Expr)))
(define (alloc-set-ugbox e)
  (do (bind-state : (State SR D0-Expr))
      (tmp : Uid <- (uid-state "gproxy"))
      (let* ([tmp-var (Var tmp)]
             [alloc   (Op 'Alloc (list UGBOX-SIZE-VALUE))]
             [set     (Op 'Array-set! (list tmp-var UGBOX-VALUE-INDEX-VALUE e))])
        (return-state
         (Let (list (cons tmp alloc))
              (Begin (list set) tmp-var))))))

(: sr-value* (-> C4-Value* (State SR D0-Expr*)))
(define (sr-value* e*) (map-state sr-value e*))


(: sr-effect (-> C4-Effect (State SR D0-Effect)))
(define (sr-effect eff)
  (match eff
    [(Letrec b* e)
     (lift-state (inst Letrec D0-Bnd* D0-Effect)
                 (sr-bnd* b*) (sr-effect e))]
    [(Let b* e)
     (lift-state (inst Let D0-Bnd* D0-Effect)
                 (sr-bnd* b*) (sr-effect e))]
    [(Begin e* (No-Op))
     (lift-state (inst Begin D0-Effect* No-Op)
                 (sr-effect* e*)
                 ((inst return-state SR No-Op) NO-OP))]
    [(App e e*)
     (lift-state (inst App D0-Expr D0-Expr*)
                 (sr-value e) (sr-value* e*))]
    [(If t c a)
     (lift-state (inst If D0-Expr D0-Effect D0-Effect)
                 (sr-value t) (sr-effect c) (sr-effect a))]
    [(No-Op) (return-state NO-OP)]
    [(UGbox-set! e1 e2)
     (do (bind-state : (State SR D0-Effect))
         (e1 : D0-Expr <- (sr-value e1))
         (e2 : D0-Expr <- (sr-value e2))
         (return-state
          (Op 'Array-set! (list e1 UGBOX-VALUE-INDEX-VALUE e2))))]))

;; And the fold over a list of effects
(: sr-effect* (-> C4-Effect* (State SR D0-Effect*)))
(define (sr-effect* e*) (map-state sr-effect e*))

;; What to do with a binding
(: sr-bnd (-> C4-Bnd (State SR D0-Bnd)))
(define (sr-bnd b)
  (do (bind-state : (State SR D0-Bnd))
      (e : D0-Expr <- (sr-value (cdr b)))
      (let ([i : Uid (car b)])
        (return-state (cons i e)))))

;; And the catamorphism for a List of bind
(: sr-bnd* (-> C4-Bnd* (State SR D0-Bnd*)))
(define (sr-bnd* b*) (map-state sr-bnd b*))

(: sr-type (-> Schml-Type SR (values D0-Expr SR)))
(define (sr-type t n)
  (: array-set* (-> D0-Expr Integer (Listof D0-Expr) (Listof D0-Stmt)))
  (define (array-set* a i f*)
    (if (null? f*)
        '()
        (cons
         (Op 'Array-set! (list a (Quote i) (car f*)))
         (array-set* a (add1 i) (cdr f*)))))
  (: sr-type* (-> (Listof Schml-Type) SR (values (Listof D0-Expr) SR)))
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

(: sr-dyn-make (-> D0-Expr C4-Value (State SR D0-Expr)))
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
     (do (bind-state : (State SR D0-Expr))
         (e2  : D0-Expr <- (sr-value e2))
         (tmp : Uid     <- (uid-state "dyn_box"))
         (let* ([tmp-var  : D0-Expr (Var tmp)]
                [alloc    : D0-Expr (Op 'Alloc (list DYN-BOX-SIZE-VALUE))]
                [set-val  : D0-Effect (Op 'Array-set!
                                          (list tmp-var DYN-VALUE-INDEX-VALUE e1))]
                [set-type : D0-Effect (Op 'Array-set!
                                          (list tmp-var DYN-TYPE-INDEX-VALUE e2))])
           (return-state
            (Let (list (cons tmp alloc))
                 (Begin (list set-val set-type) tmp-var)))))]))

(: sr-observe (-> D0-Expr Schml-Type SR (values D0-Expr SR)))
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

(: sr-expr (-> Env (-> L3-Expr L4-Expr)))
(define (sr-expr env)
  (define-syntax-rule (clos-code-ref c)
    (Op 'Clos:ref (list c (Quote 0))))
  (: recur (-> L3-Expr L4-Expr))
  (define (recur exp)
    (match exp
      [(LetP p* (LetC c* e)) (sr-let-proc p* c* e env)]
      [(Let bnd* exp) (Let (sr-bnd-data* bnd* env)
			   ((sr-expr env) exp))]
      [(App (cons (app recur e) (app recur e^)) e*)
       (if (Code-Label? e)
	   (App e (cons e^ (sr-expr* e* env)))
	   (App (Op 'Array-ref (list e (Quote CLOS-CODE-INDEX)))
                (cons e^ (sr-expr* e* env))))]
      [(Op p e*) (Op p (sr-expr* e* env))]
      [(Fn-Caster (app recur e))
       (Op 'Array-ref (list e (Quote CLOS-CSTR-INDEX)))]
      [(If (app recur t) (app recur c) (app recur a)) (If t c a)]
      [(Begin stm* (app recur exp)) (Begin (sr-stmt* stm* env) exp)]
;;      [(Halt) (Halt)]
      [(Var i) (env-lookup env i)]
      [(Quote k) (Quote k)]))
  recur)

(: sr-expr* (-> (Listof L3-Expr) Env (Listof L4-Expr)))
(define (sr-expr* exp* env) (map (sr-expr env) exp*))

;; this is rediculous to maintain
;; TODO make the code loop through the closures and build sets and refs all at once


(: sr-let-proc (-> L3-Bnd-Procedure* L3-Bnd-Closure* L3-Expr Env L4-Expr))
(define (sr-let-proc bndp* bndc* exp env)
  (let*-values ([(bndp* env) (sr-bndp* bndp* env)]
		[(bndc* setc* env) (sr-bndc* bndc* env)])
    (Labels bndp* (Let bndc* (Begin setc* ((sr-expr env) exp))))))

(: sr-bndp* (-> L3-Bnd-Procedure* Env (values L4-Bnd-Code* Env)))
(define (sr-bndp* bnd* env)
  (: loop (-> L3-Bnd-Procedure* L4-Bnd-Code* Env (values L4-Bnd-Code* Env)))
  (define (loop b3* b4* env)
    (if (null? b3*)
	(values b4* env)
	(let* ([b3 (car b3*)] [b3* (cdr b3*)])
	  (match-let ([(cons u (Procedure cp param* ctr? fvar* exp)) b3])
	    (let ([b4 (cons u (sr-procedure cp param* ctr? fvar* exp))])
	      (loop b3* (cons b4 b4*) (env-extend env u (Code-Label u))))))))
  (loop bnd* '() env))


(: sr-bndc* (-> L3-Bnd-Closure* Env (values L4-Bnd-Data* L4-Stmt* Env)))
(define (sr-bndc* b3* env)
  (: collect-clos (-> L3-Bnd-Closure* Env Env))
  ;; since the environment is short circuts there may not be a need to
  ;; do this but it could be used as a means to identifify well known
  ;; functions
  (define (collect-clos b* env)
    (if (null? b*)
	env
	(let ((u (caar b*)))
	  (collect-clos (cdr b*) (env-extend env u (Var u))))))
  (: fold-clos (-> L3-Bnd-Closure* Env (values L4-Bnd-Data* L4-Stmt*)))
  (define (fold-clos bnd* env)
    (if (null? bnd*)
	(values '() '())
	(let*-values ([(bnd bnd*) (values (car bnd*) (cdr bnd*))]
		      [(bnd* set*)  (fold-clos bnd* env)])
	  (values (mk-bnd* bnd bnd*) (mk-set* bnd set* env)))))
  (: mk-bnd* (-> L3-Bnd-Closure L4-Bnd-Data* L4-Bnd-Data*))
  (define (mk-bnd* b3 b4*)
    (match-let ([(cons uid (Closure-Data lbl ctr? free*)) b3])
      (let* ([size (+ CLOS-FVAR-OFFSET (length free*))]
	     [bnd (cons uid (Op 'Alloc (list (Quote size))))])
	(cons bnd b4*))))
  (: mk-set* (-> L3-Bnd-Closure L4-Stmt* Env L4-Stmt*))
  (define (mk-set* b3 set* env)
    (: next-set (-> L4-Expr Index Uid L4-Stmt* Env (Values L4-Stmt* Index)))
    (define (next-set v i u set* env)
      (values (cons (Op 'Array-set! (list v (Quote i) (env-lookup env u))) set*)
	      (let ([i (add1 i)])
		(if (index? i)
		    i
		    (error 'sr-mk-set!* "closure too large")))))
    (match-let ([(cons uid (Closure-Data lbl ctr? free*)) b3])
      (: loop (-> (Var Uid) Index Uid* L4-Stmt* L4-Stmt*))
      (define (loop cvar i free* set*)
	(if (null? free*)
	    set*
	    (let*-values ([(free free*) (values (car free*) (cdr free*))]
			  [(set* i) (next-set cvar i free set* env)])
	      (if (index? i)
		  (loop cvar i free* set*)
		  (error 'indroduce-closure-primitives "Closure is to big")))))
      (let* ([cvar (Var uid)]
             [setcode (Op 'Array-set! (list cvar (Quote CLOS-CODE-INDEX) (env-lookup env lbl)))]
             [cstr (if ctr? (env-lookup env ctr?) (Quote FALSE-IMDT))]
             [setcstr (Op 'Array-set! (list cvar (Quote CLOS-CSTR-INDEX) cstr))]
             [set* (cons setcode (cons setcstr set*))]);; I added this line
	(loop cvar CLOS-FVAR-OFFSET free* set*))))
  (let*-values ([(env) (collect-clos b3* env)]
		[(bnd* exp*) (fold-clos b3* env)])
    (values bnd* exp* env)))

(: sr-procedure (-> Uid Uid* (Option Uid) Uid* L3-Expr L4-Code))
(define (sr-procedure cp param* ctr? free* body)
  ;; build-env has two parts first the refs are generated
  (: build-env ((Var Uid) (Option Uid) (Listof Uid) . -> . Env))
  (define (build-env cvar ctr? free*)
    (: loop (-> Uid* Index Env Env))
    (define (loop f* i env)
      (let ([i^ (add1 i)])
	(if (index? i^)
	    (if (null? f*)
		env
		(loop
		 (cdr f*)
		 i^
		 (env-extend env (car f*) (Op 'Array-ref (list cvar (Quote i))))))
	    (error 'specify-representation "Index rolled over"))))
    (loop free* CLOS-FVAR-OFFSET (hash)))
  (Code (cons cp param*)
	((sr-expr (build-env (Var cp) ctr? free*)) body)))

(: sr-bnd-data* (-> L3-Bnd-Data* Env L4-Bnd-Data*))
(define (sr-bnd-data* b3* env)
  (let ([sr-expr (sr-expr env)])
    (: loop (-> L3-Bnd-Data* L4-Bnd-Data* L4-Bnd-Data*))
    (define (loop b3* b4*)
      (if (null? b3*)
	  b4*
	  (let* ([b3 (car b3*)]
		 [b3* (cdr b3*)]
		 [b4 (cons (car b3) (sr-expr (cdr b3)))])
	    (loop b3* (cons b4 b4*)))))
    (loop b3* '())))

(define-type Env (HashTable Uid L4-Expr))

(define-syntax-rule (extend e k v)
  (hash-set e k v))

(: extend* (-> Env Uid* D0-Expr*))
(define (extend* env id* exp*)
 (match* (id* exp*)
   [('() _) env]
   [(_ '()) env]
   [((cons id id*) (cons exp exp*))
    (extend* (extend env id exp) id* exp*)]))

(: env-lookup (-> Env Uid L4-Expr))
(define (env-lookup e u)
  (hash-ref e u (lambda () (Var u))))

