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

#;
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
#;
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
(define CLOS-CODE-INDEX-VALUE     : D0-Expr (Quote CLOS-CODE-INDEX))
(define CLOS-CSTR-INDEX-VALUE     : D0-Expr (Quote CLOS-CSTR-INDEX))
(define CLOS-FVAR-OFFSET-VALUE    : D0-Expr (Quote CLOS-FVAR-OFFSET))

(: specify-representation (Cast6-Lang Config -> Data0-Lang))
(define (specify-representation prgm comp-config)
  (match-let ([(Prog (list name next type) expr) prgm])
    (match-let-values ([(expr next) (run-state ((sr-expr (hash)) expr) next)])
      (Prog (list name next type) expr))))

;; Env must be maintained as a mapping from uids to how to access those
;; values. This is important because uid references to variable inside a
;; closure must turn into memory loads.

(: sr-expr (-> Env (-> C6-Expr (State Nat D0-Expr))))
(define (sr-expr env)
  (: recur* (-> C6-Expr* (State Nat D0-Expr*)))
  (define (recur* e*) (map-state recur e*))
  (: recur (-> C6-Expr (State Nat D0-Expr)))
  (define (recur e)
   (match e
      [(Let b* e)
       (do (bind-state : (State Nat D0-Expr))
           (b* : D0-Bnd*  <- (sr-bnd* env b*))
           (let* ([id* (map (inst car Uid Any) b*)]
                  [env (extend* env id* (map var id*))])
             (e  : D0-Expr  <- ((sr-expr env) e))
             (return-state (Let b* e))))]
      [(If t c a)
       (do (bind-state : (State Nat D0-Expr))
           (t : D0-Expr <- (recur t))
           (c : D0-Expr <- (recur c))
           (a : D0-Expr <- (recur a))
           (return-state (If t c a)))]
      [(App (cons e e^) e*)
       (do (bind-state : (State Nat D0-Expr))
           (e  : D0-Expr  <- (recur  e))
           (e^ : D0-Expr  <- (recur  e^))
           (e* : D0-Expr* <- (recur* e*))
           (return-state
            (if (Code-Label? e)
                (App e (cons e^ e*))
                (App (Op 'Array-ref (list e (Quote CLOS-CODE-INDEX)))
                     (cons e^ e*)))))]
      [(Op p e*)
       (do (bind-state : (State Nat D0-Expr))
           (e* : D0-Expr* <- (recur* e*))
           (return-state (Op p e*)))]
      [(Quote k) (return-state
                  (if (boolean? k)
                      (if k
                          TRUE-IMDT-VALUE
                          FALSE-IMDT-VALUE)
                      (Quote k)))]
     ;; FN Representation
     [(LetP p* (LetC c* e))
       (do (bind-state : (State Nat D0-Expr))
           (p* : D0-Bnd-Code*  <- (sr-bndp* p*))
           (let* ([l*  : Uid* (map (inst car Uid D0-Code) p*)]
                  [env : Env  (extend* env l* (map label l*))]
                  [u*  : Uid* (map (inst car Uid Any) c*)]
                  [env : Env  (extend* env u* (map var u*))])
             (c* : (Pair D0-Bnd* D0-Expr*) <- (sr-bndc* env c*))
             (e  : D0-Expr <- ((sr-expr env) e))
             (return-state (Labels p* (Let (car c*) (Begin (cdr c*) e))))))]
      [(Fn-Caster e)
       (do (bind-state : (State Nat D0-Expr))
           (e : D0-Expr <- (recur e))
           (return-state (Op 'Array-ref (list e CLOS-CSTR-INDEX-VALUE))))]
      [(Var i)  (return-state (lookup env i))]
     ;; Type Representation
      [(Type t) (lambda ([n : Nat]) (sr-type t n))]
      [(Type-Fn-arity e)
       (do (bind-state : (State Nat D0-Expr))
           (e : D0-Expr <- (recur e))
           (return-state (Op 'Array-ref (list e FN-ARITY-INDEX-VALUE))))]
      [(Type-Fn-return e)
       (do (bind-state : (State Nat D0-Expr))
           (e : D0-Expr <- (recur e))
           (return-state (Op 'Array-ref (list e FN-RETURN-INDEX-VALUE))))]
      [(Type-Fn-arg e1 e2)
       (do (bind-state : (State Nat D0-Expr))
           (e1 : D0-Expr <- (recur e1))
           (e2 : D0-Expr <- (recur e2))
           (let ([e2^ (match e2
                        [(Quote (? number? k)) (Quote (+ FN-FMLS-OFFSET k))]
                        [otherwiths (Op '+ (list e2 FN-FMLS-OFFSET-VALUE))])])
           (return-state (Op 'Array-ref (list e1 e2^)))))]
      [(Type-tag e)
       (do (bind-state : (State Nat D0-Expr))
           (e : D0-Expr <- (recur e))
         (return-state (Op 'binary-and (list e TYPE-TAG-MASK-VALUE))))]
      ;; I am using tags for the purpose of constant folding
      ;; This should be replase with deep pattern matching
      ;; (Tag 'Atomic) == (Type-Tag (Int))
      [(Tag  t) (return-state (sr-tag t))]
      ;; Dynamic Values Representation
      [(Dyn-tag e)
       (do (bind-state : (State Nat D0-Expr))
           (e : D0-Expr <- (recur e))
           (return-state (Op 'binary-and (list e DYN-TAG-MASK-VALUE))))]
      [(Dyn-immediate e)
       (do (bind-state : (State Nat D0-Expr))
           (e : D0-Expr <- (recur e))
           (return-state (Op '%>> (list e DYN-IMDT-SHIFT-VALUE))))]
      [(Dyn-make e1 e2)
       (do (bind-state : (State Nat D0-Expr))
           (e1 : D0-Expr <- (recur e1))
           (sr-dyn-make e1 e2 env))]
      [(Dyn-type e)
       (do (bind-state : (State Nat D0-Expr))
           (e : D0-Expr <- (recur e))
           (return-state (Op 'Array-ref (list e DYN-TYPE-INDEX-VALUE))))]
      [(Dyn-value e)
       (do (bind-state : (State Nat D0-Expr))
           (e : D0-Expr <- (recur e))
           (return-state (Op 'Array-ref (list e DYN-VALUE-INDEX-VALUE))))]
      ;; Observable Results Representation
      [(Blame e)
       (do (bind-state : (State Nat D0-Expr))
           (e : D0-Expr <- (recur e))
         (return-state
          (Begin
            (list (Op 'Print (list e))
                  (Op 'Exit  (list (Quote -1))))
            UNDEF-IMDT-VALUE)))]
      [(Observe e t)
       (do (bind-state : (State Nat D0-Expr))
           (e : D0-Expr <- (recur e))
         (lambda ([n : Nat]) : (Values D0-Expr Nat)
                 ;; This is a break in the monad abstraction that needs fixed
                 ;; But I want to remove the Observe abstraction and implement
                 ;; Entirely here and now -- Let's get really observations are
                 ;; not expressions
                 (sr-observe e t n)))]
      ;; References Representation
      [(Begin eff* exp)
       (do (bind-state : (State Nat D0-Expr))
           (eff* : D0-Expr* <- (recur* eff*))
           (exp  : D0-Expr  <- (recur exp))
           (return-state (Begin eff* exp)))]
      ;; Guarded
      [(UGbox e)
       (do (bind-state : (State Nat D0-Expr))
           (e : D0-Expr <- (recur e))
         (alloc-set-ugbox e))]
      [(UGbox-ref e)
       (do (bind-state : (State Nat D0-Expr))
           (e : D0-Expr <- (recur e))
         (return-state (Op 'Array-ref (list e UGBOX-VALUE-INDEX-VALUE))))]
      [(GRep-proxied? e)
       (do (bind-state : (State Nat D0-Expr))
           (e : D0-Expr <- (recur e))
         (return-state (Op 'binary-and (list e GREP-TAG-MASK-VALUE))))]
      [(Gproxy for from to blames)
       (do (bind-state : (State Nat D0-Expr))
           (for     : D0-Expr <- (recur for))
         (from    : D0-Expr <- (recur from))
         (to      : D0-Expr <- (recur to))
         (blames  : D0-Expr <- (recur blames))
         (alloc-tag-set-gproxy for from to blames))]
      [(Gproxy-for e)
       (lift-state (untag-deref-gproxy GPROXY-FOR-INDEX-VALUE) (recur e))]
      [(Gproxy-from e)
       (lift-state (untag-deref-gproxy GPROXY-FROM-INDEX-VALUE) (recur e))]
      [(Gproxy-to e)
       (lift-state (untag-deref-gproxy GPROXY-TO-INDEX-VALUE) (recur e))]
      [(Gproxy-blames e)
       (lift-state (untag-deref-gproxy GPROXY-BLAMES-INDEX-VALUE) (recur e))]))
  recur)

(: untag-deref-gproxy (-> D0-Expr (-> D0-Expr D0-Expr)))
(define (untag-deref-gproxy index-e)
  (lambda ((proxy-e : D0-Expr)) : D0-Expr
   (Op 'Array-ref (list (Op 'binary-xor (list proxy-e GPROXY-TAG-VALUE))
                        index-e))))

(: alloc-tag-set-gproxy
   (D0-Expr D0-Expr D0-Expr D0-Expr . -> . (State Nat D0-Expr)))
(define (alloc-tag-set-gproxy for from to blames)
  (do (bind-state : (State Nat D0-Expr))
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

(: alloc-set-ugbox (D0-Expr . -> . (State Nat D0-Expr)))
(define (alloc-set-ugbox e)
  (do (bind-state : (State Nat D0-Expr))
      (tmp : Uid <- (uid-state "gproxy"))
      (let* ([tmp-var (Var tmp)]
             [alloc   (Op 'Alloc (list UGBOX-SIZE-VALUE))]
             [set     (Op 'Array-set! (list tmp-var UGBOX-VALUE-INDEX-VALUE e))])
        (return-state
         (Let (list (cons tmp alloc))
              (Begin (list set) tmp-var))))))

;; What to do with a binding
(: sr-bnd (-> Env (-> C6-Bnd-Data (State Nat D0-Bnd))))
(define (sr-bnd env)
  (lambda ([b : C6-Bnd-Data]) : (State Nat D0-Bnd)
    (do (bind-state : (State Nat D0-Bnd))
        (e : D0-Expr <- ((sr-expr env) (cdr b)))
        (let ([i : Uid (car b)])
          (return-state (cons i e))))))

;; And the catamorphism for a List of bind
(: sr-bnd* (-> Env C6-Bnd-Data* (State Nat D0-Bnd*)))
(define (sr-bnd* env b*) (map-state (sr-bnd env) b*))

(: sr-type (-> Schml-Type Nat (values D0-Expr Nat)))
(define (sr-type t n)
  (: array-set* (-> D0-Expr Integer (Listof D0-Expr) (Listof D0-Expr)))
  (define (array-set* a i f*)
    (if (null? f*)
        '()
        (cons
         (Op 'Array-set! (list a (Quote i) (car f*)))
         (array-set* a (add1 i) (cdr f*)))))
  (: sr-type* (-> (Listof Schml-Type) Nat (values (Listof D0-Expr) Nat)))
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


;; The way that boxed immediate work currently bothers me.
;; Since we have access to unboxed static ints should we just
;; abandon the unboxed dyn integers another a mixture of static
;; allocation and and constant lifting could be used to make all
(: sr-dyn-make (-> D0-Expr C6-Expr Env (State Nat D0-Expr)))
(define (sr-dyn-make e1 e2 env)
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
     (do (bind-state : (State Nat D0-Expr))
         (e2  : D0-Expr <- ((sr-expr env) e2))
         (sr-alloc "dyn_box" e1 e2))]))

(: sr-observe (-> D0-Expr Schml-Type Nat (values D0-Expr Nat)))
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

;; This is a pretty concise way of performing this but one could imagine looking at
;; only the capture form and build the set and ref forms at once.
;; TODO make the code loop through the closures and build sets and refs all at once

(: sr-bndp* (-> C6-Bnd-Procedure* (State Nat D0-Bnd-Code*)))
(define (sr-bndp* b*) (map-state sr-bndp b*))

(: sr-bndp (-> C6-Bnd-Procedure (State Nat D0-Bnd-Code)))
(define (sr-bndp b)
  (match-let ([(cons u (Procedure cp param* ctr? fvar* exp)) b])
    (let* ([env (for/hash : Env ([u fvar*] [i (in-naturals)])
                  (values u (sr-clos-ref-free (Var cp) i)))]
           [env (extend* env param* (map var param*))])
      (do (bind-state : (State Nat D0-Bnd-Code))
          (exp : D0-Expr <- ((sr-expr env) exp))
          (return-state
           (cons u (Code (cons cp param*) exp)))))))

(: sr-bndc* (-> Env C6-Bnd-Closure* (State Nat (Pair D0-Bnd* D0-Expr*))))
(define (sr-bndc* env b*)
  (do (bind-state : (State Nat (Pair D0-Bnd* D0-Expr*)))
      (b* : (Listof (Pairof D0-Bnd D0-Expr*)) <- (map-state (sr-bndc env) b*))
      (let ([b* (map (inst car D0-Bnd Any) b*)]
            [e* (append-map (inst cdr Any D0-Expr*) b*)])
        (return-state (ann (cons b* e*) (Pair D0-Bnd* D0-Expr*))))))

;; The representation of closures as created by letrec
(: sr-bndc (-> Env (-> C6-Bnd-Closure (State Nat (Pair D0-Bnd D0-Expr*)))))
(define (sr-bndc env)
  ;; Capture/Initialize the values of the closure
  (: mk-set* (-> Uid Uid (Option Uid) Uid* D0-Expr*))
  (define (mk-set* clos code cst? fvar*)
    (let ([clos (lookup env clos)]
          [code (lookup env code)]
          [cst  (if cst? (lookup env cst?) FALSE-IMDT-VALUE)])
      (cons
       (sr-clos-set-code clos code)
       (cons
        (sr-clos-set-caster clos cst)
        (for/list : (Listof D0-Expr) ([fvar : Uid fvar*] [i : Integer (in-naturals)])
          (sr-clos-set-free clos i (lookup env fvar)))))))
  (lambda ([b : C6-Bnd-Closure]) : (State Nat (Pair D0-Bnd D0-Expr*))
    (match-let ([(cons uid (Closure-Data lbl ctr? free*)) b])
      (let ([bnd  : D0-Bnd   (cons uid (sr-clos-alloc uid lbl ctr? free*))]
            [set* : D0-Expr* (mk-set* uid lbl ctr? free*)])
        (return-state (ann (cons bnd set*) (Pair D0-Bnd D0-Expr*)))))))

;; The representation details for closures without recursion
(: sr-clos-alloc (-> Uid Uid (Option Uid) Uid* D0-Expr))
(define (sr-clos-alloc clos code ctr? free*)
  (Op 'Alloc (list (Quote (+ CLOS-FVAR-OFFSET (length free*))))))

(: sr-clos-set-code (-> D0-Expr D0-Expr D0-Expr))
(define (sr-clos-set-code clos code)
  (Op 'Array-set! (list code CLOS-CODE-INDEX-VALUE code)))

(: sr-clos-set-caster (-> D0-Expr D0-Expr D0-Expr))
(define (sr-clos-set-caster clos caster)
  (Op 'Array-set! (list clos CLOS-CSTR-INDEX-VALUE caster)))

(: sr-clos-set-free (-> D0-Expr Integer D0-Expr D0-Expr))
(define (sr-clos-set-free clos index fvar)
  (Op 'Array-set! (list clos (Quote (+ CLOS-FVAR-OFFSET index)) fvar)))

(: sr-clos-ref-code (-> D0-Expr D0-Expr))
(define (sr-clos-ref-code clos)
  (Op 'Array-ref  (list clos CLOS-CODE-INDEX-VALUE)))

(: sr-clos-ref-caster (-> D0-Expr D0-Expr))
(define (sr-clos-ref-caster clos)
  (Op 'Array-ref  (list clos CLOS-CSTR-INDEX-VALUE)))

(: sr-clos-ref-free (-> D0-Expr Integer D0-Expr))
(define (sr-clos-ref-free clos index)
  (Op 'Array-ref (list clos (Quote (+ CLOS-FVAR-OFFSET index)))))

(define-type Env (HashTable Uid D0-Expr))

(define-syntax-rule (extend e k v)
  (hash-set e k v))

(: extend* (-> Env Uid* D0-Expr* Env))
(define (extend* env id* exp*)
  (match* (id* exp*)
    [('() _) env]
    [(_ '()) env]
    [((cons id id*) (cons exp exp*))
     (extend* (extend env id exp) id* exp*)]))

;; The variable is a normal variable
(: var  (-> Uid D0-Expr))
(define (var id) (Var id))

(: label (-> Uid D0-Expr))
(define (label id) (Code-Label id))

(: cref  (-> Uid Natural D0-Expr))
(define (cref clos index)
  (Op 'Array-ref (list (Var clos) (Quote (+ index CLOS-FVAR-OFFSET)))))

(: lookup (-> Env Uid D0-Expr))
(define (lookup e u)
  (hash-ref e u (lambda () (error "Unbound uid in program"))))

(define-type Triv (U (Quote String) (Quote Integer) (Code-Label Uid) (Var Uid)))
(define-predicate triv? Triv)

(: rename (-> String (-> Any String)))
(define (rename name)
  (lambda (_) name))

(: sr-alloc-init (-> D0-Expr (-> D0-Expr D0-Expr D0-Expr)))
(define (sr-alloc-init var)
  (lambda ([n : D0-Expr] [v : D0-Expr]) : D0-Expr
    (Op 'Array-set! (list var n v))))

;; Allocate without forgetting to lift evaluating subterms first
;; this prevents evaluating terms which may cause more allocation
;; will initializing the values of an allocation
(: sr-alloc (->* (String) #:rest D0-Expr (State Nat D0-Expr)))
(define (sr-alloc name . exp*)
  (let ([size (length exp*)])
    (if (= size 0)
        (error "Empty objects can not be allocated")
        (let ([name* (build-list size (rename (string-append name "_init")))])
          (do (bind-state : (State Nat D0-Expr))
              (init-id* : Uid* <- (map-state uid-state name*))
              (box-id   : Uid  <- (uid-state name))
              (let* ([init-expr* (map var init-id*)]
                     [init-index* (map sr-quote (range 0 size))]
                     [set* (map (sr-alloc-init (Var box-id)) init-index* init-expr*)]
                     [bnd-init* (map (inst cons Uid D0-Expr) init-id* exp*)]
                     [bnd-box (cons box-id (Op 'Alloc (list (Quote size))))]
                     [box-var (Var box-id)])
                (return-state
                 (Let bnd-init*
                  (Let (list bnd-box)
                   (Begin set* box-var))))))))))

(define sr-quote : (D0-Literal -> D0-Expr) Quote)
