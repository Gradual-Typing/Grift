#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: src/casts/specify-representation
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
Description: This pass exposes the memory layout of aspects of the program.
After this pass language complexity decreases greatly! But all operations are
exposed as the effects that they truelly are.
+-------------------------------------------------------------------------------+
| Source Grammar : Cast6
| Target Grammar : Data0
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require "../helpers.rkt"
         "../errors.rkt"
         "../language.rkt")
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
      (define DYN-UNIT-TAG-VALUE        : D0-Expr (Quote DYN-UNIT-TAG))
      (define DYN-IMDT-SHIFT-VALUE      : D0-Expr (Quote DYN-IMDT-SHIFT))
      (define DYN-BOX-SIZE-VALUE        : D0-Expr (Quote DYN-BOX-SIZE))
      (define DYN-VALUE-INDEX-VALUE     : D0-Expr (Quote DYN-VALUE-INDEX))
      (define DYN-TYPE-INDEX-VALUE      : D0-Expr (Quote DYN-TYPE-INDEX))
      (define FALSE-IMDT-VALUE          : D0-Expr (Quote FALSE-IMDT))
      (define TRUE-IMDT-VALUE           : D0-Expr (Quote TRUE-IMDT))
      (define UNDEF-IMDT-VALUE          : D0-Expr (Quote UNDEF-IMDT))
      (define UNIT-IMDT-VALUE           : D0-Expr (Quote UNIT-IMDT))
      (define GREP-TAG-MASK-VALUE       : D0-Expr (Quote GREP-TAG-MASK))
      (define UGBOX-SIZE-VALUE          : D0-Expr (Quote UGBOX-SIZE))
      (define UGBOX-VALUE-INDEX-VALUE   : D0-Expr (Quote UGBOX-VALUE-INDEX))
      (define UGVECT-SIZE-INDEX-VALUE   : D0-Expr (Quote UGVECT-SIZE-INDEX))
      (define UGVECT-OFFSET-VALUE       : D0-Expr (Quote UGVECT-OFFSET))
      (define GPROXY-TAG-VALUE          : D0-Expr (Quote GPROXY-TAG))
      (define GPROXY-SIZE-VALUE         : D0-Expr (Quote GPROXY-SIZE))
      (define GPROXY-FOR-INDEX-VALUE    : D0-Expr (Quote GPROXY-FOR-INDEX))
      (define GPROXY-FROM-INDEX-VALUE   : D0-Expr (Quote GPROXY-FROM-INDEX))
      (define GPROXY-TO-INDEX-VALUE     : D0-Expr (Quote GPROXY-TO-INDEX))
      (define GPROXY-BLAMES-INDEX-VALUE : D0-Expr (Quote GPROXY-BLAMES-INDEX))
      (define CLOS-CODE-INDEX-VALUE     : D0-Expr (Quote CLOS-CODE-INDEX))
      (define CLOS-CSTR-INDEX-VALUE     : D0-Expr (Quote CLOS-CSTR-INDEX))
      (define CLOS-FVAR-OFFSET-VALUE    : D0-Expr (Quote CLOS-FVAR-OFFSET))
      (define TYPE-GREF-SIZE-VALUE      : D0-Expr (Quote TYPE-GREF-SIZE))
      (define TYPE-GREF-TAG-VALUE       : D0-Expr (Quote TYPE-GREF-TAG))
      (define GREF-TO-INDEX-VALUE       : D0-Expr (Quote GREF-TO-INDEX))
      (define TYPE-GVECT-SIZE-VALUE     : D0-Expr (Quote TYPE-GVECT-SIZE))
      (define TYPE-GVECT-TAG-VALUE      : D0-Expr (Quote TYPE-GVECT-TAG))
      (define GVECT-TO-INDEX-VALUE      : D0-Expr (Quote GVECT-TO-INDEX))

      (: specify-representation (Cast6-Lang Config -> Data0-Lang))
      (trace-define (specify-representation prgm comp-config)
                    (match-let ([(Prog (list name next type) (LetT tbnd* expr)) prgm])
                      (let ([sr-top-expr (sr-expr (hash) empty-index-map)]
                            [d* (map dec-var ((inst map Uid C/LT-TBnd) car tbnd*))])
                        (let*-values ([(texp* next) (run-state (map-state sr-type tbnd*) next)]
                                      [(expr next) (run-state (sr-top-expr expr) next)])
                          (Prog (list name next type) (Begin (append d* texp*) expr))))))

      (: dec-var (Uid -> D0-Expr))
      (define (dec-var u)
        (GlobalDec u))
      
      ;; Env must be maintained as a mapping from uids to how to access those
      ;; values. This is important because uid references to variable inside a
      ;; closure must turn into memory loads.

      (define-type IndexMap (-> Uid Uid Nat))

      (: index-closure (-> Uid Uid* IndexMap))
      (define (index-closure clos fvar*)
        (define ((fvar-err f))
          (error 'specifiy-representation
                 "failed attempt to index free var ~a from clos ~a"
                 f clos))
        (define (clos-err c f) (error 'specify-representation
                                      "unbound closure index ~a from closure ~a inside of clos ~a"
                                      f c clos))
        (let ([map (for/fold ([index : (HashTable Uid Nat) (hash)])
                             ([fvar : Uid fvar*] [i : Nat (in-naturals CLOS-FVAR-OFFSET)])
                     (hash-set index fvar i))])
          (lambda ([c : Uid] [f : Uid]) : Nat
                  #;(logf "index-closure: ~a ~a ~a\n\ttable: ~a\n" clos c f map)
                  (if (uid=? c clos)
                      (hash-ref map f (fvar-err f))
                      (clos-err c f)))))

      (define (empty-index-map u i)
        (error 'specify-representation "attempt to index without index map ~a ~a" u i))

      (: sr-expr (-> Env IndexMap (-> C6-Expr (State Nat D0-Expr))))
      (define (sr-expr env cenv)
        (: recur* (-> C6-Expr* (State Nat D0-Expr*)))
        (define (recur* e*) (map-state recur e*))
        (: recur (-> C6-Expr (State Nat D0-Expr)))
        (define (recur e)
          (match e
            [(Let b* e)
             (do (bind-state : (State Nat D0-Expr))
                 (b* : D0-Bnd*  <- (sr-bnd* env cenv b*))
               (let* ([id* (map (inst car Uid Any) b*)]
                      [env (extend* env id* (map var id*))])
                 (e  : D0-Expr  <- ((sr-expr env cenv) e))
                 (return-state (Let b* e))))]
            [(If t c a)
             (do (bind-state : (State Nat D0-Expr))
                 (t : D0-Expr <- (recur t))
               (c : D0-Expr <- (recur c))
               (a : D0-Expr <- (recur a))
               (return-state (If t c a)))]
            [(App (cons e e^) e*)
             #;(TODO this is likely broken fix it)
             (do (bind-state : (State Nat D0-Expr))
                 (e  : D0-Expr  <- (recur  e))
               (e^ : D0-Expr  <- (recur  e^))
               (e* : D0-Expr* <- (recur* e*))
               (return-state (App e (cons e^ e*))))]
            [(Op p e*)
             (do (bind-state : (State Nat D0-Expr))
                 (e* : D0-Expr* <- (recur* e*))
               (cond
                 [(schml-prim? p) (return-state (Op p e*))]
                 [(schml-prim!? p) (return-state (Op p e*))]))]
            [(Quote k)
             (return-state
              (cond
                [(null? k)  UNIT-IMDT-VALUE]
                [(boolean? k) (if k TRUE-IMDT-VALUE FALSE-IMDT-VALUE)]
                [(fixnum? k) (Quote k)]
                [(string? k) (Quote k)]
                [else (error 'specify-representation/quote "given ~a" k)]))]
            ;; Closure Representation
            [(LetP p* (LetC c* e))
             (do (bind-state : (State Nat D0-Expr))
                 (p* : D0-Bnd-Code*  <- (sr-bndp* p*))
               (let* ([l*  : Uid* (map (inst car Uid D0-Code) p*)]
                      [env : Env  (extend* env l* (map label l*))]
                      [u*  : Uid* (map (inst car Uid Any) c*)]
                      [env : Env  (extend* env u* (map var u*))])
                 (c* : (Pair D0-Bnd* D0-Expr*) <- (sr-bndc* env cenv c*))
                 (e  : D0-Expr <- ((sr-expr env cenv) e))
                 (return-state (Labels p* (Let (car c*) (Begin (cdr c*) e))))))]
            [(Closure-caster e)
             (do (bind-state : (State Nat D0-Expr))
                 (e : D0-Expr <- (recur e))
               (return-state (Op 'Array-ref (list e CLOS-CSTR-INDEX-VALUE))))]
            [(Closure-code e)
             (do (bind-state : (State Nat D0-Expr))
                 (e : D0-Expr <- (recur e))
               (return-state (Op 'Array-ref (list e CLOS-CODE-INDEX-VALUE))))]
            [(Closure-ref clos fvar)
             (return-state
              (Op 'Array-ref (list (Var clos) (Quote (cenv clos fvar)))))]
            [(Var i)  (return-state (lookup env i))]
            [(Code-Label u) (return-state (Code-Label u))]
            ;; Type Representation
            [(Type t) (sr-prim-type t)]
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
            [(Type-GRef-to e)
             (do (bind-state : (State Nat D0-Expr))
                 (e : D0-Expr <- (recur e))
               (return-state
                (let ([arg : D0-Expr
                           (Op 'binary-xor (list e TYPE-GREF-TAG-VALUE))])
                  (Op 'Array-ref (list arg GREF-TO-INDEX-VALUE)))))]
            [(Type-GVect-to e)
             (do (bind-state : (State Nat D0-Expr))
                 (e : D0-Expr <- (recur e))
               (return-state
                (let ([arg : D0-Expr
                           (Op 'binary-xor (list e TYPE-GVECT-TAG-VALUE))])
                  (Op 'Array-ref (list arg GVECT-TO-INDEX-VALUE)))))]
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
               (sr-dyn-make e1 e2 env cenv))]
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
               (sr-observe e t))]
            ;; References Representation
            [(Begin eff* exp)
             (do (bind-state : (State Nat D0-Expr))
                 (eff* : D0-Expr* <- (recur* eff*))
               (exp  : D0-Expr  <- (recur exp))
               (return-state (Begin eff* exp)))]
            [(Repeat i e1 e2 e3)
             (do (bind-state : (State Nat D0-Expr))
                 (e1 : D0-Expr <- (recur e1))
               (e2 : D0-Expr <- (recur e2))
               (let ([env (extend env i (Var i))])
                 (e3 : D0-Expr <- ((sr-expr env cenv) e3))
                 (return-state (Repeat i e1 e2 e3))))]
            ;; Guarded
            [(UGbox e)
             (do (bind-state : (State Nat D0-Expr))
                 (e : D0-Expr <- (recur e))
               (tmp1 : Uid <- (uid-state "ugbox1"))
               (tmp2 : Uid <- (uid-state "ugbox2"))
               (let* ([tmp1-var (Var tmp1)]
                      [tmp2-var (Var tmp2)]
                      [alloc    (Op 'Alloc (list UGBOX-SIZE-VALUE))]
                      [set      (Op 'Array-set! (list tmp2-var UGBOX-VALUE-INDEX-VALUE tmp1-var))])
                 (return-state
                  (Let (list (cons tmp1 e))
                       (Let (list (cons tmp2 alloc))
                            (Begin (list set) tmp2-var))))))]
            [(UGbox-ref e)
             (do (bind-state : (State Nat D0-Expr))
                 (e : D0-Expr <- (recur e))
               (return-state (Op 'Array-ref (list e UGBOX-VALUE-INDEX-VALUE))))]
            [(UGbox-set! e1 e2)
             (do (bind-state : (State Nat D0-Expr))
                 (e1 : D0-Expr <- (recur e1))
               (e2 : D0-Expr <- (recur e2))
               (return-state (Op 'Array-set! (list e1 UGBOX-VALUE-INDEX-VALUE e2))))]
            [(UGvect n e)
             (do (bind-state : (State Nat D0-Expr))
                 (n : D0-Expr <- (recur n))
               (e : D0-Expr <- (recur e))
               (tmp1 : Uid <- (uid-state "ugvect1"))
               (tmp2 : Uid <- (uid-state "ugvect2"))
               (tmp3 : Uid <- (uid-state "ugvect3"))
               (tmp4 : Uid <- (uid-state "ugvect4"))
               (i    : Uid <- (uid-state "index"))
               (let* ([tmp1-var (Var tmp1)]
                      [tmp2-var (Var tmp2)]
                      [tmp3-var (var tmp3)]
                      [tmp4-var (var tmp4)]
                      [tl       (Op '+ (list tmp1-var UGVECT-OFFSET-VALUE))]
                      [alloc    (Op 'Alloc (list tmp4-var))]
                      [set      (Repeat i UGVECT-OFFSET-VALUE tmp4-var
                                        (Op 'Array-set! (list tmp3-var (Var i) tmp2-var)))]
                      [set-n    (Op 'Array-set! (list tmp3-var UGVECT-SIZE-INDEX-VALUE tmp1-var))])
                 (return-state
                  (Let (list (cons tmp1 n) (cons tmp2 e))
                       (Let (list (cons tmp4 tl))
                            (Let (list (cons tmp3 alloc))
                                 (Begin (list set-n set) tmp3-var)))))))]
            [(UGvect-ref e i)
             (do (bind-state : (State Nat D0-Expr))
                 (e    : D0-Expr <- (recur e))
               (i    : D0-Expr <- (recur i))
               (ind  : Uid <- (uid-state "index"))
               (tmp1 : Uid <- (uid-state "e"))
               (let ([zro (Quote 0)]
                     [tmp1-var (Var tmp1)]
                     [ind-var (Var ind)])
                 (return-state
                  (Let (list (cons ind i) (cons tmp1 e))
                       (If (Op '>= (list ind-var zro)) ;; vectors indices starts from 0
                           (If (Op '< (list ind-var (Op 'Array-ref (list tmp1-var zro))))
                               (Op 'Array-ref (list tmp1-var (Op '+ (list ind-var UGVECT-OFFSET-VALUE))))
                               (Op 'Printf (list (Quote "index out of bound %l\n") ind-var)))
                           (Op 'Printf (list (Quote "index out of bound %l\n") ind-var)))))))]
            [(UGvect-set! e1 i e2)
             (do (bind-state : (State Nat D0-Expr))
                 (e1 : D0-Expr <- (recur e1))
               (i :  D0-Expr <- (recur i))
               (e2 : D0-Expr <- (recur e2))
               (ind  : Uid <- (uid-state "index"))
               (tmp1 : Uid <- (uid-state "ugvect"))
               (let ([zro (Quote 0)]
                     [tmp1-var (Var tmp1)]
                     [ind-var (Var ind)])
                 (return-state
                  (Let (list (cons ind i))
                       (If (Op '>= (list ind-var zro)) ;; vectors indices starts from 0
                           (Let (list (cons tmp1 e1))
                                (If (Op '< (list ind-var (Op 'Array-ref (list tmp1-var zro))))
                                    (Op 'Array-set! (list tmp1-var (Op '+ (list ind-var UGVECT-OFFSET-VALUE)) e2))
                                    (Op 'Printf (list (Quote "index out of bound %l\n") ind-var))))
                           (Op 'Printf (list (Quote "index out of bound %l\n") ind-var)))))))]
            [(GRep-proxied? e)
             (do (bind-state : (State Nat D0-Expr))
                 (e : D0-Expr <- (recur e))
               (return-state
                (Op '= (list GPROXY-TAG-VALUE
                             (Op 'binary-and (list e GREP-TAG-MASK-VALUE))))))]
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
         (D0-Expr D0-Expr D0-Expr D0-Expr -> (State Nat D0-Expr)))
      (define (alloc-tag-set-gproxy for from to blames)
        (do (bind-state : (State Nat D0-Expr))
            (tmpp : Uid <- (uid-state "gproxy"))
          (tmpf : Uid <- (uid-state "for"))
          (tmpm : Uid <- (uid-state "from"))
          (tmpt : Uid <- (uid-state "to"))
          (tmpl : Uid <- (uid-state "blames"))
          (let* ([tmp-var : D0-Expr (Var tmpp)]
                 [alloc   : D0-Expr (Op 'Alloc (list GPROXY-SIZE-VALUE))]
                 [set-for : D0-Expr
                          (Op 'Array-set! (list tmp-var GPROXY-FOR-INDEX-VALUE (Var tmpf)))]
                 [set-from : D0-Expr
                           (Op 'Array-set! (list tmp-var GPROXY-FROM-INDEX-VALUE (Var tmpm)))]
                 [set-to : D0-Expr
                         (Op 'Array-set! (list tmp-var GPROXY-TO-INDEX-VALUE (Var tmpt)))]
                 [set-blames : D0-Expr
                             (Op 'Array-set! (list tmp-var GPROXY-BLAMES-INDEX-VALUE (Var tmpl)))]
                 [bnd-tmps : (Listof (Pairof Uid D0-Expr))
                           (list (cons tmpf for) (cons tmpm from)
                                 (cons tmpt to) (cons tmpl blames))]
                 [set* : (Listof D0-Expr)
                       (list set-for set-from set-to set-blames)]
                 [bnd-alloc : (Listof (Pairof Uid D0-Expr)) (list (cons tmpp alloc))]
                 [tag-proxy : D0-Expr (Op 'binary-or (list tmp-var GPROXY-TAG-VALUE))])
            (return-state
             (Let bnd-tmps (Let bnd-alloc (Begin set* tag-proxy)))))))

      (: sr-prim-type (Prim-Type -> (State Nat D0-Expr)))
      (define (sr-prim-type t)
        (match t
          [(TypeId u) (return-state (Var u))]
          [(Int)  (return-state (Quote TYPE-INT-RT-VALUE))]
          [(Bool) (return-state (Quote TYPE-BOOL-RT-VALUE))]
          [(Dyn)  (return-state (Quote TYPE-DYN-RT-VALUE))]
          [(Unit) (return-state (Quote TYPE-UNIT-RT-VALUE))]))
      
      ;; What to do with a binding
      (: sr-bnd (-> Env IndexMap (-> C6-Bnd-Data (State Nat D0-Bnd))))
      (define (sr-bnd env cenv)
        (lambda ([b : C6-Bnd-Data]) : (State Nat D0-Bnd)
                (do (bind-state : (State Nat D0-Bnd))
                    (e : D0-Expr <- ((sr-expr env cenv) (cdr b)))
                  (let ([i : Uid (car b)])
                    (return-state (cons i e))))))
      
      ;; And the catamorphism for a List of bind
      (: sr-bnd* (-> Env IndexMap C6-Bnd-Data* (State Nat D0-Bnd*)))
      (define (sr-bnd* env cenv b*) (map-state (sr-bnd env cenv) b*))


      (: sr-type (-> C/LT-TBnd (State Nat D0-Expr)))
      (define (sr-type b)
        ;; lays down the data in the third list in continuous sequence
        ;; of memory
        (: array-set* (-> D0-Expr Integer (Listof D0-Expr) (Listof D0-Expr)))
        (define (array-set* a i f*)
          (if (null? f*)
              '()
              (cons
               (Op 'Array-set! (list a (Quote i) (car f*)))
               (array-set* a (add1 i) (cdr f*)))))
        (match-let ([(cons i t) b])
          (match t
            ;; abstract over allocating so that all allocations
            ;; are shorter than this.
            [(GRef t)
             (do (bind-state : (State Nat D0-Expr))
                 (ty : D0-Expr <- (sr-prim-type t))
               (let* ([alloc  : D0-Expr
                              (Op 'Alloc (list TYPE-GREF-SIZE-VALUE))]
                      [gref-init : D0-Expr (Assign i alloc)]
                      [stmt : D0-Expr
                            (Op 'Array-set! (list (Var i) GREF-TO-INDEX-VALUE ty))])
                 (return-state
                  (Begin (list gref-init stmt)
                         (Assign i (Op 'binary-or (list (Var i) TYPE-GREF-TAG-VALUE)))))))]
            ;; TODO: parametrize over the logic of GRef and GVect
            [(GVect t)
             (do (bind-state : (State Nat D0-Expr))
                 (ty : D0-Expr <- (sr-prim-type t))
               (let* ([alloc  : D0-Expr
                              (Op 'Alloc (list TYPE-GVECT-SIZE-VALUE))]
                      [gvect-init : D0-Expr (Assign i alloc)]
                      [stmt   : D0-Expr
                              (Op 'Array-set! (list (Var i) GVECT-TO-INDEX-VALUE ty))])
                 (return-state
                  (Begin (list gvect-init stmt)
                         (Assign i (Op 'binary-or (list (Var i) TYPE-GVECT-TAG-VALUE)))))))]
            [(Fn a f* r)
             (do (bind-state : (State Nat D0-Expr))
                 (f*  : D0-Expr* <- (map-state sr-prim-type f*))
               (r   : D0-Expr  <- (sr-prim-type r))
               (let* ([v (Var i)]
                      [alloc (Op 'Alloc (list (Quote (+ a FN-FMLS-OFFSET))))]
                      [fun-init (Assign i alloc)]
                      [stmt1
                       (Op 'Array-set! (list v (Quote FN-ARITY-INDEX) (Quote a)))]
                      [stmt2
                       (Op 'Array-set! (list v (Quote FN-RETURN-INDEX) r))]
                      [stmt* (array-set* v FN-FMLS-OFFSET f*)])
                 (return-state
                  (Begin (append (list fun-init stmt1 stmt2) stmt*) v))))]
            [else (TODO)])))

      ;; The way that boxed immediate work currently bothers me.
      ;; Since we have access to unboxed static ints should we just
      ;; abandon the unboxed dyn integers another a mixture of static
      ;; allocation and and constant lifting could be used to make all
      (: sr-dyn-make (-> D0-Expr C6-Expr Env IndexMap (State Nat D0-Expr)))
      (define (sr-dyn-make e1 e2 env cenv)
        (match e2
          [(Type (Int))
           (return-state
            (Op '+ (list (Op '%<< (list e1 DYN-IMDT-SHIFT-VALUE))
                         DYN-INT-TAG-VALUE)))]
          [(Type (Bool))
           (return-state
            (Op '+ (list (Op '%<< (list e1 DYN-IMDT-SHIFT-VALUE))
                         DYN-BOOL-TAG-VALUE)))]
          [(Type (Bool))
           (return-state
            (Op '+ (list (Op '%<< (list e1 DYN-IMDT-SHIFT-VALUE))
                         DYN-UNIT-TAG-VALUE)))]
          [otherwise
           (do (bind-state : (State Nat D0-Expr))
               (e2  : D0-Expr <- ((sr-expr env cenv) e2))
             (sr-alloc "dyn_box" e1 e2))]))

      (: sr-observe (-> D0-Expr Schml-Type (State Nat D0-Expr)))
      (define (sr-observe e t)
        (: generate-print (Uid Schml-Type -> D0-Expr))
        (define (generate-print id ty)
          (cond
            [(Int? t) (Op 'Printf (list (Quote "Int : %d\n") (Var id)))]
            [(Unit? t) (Op 'Printf (list (Quote "Unit : ()\n")))]
            [(Bool? t) (If (Op '= (list (Quote TRUE-IMDT) (Var id)))
                           (Op 'Print (list (Quote "Bool : #t\n")))
                           (Op 'Print (list (Quote "Bool : #f\n"))))]
            [(Fn? t) (Op 'Print (list (Quote "Function : ?\n")))]
            [(GRef? t) (Op 'Print (list (Quote "GReference : ?\n")))]
            [(GVect? t) (Op 'Print (list (Quote "GVector : ?\n")))]
            [(Dyn? t) (Op 'Print (list (Quote "Dynamic : ?\n")))]
            [else (TODO)]))
        (do (bind-state : (State Nat D0-Expr))
            (res : Uid <- (uid-state "result"))
          (let ([prt : D0-Expr (generate-print res t)])
            (return-state
             (Let (list (cons res e))
                  (Begin (list prt)
                         (Quote 0)))))))

      #;(TODO GET RID OF TAGS IN THE COMPILER)
      (: sr-tag (Tag-Symbol -> (Quote Integer)))
      (define (sr-tag t)
        (case t
          [(Int)    (Quote DYN-INT-TAG)]
          [(Bool)   (Quote DYN-BOOL-TAG)]
          [(Unit)   (Quote DYN-UNIT-TAG)]
          [(Atomic) (Quote TYPE-ATOMIC-TAG)]
          [(Fn)     (Quote TYPE-FN-TAG)]
          [(GRef)   (Quote TYPE-GREF-TAG)]
          [(GVect)  (Quote TYPE-GVECT-TAG)]
          [(Boxed)  (Quote DYN-BOXED-TAG)]))

      ;; This is a pretty concise way of performing this but one could imagine looking at
      ;; only the capture form and build the set and ref forms at once.
      ;; TODO make the code loop through the closures and build sets and refs all at once

      (: sr-bndp* (-> C6-Bnd-Procedure* (State Nat D0-Bnd-Code*)))
      (define (sr-bndp* b*) (map-state sr-bndp b*))

      (: sr-bndp (-> C6-Bnd-Procedure (State Nat D0-Bnd-Code)))
      (define (sr-bndp b)
        (match-let ([(cons u (Procedure cp param* code ctr? fvar* exp)) b])
          (let* ([env (for/hash : Env ([fvar fvar*] [i (in-naturals 2)])
                        (values fvar (sr-clos-ref-free (Var cp) i)))]
                 [env (extend* env param* (map var param*))]
                 [cenv (index-closure cp fvar*)])
            (do (bind-state : (State Nat D0-Bnd-Code))
                (exp : D0-Expr <- ((sr-expr env cenv) exp))
              (return-state
               (cons u (Code (cons cp param*) exp)))))))

      (: sr-bndc* (-> Env IndexMap C6-Bnd-Closure* (State Nat (Pair D0-Bnd* D0-Expr*))))
      (define (sr-bndc* env cenv b*)
        (do (bind-state : (State Nat (Pair D0-Bnd* D0-Expr*)))
            (b* : (Listof (Pairof D0-Bnd D0-Expr*)) <- (map-state (sr-bndc env cenv) b*))
          (let ([b* (map (inst car D0-Bnd Any) b*)]
                [e* (append-map (inst cdr Any D0-Expr*) b*)])
            (return-state (ann (cons b* e*) (Pair D0-Bnd* D0-Expr*))))))

      ;; The representation of closures as created by letrec
      (: sr-bndc (-> Env IndexMap (-> C6-Bnd-Closure (State Nat (Pair D0-Bnd D0-Expr*)))))
      (define (sr-bndc env cenv)
        ;; Capture/Initialize the values of the closure
        (: mk-set* (-> D0-Expr D0-Expr D0-Expr D0-Expr* D0-Expr*))
        (define (mk-set* clos code cst fvar*)
          (cons
           (sr-clos-set-code clos code)
           (cons
            (sr-clos-set-caster clos cst)
            (for/list : (Listof D0-Expr) ([fvar : D0-Expr fvar*] [i : Integer (in-naturals)])
              (sr-clos-set-free clos i fvar)))))
        (lambda ([b : C6-Bnd-Closure]) : (State Nat (Pair D0-Bnd D0-Expr*))
                (match-let ([(cons uid (Closure-Data lbl ctr free*)) b])
                  (do (bind-state : (State Nat (Pair D0-Bnd D0-Expr*)))
                      (lbl : D0-Expr  <- ((sr-expr env cenv) lbl))
                    (ctr : D0-Expr  <- ((sr-expr env cenv) ctr))
                    (fr* : D0-Expr* <- (map-state (sr-expr env cenv) free*))
                    (let ([bnd  : D0-Bnd   (cons uid (sr-clos-alloc fr*))]
                          [set* : D0-Expr* (mk-set* (Var uid) lbl ctr fr*)])
                      (return-state (ann (cons bnd set*) (Pair D0-Bnd D0-Expr*))))))))

      ;; The representation details for closures without recursion
      (: sr-clos-alloc (-> (Listof Any) D0-Expr))
      (define (sr-clos-alloc free*)
        (Op 'Alloc (list (Quote (+ CLOS-FVAR-OFFSET (length free*))))))

      (: sr-clos-set-code (-> D0-Expr D0-Expr D0-Expr))
      (define (sr-clos-set-code clos code)
        (Op 'Array-set! (list clos CLOS-CODE-INDEX-VALUE code)))

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
        (hash-ref e u (lookup-error e u)))

      (define (lookup-error e u)
        (lambda ()
          (error 'specify-representation/lookup
                 "Unbound uid ~a\n\tin program with env ~a" u e)))

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
