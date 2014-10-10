#lang typed/racket
#|-----------------------------------------------------------------------------+
|Pass: compiler/casts/interpret-casts                                          |
+------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                             |
+------------------------------------------------------------------------------+
|Discription:
 -This pass creates a cast-interpreter that can cast arbitrary values at 
  runtime.
 -Coverts runtime casts to applications of the cast-interpreter.
 -Specializes casts with known types, at this point represented with the cast 
  form, to the shortest branch of the cast tree that is relevant.
 -Replaces types that are structure with a psuedo constructor call
 -Replaces tags atomic types with the Type form.
 -Introduces the short lived dynamic type manipulation forms
+------------------------------------------------------------------------------+
|Input Grammar Cast1-Language                                                  |
|Output Grammar Cast2-Language                                                 |
+-----------------------------------------------------------------------------|#
;; The define-pass syntax
(require Schml/framework/build-compiler
         Schml/framework/helpers
         Schml/framework/errors
	 Schml/compiler/language)

;; Only the pass is provided by this module
(provide interpret-casts)

(: interpret-casts (Cast1-Lang Config . -> . Cast2-Lang))
(define (interpret-casts prgm config)
  (match-let ([(Prog (list name next type) exp) prgm])
    (let*-values ([(interp-cast interp-bnd next) (mk-cast-interp next)]
                  [(exp next) ((ic-expr interp-cast) exp next)]
                  [(prog-uid next) (next-uid "prog_returns" next)]
                  [(prog-bnd) (cons prog-uid exp)]
                  [(prog-var) (Var prog-uid)])
      (Prog (list name next type) 
       (Letrec interp-bnd
        (Let (list prog-bnd)
         (Observe prog-var type)))))))

(: mk-cast-interp (Natural . -> . (values Cast-Rule C2-Bnd* Natural)))
(define (mk-cast-interp next)
  (let*-values ([(interp-uid next) (next-uid "interp_cast" next)]
		[(interp-var) (Var interp-uid)]
		[(interp-fn next) (finalize-interp-code interp-var next)])
    (values (interp-cast interp-var) (list (cons interp-uid interp-fn)) next)))


(: finalize-interp-code (C2-Expr Natural . -> . (values C2-Expr Natural)))
(define (finalize-interp-code interp-cast next)
  (let*-values ([(v next)  (next-uid "val" next)]     
		[(v2 next) (next-uid "val" next)]     
		[(t1 next) (next-uid "type1" next)]   
		[(t2 next) (next-uid "type2" next)]   
		[(l next)  (next-uid "label" next)]) 
    (let ([v-var   (Var v)]
	  [v2-var  (Var v2)]  
	  [t1-var  (Var t1)]  
	  [t2-var  (Var t2)]  
	  [l-var   (Var l)])
      (let-values ([(body next) (cast-Any-Type->Any-Type v-var t1-var t2-var l-var next)])
	(values (Lambda (list v t1 t2 l) #f (Castable #f body)) next)))))

(: ic-expr (-> Cast-Rule (-> C1-Expr Natural (values C2-Expr Natural))))
(define (ic-expr interp-cast)
  (: recur (-> C1-Expr Natural (values C2-Expr Natural)))
  (define (recur exp next)
    (: recur/next (-> C1-Expr (values C2-Expr Natural)))
    (define (recur/next exp) (recur exp next))
    (match exp 
      [(Lambda f* t (Castable ctr (app recur/next exp next))) 
       (values (Lambda f* #f (Castable ctr exp)) next)]
      [(Letrec b* (app recur/next exp next)) 
       (let-values ([(b* next) (cata-bnd* b* next)])
	 (values (Letrec b* exp) next))]
      [(Let b* (app recur/next exp next))
       (let-values ([(b* next) (cata-bnd* b* next)])
	 (values (Let b* exp) next))]
      [(App (app recur/next exp next) exp*) 
       (let-values ([(exp* next) (recur* exp* next)])
	 (values (App exp exp*) next))]
      [(Op p exp*) 
       (let-values ([(exp* next) (recur* exp* next)])
	 (values (Op p exp*) next))]
      [(Runtime-Cast (app recur/next v next) t1 t2 l)
       (let*-values ([(t1 next) (recur t1 next)]
		     [(t2 next) (recur t2 next)]
		     [(l next) (recur l next)])
	 (interp-cast v t1 t2 l next))]
      [(Cast (app recur/next e next) t1 t2 l)
       ((specialize-cast interp-cast) e (Type t1) (Type t2) (Quote l) next)]
      [(Fn-Cast (app recur/next e next) t1 t2 l)
       (cast-Fn->Fn e (Type t1) (Type t2) (Quote l) next)]
      [(Type-Fn-ref (app recur/next e next) s)
       (values (Type-Fn-ref e s) next)]
      [(Blame (app recur/next e next)) (values (Blame e) next)]
      [(If (app recur/next tst next) csq alt)
       (let*-values ([(csq next) (recur csq next)]
		     [(alt next) (recur alt next)])
	 (values (If tst csq alt) next))]
      [(Var i) (values (Var i) next)]
      [(Quote k) (values (Quote k) next)]))
  
  (: recur* (-> (Listof C1-Expr) Natural (values (Listof C2-Expr) Natural)))
  (define (recur* e* n) 
    (if (null? e*)
	(values '() n)
	(let*-values ([(e) (car e*)]
		      [(e* n) (recur* (cdr e*) n)]
		      [(e n) (recur e n)])
	  (values (cons e e*) n))))
  (: cata-bnd* (-> C1-Bnd* Natural (values C2-Bnd* Natural)))
  (define (cata-bnd* b* n) 
    (if (null? b*)
	(values '() n)
	(match-let ([(cons i r) (car b*)])
	  (let*-values ([(b* n) (cata-bnd* (cdr b*) n)]
			[(r n) (recur r n)])
	    (values (cons (cons i r) b*) n)))))
  recur)

;; A Cast rule is part of the decision tree for allowed versus
;; not allowed casts. They use a few macros that keep invariants
;; managable and allow literals to prune the tree to only possible
;; needed branches
(define-type Cast-Rule (-> C2-Expr C2-Expr C2-Expr C2-Expr Natural
			   (values C2-Expr Natural)))

;; Build a custom cast decision tree
(: specialize-cast (-> Cast-Rule Cast-Rule))

(define (specialize-cast interp-cast)
  (lambda (v t1 t2 l next)
    (if #f
        (cast-Any-Type->Any-Type v t1 t2 l next)
        (interp-cast v t1 t2 l next))))

;; Invoke the prebuild tree as a function
(: interp-cast (-> C2-Expr Cast-Rule))
(define (interp-cast interp-var)
  (: help Cast-Rule)
  (define (help v t1 t2 l next)
    (values (App interp-var (list v t1 t2 l)) next))
  help)

;;These functions type to fold predicates in order to allow ifs to
;;generate only checks that are actually needed
(: type-tag (-> C2-Expr C2-Expr))
(define (type-tag o)
  (if (Type? o)
      (let ([v (Type-type o)])
        (cond
         [(or (Dyn? v)
              (Int? v)
              (Bool? v)) 
          (Tag 'Atomic)]
         [(Fn? v) (Tag 'Fn)]
         [else (error 'type-tag)])) 
      (Type-tag o)))

;; performs compile time folding of prim = on literals
(: op=? (-> C2-Expr C2-Expr C2-Expr))
(define (op=? o x)
  (if (or (and (Quote? o) (Quote? x)
               (eq? (Quote-literal o)
                    (Quote-literal x)))
          (and (Tag? o) (Tag? x)
               (eq? (Tag-bits o) 
                    (Tag-bits x)))
          (and (Type? o) (Type? x)
               (equal? (Type-type o) 
                       (Type-type x))))
      (Quote #t)
      (Op '= (list o x))))

;; construct new type literals based on the fn-ref operation 
(: fn-ref (-> C2-Expr (U Index 'return 'arity) C2-Expr))
(define (fn-ref t i)
  (if (Type? t)
      (let ((t (Type-type t)))
        (if (Fn? t)
            (cond
             [(eq? i 'return) (Type (Fn-ret t))]
             [(eq? i 'arity)  (Quote (Fn-arity t))]
             [else (Type ((inst list-ref Schml-Type) (Fn-fmls t) i))])
            (error 'fn-ref "generated a function ref for non function type")))
      (Type-Fn-ref t i)))

;; if$ and cond$ will prune branches if the condition is (Quote #t) or (Quote #f)
(define-syntax if$
  (lambda (stx)
    (syntax-case stx ()
      [(_ t c a)
       (begin ;(print-syntax-width +inf.0) (print stx) (newline)
         #'(let-values  ([(tmp-t) t])
           (if (Quote? tmp-t)
               (if (Quote-literal tmp-t)
                   c
                   a)
               (let*-values ([(tmp-c n) c]
                             [(tmp-a n) a])
                 (values (If tmp-t tmp-c tmp-a) n)))))])))

(define-syntax cond$
  (syntax-rules (else)
    [(_ (else c)) c]
    [(_ (t c) (t* c*) ...)
     (if$ t c (cond$ (t* c*) ...))]))

;; make sure that t is a terminal expression
;; expects to be used in the store passing style expressions
(define-syntax let$*
  (syntax-rules (bind)
    [(_ () (bind (n) b)) b]
    [(_ ([t v] [t* v*] ...) (bind (n) b))
     (let ((tmp : C2-Expr v))
       (if (or (Quote? tmp) (Tag? tmp) (Type? tmp) (Var? tmp))
           ;;if the expression is a terminal then just update the binding 
           (let ((t : C2-Expr tmp)) (let$* ([t* v*] ...) (bind (n) b)))
           ;;if the expression is non terminal bind a var and bind the
           ;;expression to the var
           (let-values ([(uid n) (next-uid (~a 't) n)])
             (let ([t : C2-Expr (Var uid)])
               (let-values ([(body n) (let$* ([t* v*] ...) (bind (n) b))])
                 (values (Let (list (cons uid tmp)) body) n))))))]))

(: cast-Any-Type->Any-Type Cast-Rule)
(define (cast-Any-Type->Any-Type v t1 t2 lbl next)
  (let$* ([val v] 
          [type1 t1])
   (bind (next)
    (if$   (op=? type1 (Type DYN-TYPE))
           (cast-Dyn->Any-Type val type1 t2 lbl next)
           (cast-Ground-Type->Any-Type val type1 t2 lbl next)))))


(: cast-Ground-Type->Any-Type Cast-Rule)
(define (cast-Ground-Type->Any-Type v t1 t2 lbl next)
  (let$* ([type1 t1] 
          [tag1 (type-tag type1)])
    (bind (next)
     (cond$ 
      [(op=? (Tag 'Fn) tag1) (cast-Fn->Any-Type v type1 t2 lbl next)]
      [(op=? type1 (Type INT-TYPE)) (cast-Int->Any-Type v type1 t2 lbl next)]
      [(op=? type1 (Type BOOL-TYPE)) (cast-Bool->Any-Type v type1 t2 lbl next)]
      [else (values (Blame lbl) next)]))))

(: cast-Dyn->Any-Type Cast-Rule)
(define (cast-Dyn->Any-Type v t1 t2 lbl next)
  (let$* ([val v]
          [type2 t2])
   (bind (next)
    (if$ (op=? (Type DYN-TYPE) type2)
         (values v next)
         (let$* ([val v]
                 [dyn-tag (Dyn-tag val)])
           (bind (next)
            (cond$
             [(op=? (Tag 'Int) dyn-tag)
               (cast-Ground-Type->Any-Type (Dyn-immediate val) (Type INT-TYPE) type2 lbl next)]
              [(op=? (Tag 'Bool) dyn-tag)
               (cast-Ground-Type->Any-Type (Dyn-immediate val) (Type BOOL-TYPE) type2 lbl next)]
              [(op=? (Tag 'Boxed) dyn-tag)
               (cast-Ground-Type->Any-Type (Dyn-ref val 'value) 
                                           (Dyn-ref val 'type) 
                                           type2 lbl next)]
              [else (values (Blame lbl) next)])))))))


(: cast-Int->Any-Type Cast-Rule)
(define (cast-Int->Any-Type v t1 t2 lbl next)
  (let$* ([type2 t2])
    (bind (next)
     (cond$ 
      [(op=? (Type INT-TYPE) type2) (values v next)]
      [(op=? (Type DYN-TYPE) type2) (values (Dyn-make v (Type INT-TYPE)) next)]
      [else (values (Blame lbl) next)]))))

(: cast-Bool->Any-Type Cast-Rule)
(define (cast-Bool->Any-Type v t1 t2 lbl next)
  (let$* ([type2 t2])
    (bind (next)
      (cond$ 
       [(op=? (Type BOOL-TYPE) type2) (values v next)]
       [(op=? (Type DYN-TYPE) type2) (values (Dyn-make v (Type BOOL-TYPE)) next)]
       [else (values (Blame lbl) next)]))))

(: cast-Fn->Any-Type Cast-Rule)
(define (cast-Fn->Any-Type v t1 t2 lbl next)
  (let$* ([type2 t2])
    (bind (next)
      (if$   (op=? (Type DYN-TYPE) type2)
             (values (Dyn-make v t1) next)
             (cast-Fn->Fn v t1 type2 lbl next)))))

(: cast-Fn->Fn Cast-Rule)
(define (cast-Fn->Fn v t1 t2 lbl next)
  (let$* ([type2 t2]
          [tag2 (type-tag type2)])
   (bind (next)
   (if$   (op=? tag2 (Tag 'Fn))
           (let$* ([value v])
            (bind (next)
               (values (App (Fn-Caster value) (list value t1 type2 lbl)) next)))
           (values (Blame lbl) next)))))
