#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: compiler/casts/interpret-casts                                           |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
|Discription:                                                                   |
|                                                                               |
+-------------------------------------------------------------------------------+
|Input Grammar Cast Language 0                                                  |
|Prog   = (Prog File-Name Next-Uid-Suffix Type Expr)                            |                               
|Expr   = (Lambda {(Fml Uid Type)}* {Type} {Expr})                              |
|       | (Var {Uvar})                                                          |
|       | (App {Expr} {Expr}*)                                                  |
|       | (Op Prim {Expr}*)                                                     |
|       | (Cast {Expr} {Type} {Type} {String})                                  |
|       | (If {Expr} {Expr} {Expr})                                             |
|       | (Let {(Bnd Uid Expr)}* {Expr})                                        |
|       | (Letrec {(Bnd Uid Expr)}* {Expr})                                     |
|       | (Quote {Literal})                                                     |
|Uid    = A Unique Identifier                                                   |
|Literal= Fixnums and Booleans                                                  |
|Prim   = op:fix:* | op:fix:+ | op:fix:- | op:fix:and | op:fix:or | op:fix:>>   |
|       | op:fix:<< | relop:fix:< | relop:fix:<= | relop:fix:=  | relop:fix:>=  |
|       | relop:fix:>                                                           |
|Type   = Fix | Bool | Dyn | ({Type}* -> {Type})                                |
+-------------------------------------------------------------------------------+
|Output Grammar                                                                 |
|                                                                               |
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require Schml/framework/build-compiler
         Schml/framework/helpers
         Schml/framework/errors
	 Schml/compiler/language)

;; Only the pass is provided by this module
(provide interpret-casts)

(define INTERP-TYPE
  (Fn 4 (list ANY-VALUE ANY-TYPE ANY-TYPE STRING-PTR) ANY-VALUE))

(: interpret-casts (Cast1-Lang Config . -> . Lambda0-Lang))
(define (interpret-casts prgm config)
  (match-let ([(Prog (list name next type) exp) prgm])
    (let*-values ([(exp next) (mk-cast-interp exp type next)])
      (Prog (list name next type) exp))))

(: mk-cast-interp (C1-Expr Cast-Type Natural . -> . (values L0-Expr Natural)))
(define (mk-cast-interp exp type next)
  (let*-values ([(interp-uid next) (next-uid "interp_cast" next)]
		[(interp-var) (Var interp-uid)]
		[(interp-fn next) (finalize-interp-code interp-var next)]
		[(exp next) ((ic-expr (specialize-cast (interp-cast interp-var))) 
			     exp next)])
    (values (Letrec (list (cons interp-uid interp-fn)) exp) next)))


(: finalize-interp-code (L0-Expr Natural . -> . (values L0-Expr Natural)))
(define (finalize-interp-code interp-var next)
  (let*-values ([(v next)  (next-uid "val" next)]     
		[(v2 next) (next-uid "val" next)]     
		[(t1 next) (next-uid "type1" next)]   
		[(t2 next) (next-uid "type1" next)]   
		[(l next) (next-uid "label" next)]) 
    (let ([v-var   (Var v)]
	  [v2-var  (Var v2)]  
	  [t1-var  (Var t1)]  
	  [t2-var  (Var t2)]  
	  [l-var   (Var l)]
	  [recur   (interp-cast interp-var)])
      ;; I was trying to get the full decision tree without the
      ;; recursion but I think this piece of code will diverge
      ;; this might be fixible by putting a condition similar to
      ;; the cast-specializer
      ;; (letrec ([cast : Cast-Rule 
      ;; 	   (lambda (v t1 t2 l n)
      ;; 	    ((cast-Any-Type->Any-Type recur) v t1 t2 l n))]))
      
      ;; This cast purposfully builds the entire decision tree
      ;; with the exception that dyn -> Fn causes a recursive
      ;; call to occur
      (let-values ([(body next) 
		    (cast-Any-Type->Any-Type v-var t1-var t2-var l-var next)])
	(values (Lambda (list v t1 t2 l) #f (Castable #f body)) next)))))

;; A Cast rule is part of the decision tree for allowed versus
;; not allowed casts. They use a few macros that keep invariants
;; managable and allow literals to prune the tree to only possible
;; needed branches
(define-type Cast-Rule (-> L0-Expr L0-Expr L0-Expr L0-Expr Natural
			   (values L0-Expr Natural)))

;; Meta Cast Rules are Parameterized by another cast
;; rule to control their behavior.
(define-type Meta-Cast-Rule (-> Cast-Rule Cast-Rule))

;; I don't actually use this type anywhere but variables
;; and literals are what I consider terminal.
(define-type Terminal (U (Quote Cast-Literal) (Var Uid)))

;; This macro allows any literals to affect the shape
;; of the decision tree perhaps a smater way of doing this
;; in the future may be to keep an environment of variables
;; around which would let us to specialize some of the dynamic
;; checks and more of the checks where a type variable is known
;; It would also be able to unfold to quanitize the amount of
;; folding that we allow to happen.

(define-syntax-rule (dont-specialize next op? v c a)
  (let*-values ([(exp-c next) c]
		[(exp-a next) a])
    (values (If (Op op? (list v)) exp-c exp-a) next)))

(define-syntax-rule (specialize next p? op? v c a)
  (if (Quote? v) 
      (if (p? (Quote-literal v)) c a)
      (dont-specialize next op? v c a)))

(define-syntax if/spec
  (syntax-rules (Type:Int? Type:Bool? Type:Fn? Type:Dyn?)
    [(_ next (Type:Int? v) c a) (specialize next Int? 'Type:Int? v c a)]
    [(_ next (Type:Bool? v) c a) (specialize next Bool? 'Type:Bool? v c a)]
    [(_ next (Type:Dyn? v) c a) (specialize next Dyn? 'Type:Dyn? v c a)]
    [(_ next (Type:Fn? v) c a) (specialize next Fn? 'Type:Fn? v c a)]
    [(_ next (op? v) c a) (dont-specialize next 'op? v c a)]))

;; This ensures that cast primitives are only performed on terminals
;; this is a fairly mindless way of ensuring that we are not recomputing
;; values and empty lets will be folded away in a few passes
(define-syntax-rule (bind-non-terminal v b* next)
  (if (or (Quote? v) (Var? v)) 
      (values b* v next)
      (let-values ([(uid next) (next-uid "cast_tmp" next)])
	(values (cons (cons uid v) b*) (Var uid) next))))
(define-syntax-rule (with-terminals-only (v t1 t2 lbl next) body) 
  (let*-values ([(b* v next) (bind-non-terminal v '() next)]
		[(b* t1 next) (bind-non-terminal t1 b* next)]
		[(b* t2 next) (bind-non-terminal t2 b* next)]
		[(b* lbl next) (bind-non-terminal lbl b* next)])
    (if (null? b*)
	body
	(let-values ([(tmp next) body])
	  (values (Let b* tmp) next)))))




;; The following are the two meta cast rules that are used
(: cast-Any-Type->Any-Type Cast-Rule)
(define (cast-Any-Type->Any-Type v t1 t2 l next)
  (with-terminals-only
   (v t1 t2 l next)
   (if/spec next (Type:Int? t1)
      (cast-Int->Any-Type v t1 t2 l next)
      (if/spec next (Type:Bool? t1)
	 (cast-Bool->Any-Type v t1 t2 l next)
	 (if/spec next (Type:Dyn? t1)
	    (cast-Dyn->Any-Type v t1 t2 l next)
	    (if/spec next (Type:Fn? t1)
	       (cast-Fn->Any-Type v t1 t2 l next)
	       (values (Op 'Blame (list l)) next)))))))

(: cast-Dyn->Any-Type Cast-Rule)
(define (cast-Dyn->Any-Type v t1 t2 lbl next)
  (with-terminals-only
     (v t1 t2 lbl next)
     (if/spec next (Type:Dyn? t2)
	 (values v next)
	 (let ([blame-lbl (Op 'Blame (list lbl))])
	   (if/spec next (Type:Int? t2)
	      (if/spec next (Dyn:Int? v)
		 (values (Op 'Dyn:Int-value (list v)) next)
		 (values blame-lbl next))
	      (if/spec next (Type:Bool? t2)
	         (if/spec next (Dyn:Bool? v)
		    (values (Op 'Dyn:Bool-value (list v)) next)
		    (values blame-lbl next))
		 (if/spec next (Type:Fn? t2)
		    (if/spec next (Dyn:Fn? v)
		       (cast-Fn->Any-Type (Op 'Dyn:Fn-value (list v)) 
					  (Op 'Dyn:Fn-type (list v)) 
					  t2 lbl next)
		       (values blame-lbl next))
		    (values blame-lbl next))))))))

;; Really this only decides weather or not to make a specialized cast
(: specialize-cast (-> Cast-Rule Cast-Rule))
(define (specialize-cast interp-cast)
  (: specialize-cast-closure Cast-Rule)
  (define (specialize-cast-closure v t1 t2 l next)
    (with-terminals-only
     (v t1 t2 l next)
     (if (or (Quote? t1) (Quote? t2)) 
	 ;; Some of the code may be specialized away
	 (cast-Any-Type->Any-Type v t1 t2 l next)
	 ;; Then we would end up with a full cast tree if it didn't call
	 ;; the cast interpreter
	 (interp-cast v t1 t2 l next))))
  specialize-cast-closure)

;; These are all the Cast Rules
(: interp-cast (-> L0-Expr Cast-Rule))
(define (interp-cast interp-var)
  (: help Cast-Rule)
  (define (help v t1 t2 l next)
    (values (App interp-var (list v t1 t2 l)) next))
  help)

(: cast-Int->Dyn Cast-Rule)
(define (cast-Int->Dyn v t1 t2 l next)
  (values (Op 'Dyn:Int-make (list v)) next))

(: cast-Bool->Dyn Cast-Rule)
(define (cast-Bool->Dyn v t1 t2 l next) 
  (values (Op 'Dyn:Bool-make (list v)) next))

(: cast-Fn->Dyn Cast-Rule)
(define (cast-Fn->Dyn v t1 t2 l next)
  (values (Op 'Dyn:Fn-make (list v t1)) next))


(: cast-Int->Any-Type Cast-Rule)
(define (cast-Int->Any-Type v t1 t2 l next)
  (with-terminals-only 
   (v t1 t2 l next)
   (if/spec next (Type:Int? t2)
	    (values v next)
	    (if/spec next (Type:Dyn? t2)
		     (cast-Int->Dyn v t1 t2 l next)
		     (values (Op 'Blame (list l)) next)))))

(: cast-Bool->Any-Type Cast-Rule)
(define (cast-Bool->Any-Type v t1 t2 lbl next)
  (with-terminals-only
   (v t1 t2 lbl next)
   (if/spec next (Type:Bool? t2)
	    (values v next)
	    (if/spec next (Type:Dyn? t2)
		     (cast-Bool->Dyn v t1 t2 lbl next)
		     (values (Op 'Blame (list lbl)) next)))))

(: cast-Fn->Any-Type Cast-Rule)
(define (cast-Fn->Any-Type v t1 t2 lbl next)
  (with-terminals-only
   (v t1 t2 lbl next)
   (if/spec next (Type:Dyn? t2)
	    (cast-Fn->Dyn v t1 t2 lbl next)
	    (if/spec next (Type:Fn? t2)
		     (values (App (Op 'Fn-cast (list v)) (list v t1 t2 lbl))
			     next)
		     (values (Op 'Blame (list lbl)) next)))))

;; This is the catamorphism over expression and with the removal
;; of annotations formal and Binding structs

(: ic-expr (-> Cast-Rule (-> C1-Expr Natural (values L0-Expr Natural))))
(define (ic-expr cast)
  (: recur (-> C1-Expr Natural (values L0-Expr Natural)))
  (define (recur exp next)
    (: recur/next (-> C1-Expr (values L0-Expr Natural)))
    (define (recur/next exp) (recur exp next))
    (match exp 
      [(Lambda f* t (Castable ctr (app recur/next exp next))) 
       (values 
	(Lambda (map (inst Fml-identifier Uid Cast-Type) f*)
		#f
		(Castable ctr exp))
	next)]
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
      [(Cast (app recur/next e next) t1 t2 l)
       (let*-values ([(t1 next) (recur t1 next)]
		     [(t2 next) (recur t2 next)]
		     [(l next) (recur l next)])
	 (cast e t1 t2 l next))] 
      [(If (app recur/next tst next) csq alt)
       (let*-values ([(csq next) (recur csq next)]
		     [(alt next) (recur alt next)])
	 (values (If tst csq alt) next))]
      [(Var i) (values (Var i) next)]
      [(Quote k) (values (Quote k) next)]))
  (: recur* (-> (Listof C1-Expr) Natural (values (Listof L0-Expr) Natural)))
  (define (recur* e* n) 
    (if (null? e*)
	(values '() n)
	(let*-values ([(e) (car e*)]
		      [(e* n) (recur* (cdr e*) n)]
		      [(e n) (recur e n)])
	  (values (cons e e*) n))))
  (: cata-bnd* (-> C1-Bnd* Natural (values L0-Bnd* Natural)))
  (define (cata-bnd* b* n) 
    (if (null? b*)
	(values '() n)
	(match-let ([(Bnd i t r) (car b*)])
	  (let*-values ([(b* n) (cata-bnd* (cdr b*) n)]
			[(r n) (recur r n)])
	    (values (cons (cons i r) b*) n)))))
  recur)





    

