#lang racket

(require Schml/framework/pprint
         Schml/framework/helpers
         racket/generic)

;; Core Language Types
(struct Type ())
(struct Dyn Type ())
(struct Bool Type ())
(struct Int Type ())
(struct Function Type (from to) #:transparent)

;; Fly Weight the types keeps us from using heap space for no reason
(define Bool-Type (Bool))
(define Int-Type (Int))
(define Dyn-Type (Dyn))

(define/match (type? x)
  [((Dyn)) #t]
  [((Bool)) #t]
  [((Int)) #t]
  [((Function (list (? type?) ...) (? type?))) #t]
  [(otherwise) #f])

(define/match (type->doc t)
  [((Dyn)) (text "Dyn")]
  [((Int)) (text "Int")]
  [((Bool)) (text "Bool")]
  [((Function (list (app type->doc f) ...) (app type->doc t)))
   (doc-list (hs-append (hs-concat f) (text "->") t))]
  [(o) (error 'type->doc "Given non type ~a" o)])

(define (consistent? t g)
  (or (Dyn? t) (Dyn? g)
      (and (Int? t) (Int? g))
      (and (Bool? t) (Bool? g))
      (function-consistent? t g)))

(define (function-consistent? t g)
  (and (Function? t) (Function? g)
       (andmap/length= consistent? (Function-from t) (Function-from g))
       (consistent? (Function-to t) (Function-to g))))

(define (shallow-consistent? t g)
  (or (Dyn? t) (Dyn? g)
      (and (Int? t) (Int? g))
      (and (Bool? t) (Bool? g))
      (and (Function? t) (Function? g))))

;; Join :: type X type -> type
;; This is the join of the least precise latice
;;     ⊑ latice example:
;;      Int --> Int
;;         /   \
;;        /     \               
;;       /       \        Joins ↑
;;Dyn --> Int Int --> Dyn    
;;       \       /        Meets ↓
;;        \     /  
;;         \   /
;;      Dyn --> Dyn
;;           |
;;          Dyn

(define (join t g)
  (cond
    [(Dyn? t) g]
    [(Dyn? g) t]
    [(and (Int? t) (Int? g)) Int-Type]
    [(and (Bool? t) (Bool? g)) Bool-Type]
    [(and (Function? t) (Function? g))
     (Function (map join (Function-from t) (Function-from g))
               (join (Function-to t) (Function-to g)))]
    [else (error 'join "Types are not consistent")]))

;; Not that allocation takes up to much space but you only ever need
;; one of each of these types because they have no fields

;; Literal Constants
(define (int? x)
  (and (integer? x)
       (>= x 0)
       (<= x (expt 2 64))))

(define (constant? x)
  (or (boolean? x)
      (int? x)))

;; Unique Variables
(provide (all-defined-out))

(define unique (box 1))

(define (uvar p)
  (let ((u (string->symbol 
	    (format "~a$~a" p (unbox unique)))))
    (set-box! unique (add1 (unbox unique)))
    u))

(define (uvar? d)
  (let-values (((p n) (split-uvar d)))
    (if (and p (< 0 n (unbox unique))) #t #f)))

(define (uvar-prefix d)
  (let-values (((p s) (split-uvar d)))
    p))

(define (uvar-suffix d)
  (let-values (((p s) (split-uvar d)))
    s))

(define uvar=? eq?)

(define (split-uvar d)
  (if (symbol? d)
      (let* ((w (symbol->string d))
	     (l (string-length w)))
	(let loop ((n 0))
	  (if (>= n l)
	      (values #f 0)
	      (if (and (char=? (string-ref w n) #\$) (< (add1 n) l))
		  (let ((i (string->number (substring w (add1 n) l))))
		    (if i 
			(values (substring w 0 n) i)
			(values #f 0)))
		  (loop (add1 n))))))
      (values #f 0)))

;; labels

(define string->label (lambda (x) x))
(define label? string?)
(define label-table (box '(0 . ())))
(define (label->doc! lbl)
  (let* ((tbl (unbox label-table))
         (cnt (car tbl))
         (entries (cdr tbl)))
    (set-box! label-table `(,(add1 cnt) . ((,cnt . ,lbl) . ,entries)))
    (text (format "l:~a" cnt))))
(define (reset-label-table!)
  (set-box! label-table '(0 . ())))
(define (print-label-table p)
  (unless (zero? (car (unbox label-table)))
    (fprintf p "lables:\n")
    (for ((ent (in-list (cdr (unbox label-table)))))
      (fprintf p "\t l:~a -> ~a\n" (car ent) (cdr ent)))))

(define-syntax with-printed-labels
  (syntax-rules ()
    [(_ print-exp p)
     (begin (reset-label-table!)
            print-exp
            (newline p)
            (print-label-table p))]))


;; The type of typed and untyped bindings
(struct Bnd (id exp) #:transparent)
(struct Fml (id) #:transparent)
(struct Bnd:Ty Bnd (type) #:transparent)
(struct Fml:Ty Fml (type) #:transparent)

(define (Bnd/rhs? e?) 
  (make-flat-contract
   #:name 'Bnd/rhs?
   #:first-order
   (lambda (o)
     (and (Bnd? o)
          (e? (Bnd-exp o))))))

(define (mk-bnd->doc expr->doc)
  (lambda (o)
    (match o
      [(Fml:Ty (app format->doc i) (app type->doc t))
       (doc-list (hs-append i colon t))]
      [(Fml (app format->doc i)) i]
      [(Bnd:Ty (app format->doc i) (app expr->doc e) (app type->doc t))
       (doc-list (align (vs-append i (align (v-append (hs-append colon t) e)))))] 
      [(Bnd (app format->doc i) (app expr->doc e))
       (doc-list (align (vs-append i (align e))))])))

;; Primitives

(define-generics primitive
  (op-string primitive)
  (iic-delta primitive eval)
  (prim-expr? primitive expr?))

(define (Prim/args? x?)
  (make-flat-contract
   #:name 'Prim?
   #:first-order
   (lambda (o) (and (PExpr? o) (prim-expr? o x?)))))

(struct PExpr () #:transparent)

(struct RelOp PExpr ())

(define IntxInt->Bool
  (Function `(,Int-Type ,Int-Type) Bool-Type))

(struct Rel:IntxInt RelOp (fst snd))

(define-generics rel:int-int
  (mk-rel:int-int rel:int-int n m))

(define (rel:int-int-expr? x e?)
  (and (Rel:IntxInt? x) (e? (Rel:IntxInt-fst x)) (e? (Rel:IntxInt-snd x))))

(struct Rel:IntxInt:<  Rel:IntxInt ()
        #:methods gen:primitive
        [(define prim-expr? rel:int-int-expr?)
         (define (op-string p) "%<")
         (define (iic-delta p e)
           (< (e (Rel:IntxInt-fst p))
              (e (Rel:IntxInt-snd p))))]
        #:methods gen:rel:int-int
        [(define (mk-rel:int-int o n m) (Rel:IntxInt:< n m))]
        #:transparent)
(struct Rel:IntxInt:>  Rel:IntxInt ()
        #:methods gen:primitive
        [(define prim-expr? rel:int-int-expr?)
         (define (op-string p) "%>")
         (define (iic-delta p e)
           (> (e (Rel:IntxInt-fst p))
              (e (Rel:IntxInt-snd p))))]
        #:methods gen:rel:int-int
        [(define (mk-rel:int-int o n m) (Rel:IntxInt:> n m))]
        #:transparent)
(struct Rel:IntxInt:=  Rel:IntxInt ()
        #:methods gen:primitive
        [(define prim-expr? rel:int-int-expr?)
         (define (op-string p) "%=")
         (define (iic-delta p e)
           (= (e (Rel:IntxInt-fst p))
              (e (Rel:IntxInt-snd p))))]
        #:methods gen:rel:int-int
        [(define (mk-rel:int-int o n m) (Rel:IntxInt:= n m))]
        #:transparent)
(struct Rel:IntxInt:<= Rel:IntxInt ()
        #:methods gen:rel:int-int
        [(define (mk-rel:int-int o n m) (Rel:IntxInt:<= n m))]
        #:methods gen:primitive
        [(define prim-expr? rel:int-int-expr?)
         (define (op-string p) "%<=")
         (define (iic-delta p e)
           (<= (e (Rel:IntxInt-fst p))
               (e (Rel:IntxInt-snd p))))]
        #:transparent)
(struct Rel:IntxInt:>= Rel:IntxInt ()
        #:methods gen:primitive
        [(define prim-expr? rel:int-int-expr?)
         (define (op-string p) "%>=")
         (define (iic-delta p e)
           (>= (e (Rel:IntxInt-fst p))
               (e (Rel:IntxInt-snd p))))]
        #:methods gen:rel:int-int
        [(define (mk-rel:int-int o n m) (Rel:IntxInt:>= n m))]
        #:transparent)

(struct Op PExpr ())

(define IntxInt->Int
  (Function `(,Int-Type ,Int-Type) Int-Type))

(struct Op:IntxInt Op (fst snd))

(define-generics op:int-int
  (mk-op:int-int op:int-int n m))

(define (op:int-int-expr? x e?)
  (and (Op:IntxInt? x) (e? (Op:IntxInt-fst x)) (e? (Op:IntxInt-snd x))))

(struct Op:IntxInt:*   Op:IntxInt ()
        #:methods gen:op:int-int
        [(define (mk-op:int-int p n m) (Op:IntxInt:* n m))]
        #:methods gen:primitive
        [(define prim-expr? op:int-int-expr?)
         (define (op-string p) "%*")
         (define (iic-delta p e)
           (apply/overflow *
                           (e (Op:IntxInt-fst p))
                           (e (Op:IntxInt-snd p))))]
        #:transparent)
(struct Op:IntxInt:+   Op:IntxInt ()
        #:methods gen:op:int-int
        [(define (mk-op:int-int p n m) (Op:IntxInt:+ n m))]
        #:methods gen:primitive
        [(define prim-expr? op:int-int-expr?)
         (define (op-string p) "%+")
         (define (iic-delta p e)
           (apply/overflow +
                           (e (Op:IntxInt-fst p))
                           (e (Op:IntxInt-snd p))))]
        #:transparent)
(struct Op:IntxInt:- Op:IntxInt ()
        #:methods gen:op:int-int
        [(define (mk-op:int-int p n m) (Op:IntxInt:- n m))]
        #:methods gen:primitive
        [(define prim-expr? op:int-int-expr?)
         (define (op-string p) "%-")
         (define (iic-delta p e)
           (apply/overflow -
                           (e (Op:IntxInt-fst p))
                           (e (Op:IntxInt-snd p))))]
        #:transparent)
(struct Op:IntxInt:and Op:IntxInt ()
        #:methods gen:op:int-int
        [(define (mk-op:int-int p n m) (Op:IntxInt:and n m))]
        #:methods gen:primitive
        [(define prim-expr? op:int-int-expr?)
         (define (op-string p) "%and")
         (define (iic-delta p e)
           (apply/overflow bitwise-and
                           (e (Op:IntxInt-fst p))
                           (e (Op:IntxInt-snd p))))]
        #:transparent)
(struct Op:IntxInt:or  Op:IntxInt ()
        #:methods gen:op:int-int
        [(define (mk-op:int-int p n m) (Op:IntxInt:or n m))]
        #:methods gen:primitive
        [(define prim-expr? op:int-int-expr?)
         (define (op-string p) "%or")
         (define (iic-delta p e)
           (apply/overflow bitwise-ior
                           (e (Op:IntxInt-fst p))
                           (e (Op:IntxInt-snd p))))]
        #:transparent)
(struct Op:IntxInt:>>  Op:IntxInt ()
        #:methods gen:op:int-int
        [(define (mk-op:int-int p n m) (Op:IntxInt:>> n m))]
        #:methods gen:primitive
        [(define prim-expr? op:int-int-expr?)
         (define (op-string p) "%>>")
         (define (iic-delta p e)
           (apply/overflow (lambda (n m) (arithmetic-shift n (* m -1)))
                           (e (Op:IntxInt-fst p))
                           (e (Op:IntxInt-snd p))))]
        #:transparent)
(struct Op:IntxInt:<<  Op:IntxInt ()
        #:methods gen:op:int-int
        [(define (mk-op:int-int p n m) (Op:IntxInt:<< n m))]
        #:methods gen:primitive
        [(define prim-expr? op:int-int-expr?)
         (define (op-string p) "%<<")
         (define (iic-delta p e)
           (apply/overflow (lambda (n m) (arithmetic-shift n m))
                           (e (Op:IntxInt-fst p))
                           (e (Op:IntxInt-snd p))))]
        #:transparent)

(define (mk-prim? expr?)
  (lambda (x)
    (match x
      [(or (Rel:IntxInt (? expr?) (? expr?))
           (Op:IntxInt  (? expr?) (? expr?))) #t]
      [otherwsie #f])))

(define (mk-prim->doc expr->doc)
  (lambda (p)
    (match p
      [(or (Rel:IntxInt (app expr->doc fst) (app expr->doc snd))
           (Op:IntxInt  (app expr->doc fst) (app expr->doc snd)))
       (doc-list
        (align
         (hs-append (text (op-string p))
                    (align fst)
                    (align snd))))])))
