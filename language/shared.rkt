#lang racket

(require Schml/framework/pprint
         Schml/framework/helpers
         racket/generic)

(provide ast-pretty-write-proc)
(define ast-pretty-write-proc
  (lambda (o p d)
    (with-printed-labels (pretty-print (->doc o) p) p)))

;; Core Language Types
(struct Type ())
(struct Dyn Type ()
        #:methods gen:pretty
        [(define (->doc _) (text "Dyn"))])
(struct Bool Type ()
        #:methods gen:pretty
        [(define (->doc _) (text "Bool"))])
(struct Int Type ()
        #:methods gen:pretty
        [(define (->doc _) (text "Int"))])
(struct Function Type (from to) #:transparent
        #:methods gen:pretty
        [(define/generic ->d ->doc)
         (define (->doc o)
           (doc-list (vs-append (vs-concat (map ->d (Function-from o)))
                                (->d (Function-to o)))))])

;; Internal types used to indicate the type of internal constructs
(struct Void Type ()
        #:methods gen:pretty
        [(define (->doc _) (text "Void"))])
(struct Code-Label Type ()
        #:methods gen:pretty
        [(define (->doc _) (text "Code"))])


;; Fly Weight the types keeps us from using heap space for no reason
(define Bool-Type (Bool))
(define Int-Type (Int))
(define Dyn-Type (Dyn))
(define Void-Type (Void))
(define Code-Label-Type (Code-Label))

(define/match (type->doc t)
  [((Dyn)) (text "Dyn")]
  [((Int)) (text "Int")]
  [((Bool)) (text "Bool")]
  [((Function (list (app type->doc f) ...) (app type->doc t)))
   (doc-list (hs-append (hs-concat f) (text "->") t))]
  [(o) (error 'type->doc "Given non type ~a" o)])

(provide
 (contract-out
  [type->doc (Type? . -> . doc?)]
  [struct Type ()]
  [struct (Dyn Type) ()]
  [struct (Bool Type) ()]
  [struct (Int Type) ()]
  [struct (Void Type) ()]
  [struct (Code-Label Type) ()]
  [struct (Function Type) ([from (listof Type?)] [to Type?])]
  [Bool-Type Bool?]
  [Int-Type Int?]
  [Dyn-Type Dyn?]
  [Void-Type Void?]
  [Code-Label-Type Code-Label?]))

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

(provide
 (contract-out
  [consistent? (Type? Type? . -> . boolean?)]
  [shallow-consistent? (Type? Type? . -> . boolean?)]))

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

(provide
 (contract-out
  [join (Type? Type? . -> . Type?)]))

;; Not that allocation takes up too much space but you only ever need
;; one of each of these types because they have no fields

;; Literal Constants
(define (int? x)
  (and (integer? x)
       (>= x 0)
       (<= x (expt 2 64))))

(define (constant? x)
  (or (boolean? x)
      (int? x)))

(provide
 (contract-out
  [int? (any/c . -> . boolean?)]
  [constant? (any/c . -> . boolean?)]))

;; Unique Variables

(struct uvar (prefix suffix)
        #:transparent
        #:methods gen:pretty
        [(define ->doc
           (lambda (o)
             (text (uvar->string o))))])

(define (uvar=? u v)
  (or (eq? u v)
      (eq? (uvar-suffix u)
           (uvar-suffix v))))


(define (get-uvar-maker seed)
  (let ((unique (box seed)))
    (case-lambda
      [() (unbox unique)]
      [(prefix) (let ((suffix (unbox unique)))
                  (set-box! unique (+ 1 suffix))
                  (uvar (if (symbol? prefix) (symbol->string prefix) prefix)
                        suffix))])))

;; creates the string representation of a uvar
;; (uvar? . -> . string?)
(define (uvar->string u)
  (format "~a_~a" (uvar-prefix u) (uvar-suffix u)))

(provide
 (contract-out
  [struct uvar ([prefix string?] [suffix (and/c integer? positive?)])]
  [get-uvar-maker ((and/c integer? positive?) . -> .
                   (case->
                    ((or/c string? symbol?) . -> . uvar?)
                    (-> (and/c integer? positive?))))]
  [uvar=? (uvar? uvar? . -> . boolean?)]
  [uvar->string (uvar? . -> . string?)]))

;; code labels

(struct clabel (prefix suffix)
        #:transparent
        #:methods gen:pretty
        [(define ->doc
           (lambda (o)
             (text (format "Label$~a_~a" (clabel-prefix o) (clabel-suffix o)))))])

(define (clabel=? u v)
  (or (eq? u v)
      (eq? (clabel-suffix u)
           (clabel-suffix v))))

(define (get-clabel-maker seed)
  (let ((unique (box seed)))
    (case-lambda
      [() (unbox unique)]
      [(prefix) (let ((suffix (unbox unique)))
                  (set-box! unique (+ 1 suffix))
                  (clabel (if (symbol? prefix) (symbol->string prefix) prefix)
                          suffix))])))

(provide
 (contract-out
  [struct clabel ([prefix string?] [suffix (and/c integer? positive?)])]
  [get-clabel-maker ((and/c integer? positive?) . -> .
                   (case->
                    ((or/c string? symbol?) . -> . uvar?)
                    (-> (and/c integer? positive?))))]
  [clabel=? (uvar? uvar? . -> . boolean?)]))

;; blame labels

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

(provide
 (contract-out
  [string->label (string? . -> . label?)]
  [label? (any/c . -> . boolean?)]
  [label->doc! (label? . -> . doc?)]
  [reset-label-table! (-> void?)]
  [print-label-table (any/c . -> . void?)])
 with-printed-labels)

;; The type of typed and untyped bindings
(struct Bnd (id exp) #:transparent
        #:methods gen:pretty
        [(define/generic ->d ->doc)
         (define (->doc o)
           (doc-list
            (hs-append (->d (Bnd-id o))
                       (->d (Bnd-exp o)))))])

(struct Fml (id) #:transparent
        #:methods gen:pretty
        [(define/generic ->d ->doc)
         (define (->doc o) (->d (Fml-id o)))])

(struct Bnd:Ty Bnd (type) #:transparent
        #:methods gen:pretty
        [(define/generic ->d ->doc)
         (define (->doc o)
           (doc-list
            (hs-append (->d (Bnd-id o))
                       colon
                       (->d (Bnd:Ty-type o))
                       (->d (Bnd-exp o)))))])

(struct Fml:Ty Fml (type) #:transparent
        #:methods gen:pretty
        [(define/generic ->d ->doc)
         (define (->doc o)
           (doc-list
            (hs-append (->d (Fml-id o)) colon (->d (Fml:Ty-type o)))))])

(define (Bnd/rhs? e?) 
  (make-flat-contract
   #:name 'Bnd/rhs?
   #:first-order
   (lambda (o)
     (and (Bnd? o)
          (let ((v (Bnd-id o))) (or (clabel? v) (uvar? v)))
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


(provide
 ;; Contracted binding forms utilities
 (contract-out
  [struct Bnd ((id (or/c uvar? clabel?)) (exp any/c))]
  [struct Fml ((id (or/c uvar? clabel?)))]
  [struct (Bnd:Ty Bnd) [(id (or/c uvar? clabel?)) (exp any/c) (type Type?)]]
  [struct (Fml:Ty Fml) [(id (or/c uvar? clabel?)) (type Type?)]]
  [mk-bnd->doc ((any/c . -> . doc?) . -> . ((or/c Fml? Bnd?) . -> . doc?))])
 ;; I am not sure how to write contracts about contracts
 Bnd/rhs?)

;; Primitives

(define-generics primitive
  (op-string primitive)
  (iic-delta primitive eval)
  (prim-expr? primitive expr?))

(provide op-string iic-delta prim-expr?)


(define (Prim/args? x?)
  (make-flat-contract
   #:name 'Prim?
   #:first-order
   (lambda (o) (and (PExpr? o) (prim-expr? o x?)))))

(provide Prim/args?)

(struct PExpr () #:transparent)

(struct RelOp PExpr ())

(define IntxInt->Bool
  (Function `(,Int-Type ,Int-Type) Bool-Type))

(provide IntxInt->Bool)

(struct Rel:IntxInt RelOp (fst snd))

(define-generics rel:int-int
  (mk-rel:int-int rel:int-int n m))

(define (Rel:IntxInt->doc op-str op)
  (doc-list (align (vs-append (text op-str)
                              (->doc (Rel:IntxInt-fst op))
                              (->doc (Rel:IntxInt-snd op))))))

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
        #:methods gen:pretty
        [(define (->doc o) (Rel:IntxInt->doc "%<" o))]
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
        #:methods gen:pretty
        [(define (->doc o) (Rel:IntxInt->doc "%>" o))]
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
        #:methods gen:pretty
        [(define (->doc o) (Rel:IntxInt->doc "%=" o))]
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
        #:methods gen:pretty
        [(define (->doc o) (Rel:IntxInt->doc "%<=" o))]
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
        #:methods gen:pretty
        [(define (->doc o) (Rel:IntxInt->doc "%>=" o))]
        #:transparent)

(provide
 mk-rel:int-int
 (struct-out Rel:IntxInt)
 (struct-out Rel:IntxInt:<)
 (struct-out Rel:IntxInt:>)
 (struct-out Rel:IntxInt:=)
 (struct-out Rel:IntxInt:<=)
 (struct-out Rel:IntxInt:>=))


(struct Op PExpr ())

(define IntxInt->Int
  (Function `(,Int-Type ,Int-Type) Int-Type))

(provide IntxInt->Int)

(struct Op:IntxInt Op (fst snd))

(define-generics op:int-int
  (mk-op:int-int op:int-int n m))

(provide mk-op:int-int)

(define (op:int-int-expr? x e?)
  (and (Op:IntxInt? x) (e? (Op:IntxInt-fst x)) (e? (Op:IntxInt-snd x))))

(define (Op:IntxInt->doc op-str op)
  (doc-list (align (vs-append (text op-str)
                              (->doc (Op:IntxInt-fst op))
                              (->doc (Op:IntxInt-snd op))))))

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
        #:methods gen:pretty
        [(define (->doc o) (Op:IntxInt->doc "%*" o))]
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
        #:methods gen:pretty
        [(define (->doc o) (Op:IntxInt->doc "%+" o))]
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
        #:methods gen:pretty
        [(define (->doc o) (Op:IntxInt->doc "%-" o))]
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
        #:methods gen:pretty
        [(define (->doc o) (Op:IntxInt->doc "%and" o))]
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
        #:methods gen:pretty
        [(define (->doc o) (Op:IntxInt->doc "%>>" o))]
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
        #:methods gen:pretty
        [(define (->doc o) (Op:IntxInt->doc "%<<" o))]
        #:transparent)

(provide
 (struct-out Op:IntxInt)
 (struct-out Op:IntxInt:<<)
 (struct-out Op:IntxInt:>>)
 (struct-out Op:IntxInt:and)
 (struct-out Op:IntxInt:+)
 (struct-out Op:IntxInt:-)
 (struct-out Op:IntxInt:*))

(struct Closure-field PExpr ())
(struct Closure-field:Set! Closure-field (closure field exp)
        #:methods gen:pretty
        [(define/generic ->d ->doc)
         (define (->doc o)
           (let ((closure (->d (Closure-field:Set!-closure o)))
                 (field (text (Closure-field:Set!-field o)))
                 (exp (->d (Closure-field:Set!-exp o)))
                 (prim (text "closure-set!")))
             (doc-list (hs-append prim closure field exp))))]
        #:methods gen:primitive
        [(define (prim-expr? p e?)
           (and (e? (Closure-field:Set!-closure p))
                (string? (Closure-field:Set!-field p))
                (e? (Closure-field:Set!-exp p))))
         (define (op-string p) "%closure-field-set!")
         (define (iic-delta p e)
           (error 'iic-delta "%closure-field-set!"))])

(struct Closure-field:Ref  Closure-field (closure field)
        #:methods gen:pretty
        [(define/generic ->d ->doc)
         (define (->doc o)
           (let ((closure (->d (Closure-field:Ref-closure o)))
                 (field (text (Closure-field:Ref-field o))) 
                 (prim (text "closure-ref")))
             (doc-list (hs-append prim closure field))))]
        #:methods gen:primitive
        [(define (prim-expr? p e?)
           (and (e? (Closure-field:Ref-closure p))
                (string? (Closure-field:Ref-field p))))
         (define (op-string p) "%closure-field-ref")
         (define (iic-delta p e)
           (error 'iic-delta "%closure-field-ref"))])

(struct Closure-field:Build Closure-field (fields)
        #:methods gen:pretty
        [(define/generic ->d ->doc)
         (define (->doc o)
           (let ((fields (v-concat 
                          (map (lambda (t.f) 
                                 (hs-append 
                                  (->d (car t.f)) 
                                  (text (cdr t.f))))
                               (Closure-field:Build o))))
                 (prim (text "closure-build")))
             (doc-list (hs-append prim fields))))]
        #:methods gen:primitive
        [(define (prim-expr? p e?)
           (andmap (lambda (t.f) 
                     (and (Type? (car t.f))
                          (string? (cdr t.f))))
                   (Closure-field:Build-fields p)))
         (define (op-string p) "%closure-field-build")
         (define (iic-delta p e)
           (error 'iic-delta "%closure-field-build"))]) 

(provide 
 (contract-out
  [struct (Closure-field:Build Closure-field)
    ([fields (listof (cons/c Type? string?))])]
  [struct (Closure-field:Set! Closure-field) 
    ([closure any/c] [field string?] [exp any/c])]
  [struct (Closure-field:Ref Closure-field) 
    ([closure any/c] [field string?])]))


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

(provide mk-prim? mk-prim->doc)

