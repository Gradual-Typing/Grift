#lang racket
(require "../../src/language/forms.rkt"
         "../helpers.rkt"
         "../../src/schml/syntax-to-schml0.rkt"
         "../../src/casts/casts-to-coercions.rkt")
(provide (all-defined-out))

;; Save the source code ./src/name.schml
(define (write-source path prog)
  (call-with-output-file path #:exists 'replace #:mode 'text
    (lambda (file-port)
      (define print-without-quote 1)
      (pretty-print prog file-port print-without-quote)
      (pretty-print prog (current-output-port) print-without-quote))))

(define (make-timing-loop
         #:letrec-bnds    [letrec-bnds '()]
         #:let-bnds       [let-bnds '()]
         #:acc-type       acc-type
         #:acc-init       acc-init
         #:use-acc-action use-acc-action ; (Symbol -> Schml-Expr)
         #:timed-action   timed-action)   ; (Symbol Symbol -> Schml-Expr)
  `(letrec ,letrec-bnds 
     (let ([iters : Int (read-int)])
       ,(let make-let* ([b* let-bnds])
          (if (null? b*)
              `(letrec ([run-test
                         : (Int ,acc-type -> ,acc-type)
                         (lambda ([i : Int] [acc : ,acc-type])
                           ,(timed-action 'i 'acc))])
                 (let ([res (time (repeat (i 0 iters) (acc : ,acc-type ,acc-init)
                                    (run-test i acc)))])
                   ,(use-acc-action 'res)))
              `(let (,(car b*))
                 ,(make-let* (cdr b*))))))))

(define timing-loop-example
  (make-timing-loop
   #:letrec-bnds '(FUNC-BND ...)
   #:let-bnds    '(DATA-BND ...)
   #:acc-type    'TYPE-OF-ACCUMULATOR
   #:acc-init    'ACCUMULATOR-INIT
   #:timed-action   (lambda (i acc) 'TIMED-CODE)
   #:use-acc-action (lambda (acc) 'USE-OF-ACCUMULATOR)))

(define (sizeof-type t)
  (match t
    [`(,t* ... -> ,t)
     (+ (for/sum ([t t*]) (sizeof-type t))
        (sizeof-type t)
        1)]
    [(or `(GRef ,t) `(GVect ,t)) (+ 1 (sizeof-type t))]
    [(or (? symbol?) (? null?)) 1]))

(define (sizeof-type* t1 t2)
  (cond
    [(equal? t1 t2) 1]
    [else
     (match* (t1 t2)
       [(`(,t1* ... -> ,t1) `(,t2* ... -> ,t2))
        (+ (for/sum ([t1 t1*] [t2 t2*]) (sizeof-type* t1 t2))
           (sizeof-type* t1 t2)
           1)]
       [(`(GRef ,t1) `(GRef ,t2)) 
        (+ 1 (sizeof-type* t1 t2))]
       [(`(GVect ,t1) `(GVect ,t2))
        (+ 1 (sizeof-type* t1 t2))]
       [((or (? symbol?) (? null?)) _) 1]
       [(_ (or (? symbol?) (? null?))) 1])]))

(define (sizeof-coercion c)
  (match c
    [(Identity) 1]
    [(Project ty lbl) 1]
    [(Inject ty) 1]
    [(Sequence fst snd)
     (+ 1 (sizeof-coercion fst) (sizeof-coercion snd))]
    [(Failed label) 1]
    [(Fn arr args ret)
     (+ 1
        (for/sum ([c args]) (sizeof-coercion c))
        (sizeof-coercion ret))]
    [(Ref r w)
     (+ 1 (sizeof-coercion r) (sizeof-coercion w))]
    [else (error 'sizeof-coercion "unmatched ~a" c)]))

(define (sizeof-coercionof-types t1 t2)
  (sizeof-coercion
   ((make-coercion "")
    (parse-type (datum->syntax #f t1))
    (parse-type (datum->syntax #f t2)))))

(define timing-loop-test
  (make-timing-loop
   #:timed-action  (lambda (i acc) `(+ ,i ,acc))
   #:acc-type 'Int
   #:acc-init 0
   #:use-acc-action (lambda (acc) acc)))

(define (make-app-timing-loop casts t1 t2 init use)
  (define id-type `(,t1 -> ,t1))
  (define id-type-inv `(,t2 -> ,t2))
  (define-values (id-casted-type acc-type cl-invocation use-undyned)
    (if (even? casts)
        (values id-type t1  
                `(cast-loop ,casts id)
                (lambda (use) use))
        (values id-type-inv t2
                `(: (cast-loop ,(sub1 casts) id) (,t2 -> ,t2))
                (lambda (use) (lambda (v) `(: ,(use v) Int))))))
  (make-timing-loop
   #:timed-action (lambda (i acc) `(id-casted ,acc))
   #:letrec-bnds
   `([cast-loop : (Int ,id-type -> ,id-type)
                (lambda ([n : Int] [g : ,id-type])
                  (if (= n 0)
                      g 
                      (cast-loop (- n 2) (: g ,id-type-inv))))]
     [id : ,id-type (lambda ([x : ,t1]) x)])
   #:let-bnds
   `([id-casted : ,id-casted-type ,cl-invocation])
   #:acc-type acc-type
   #:acc-init init
   #:use-acc-action (use-undyned use)))

(define (symbolic-repeat n ctr base)
  (if (<= n 0)
      `(,ctr ,base)
      `(,ctr ,(symbolic-repeat (sub1 n) ctr base))))

(define (make-box-cast-timing-loop t1 t2 depth)
  (make-timing-loop
   #:timed-action (lambda (i acc) `(: (: acc ,t2) ,t1))
   #:acc-type t1
   #:acc-init (symbolic-repeat depth 'gbox 42)
   #:use-acc-action (lambda (v) (symbolic-repeat depth 'gunbox v))))

(define (make-box-write-read-timing-loop t1 t2 casts depth)
  (define ref (symbolic-repeat depth 'gbox -22))
  (define use (lambda (v)
                `(+ ,(cadr (symbolic-repeat depth 'gunbox v))
                    ,(symbolic-repeat depth 'gunbox 'guarded-ref))))
  (define-values (acc-type cl-invocation acc-init)
    (if (even? casts)
        (values (cadr t1)
                `(cast-loop ,casts ,ref)
                (cadr (symbolic-repeat depth 'gbox 21)))
        (values (cadr t2)
                `(: (cast-loop ,(sub1 casts) ,ref) ,t2)
                (cadr (symbolic-repeat depth 'gbox '(: 21 Dyn))))))
  (make-timing-loop
   #:timed-action
   (lambda (i acc)
     `(begin
        (gbox-set! guarded-ref acc)
        (gunbox guarded-ref)))
   #:letrec-bnds
   `([cast-loop : (Int ,t1 -> ,t1)
                (lambda ([n : Int] [g : ,t1])
                  (if (= n 0)
                      g 
                      (cast-loop (- n 2) (: g ,t2))))])
   #:let-bnds
   `([guarded-ref : (GRef ,acc-type) ,cl-invocation])
   #:acc-type acc-type
   #:acc-init acc-init
   #:use-acc-action use))
