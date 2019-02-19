#lang racket/base

(require
 "./forms.rkt"
 (for-syntax racket/base
             racket/syntax
             syntax/parse))
(provide (all-defined-out))

;; We should believe in the type system more and do away with the
;; tracking of the type of primitives
(struct primitive (symbol effectfull? type)
  #:transparent)

(define hashcons!-equal-hashtable (make-hash))
(define (hashcons! x)
  (cond
    [(hash-ref hashcons!-equal-hashtable x #f) => values]
    [else
     (hash-set! hashcons!-equal-hashtable x x)
     x]))

(define-syntax (syntax->type stx)
  (syntax-parse stx
    [(_ (~or () (~datum Unit)))   #'UNIT]
    [(_ (~datum Void)) #'VOID]
    [(_ (~datum Int))  #'INT]
    [(_ (~datum Float))  #'FLOAT]
    [(_ (~datum String)) #'STRING]
    [(_ (~datum Bool)) #'BOOL]
    [(_ (~datum Char)) #'CHAR]
    [(_ (~datum Obj))  #'OBJ]
    [(_ (~datum Type)) #'RT-TYPE]
    [(_ (~datum Bottom)) #'BOTTOM]
    [(_ (~datum Assoc-Stack)) #'ASSOC-STACK]
    [(_ (Args ... -> Ret))
     (define/with-syntax arity (length (syntax->list #'(Args ...))))
     #'(hashcons! (Fn arity (list (syntax->type Args) ...) (syntax->type Ret)))]
    [(_ ((~datum Tuple) Args ...))
     (define/with-syntax arity (length (syntax->list #'(Args ...))))
     #'(hashcons! (STuple arity (list (syntax->type Args) ...)))]
    [(_ (Ctr:id Arg)) #'(hashcons! (Ctr (syntax->type Arg)))]))

(define-syntax (define-primitive-table stx)
  (syntax-parse stx
    [(_ (s:id e?:boolean t) ...)
     (define/with-syntax s->p (format-id stx "symbol->primitive-table"))
     (define/with-syntax (tmp* ...) (generate-temporaries  #'(s ...)))
     #'(begin
         (define tmp* (primitive 's e? (syntax->type t))) ...
         (define s->p (make-immutable-hasheq `((s . ,tmp*) ...))))]))

(define-primitive-table
  ;; Name / Associated Data Structure / Side Effects? / Type
  (make-assoc-stack   #f (-> Assoc-Stack))
  (assoc-stack-find   #f (Assoc-Stack Obj Obj -> Int))
  (assoc-stack-ref    #f (Assoc-Stack Int -> Obj))
  (assoc-stack-pop!   #t (Assoc-Stack -> Obj))
  (assoc-stack-set!   #t (Assoc-Stack Int Obj -> Unit))
  (assoc-stack-push!  #t (Assoc-Stack Obj Obj Obj -> Unit))
  (*                  #f (Int Int -> Int))
  (+                  #f (Int Int -> Int))
  (-                  #f (Int Int -> Int))
  (binary-not         #f (Int -> Int))
  (binary-and         #f (Int Int -> Int))
  (binary-or          #f (Int Int -> Int))
  (binary-xor         #f (Int Int -> Int))
  (not                #f (Bool -> Bool))
  (%/                 #f (Int Int -> Int))
  (%>>                #f (Int Int -> Int))
  (%<<                #f (Int Int -> Int))
  (%%                 #f (Int Int -> Int))
  (quotient           #f (Int Int -> Int))
  (read-int           #t (-> Int))
  (print-int          #t (Int -> Unit))
  (<                  #f (Int Int -> Bool))
  (<=                 #f (Int Int -> Bool))
  (=                  #f (Int Int -> Bool))
  (>                  #f (Int Int -> Bool))
  (>=                 #f (Int Int -> Bool))
  (print-bool         #t (Bool -> Unit))
  (read-bool          #t (-> Bool))
  (fl+                #f (Float Float -> Float))
  (fl-                #f (Float Float -> Float))
  (fl*                #f (Float Float -> Float))
  (fl/                #f (Float Float -> Float))
  (flmodulo           #f (Float Float -> Float))
  (flexpt             #f (Float Float -> Float))
  (flmin              #f (Float Float -> Float))
  (flmax              #f (Float Float -> Float))
  (flquotient         #f (Float Float -> Int))
  (flabs              #f (Float -> Float))
  (flround            #f (Float -> Float))
  (flfloor            #f (Float -> Float))
  (flceiling          #f (Float -> Float))
  (fltruncate         #f (Float -> Float))
  (flsin              #f (Float -> Float))
  (flcos              #f (Float -> Float))
  (fltan              #f (Float -> Float))
  (flasin             #f (Float -> Float))
  (flacos             #f (Float -> Float))
  (flatan             #f (Float -> Float))
  (fllog              #f (Float -> Float))
  (flexp              #f (Float -> Float))
  (flsqrt             #f (Float -> Float))
  (flnegate           #f (Float -> Float))
  (print-float        #t (Float Int -> Unit))
  (read-float         #t (-> Float))
  (float->int         #f (Float -> Int))
  (fl<                #f (Float Float -> Bool))
  (fl<=               #f (Float Float -> Bool))
  (fl=                #f (Float Float -> Bool))
  (fl>=               #f (Float Float -> Bool))
  (fl>                #f (Float Float -> Bool))
  (int->char          #f (Int  -> Char))
  (char->int          #f (Char -> Int))
  (read-char          #t (-> Char))
  (print-char         #t (Char -> Unit))
  (display-char       #t (Char -> Unit))
  (timer-start        #t (-> Unit))
  (timer-stop         #t (-> Unit))
  (timer-report       #t (-> Unit))
  (Alloc              #t (Int -> (Array Obj)))
  (Array-set!         #t ((Array Obj) Int Obj -> Unit))
  (Array-ref          #f ((Array Obj) Int -> Obj))
  (Types-hashcons!    #t (Type -> Type))
  (Types-gen-index!   #t (Type -> Unit)) 
  (Printf             #t (String (List Obj) -> Unit))
  (Print              #t (String -> Unit))
  (Exit               #t (Int -> Bottom)))

(define (symbol->primitive s not-found-th)
  (hash-ref symbol->primitive-table s not-found-th))

(define (grift-primitive->type p)
  (primitive-type (grift-primitive p)))

(define (grift-primitive sym/prim)
  (cond
    [(symbol? sym/prim)
     (define (err) (error 'grift-primitive "invalid: ~a" sym/prim))
     (symbol->primitive sym/prim err)]
    [else sym/prim]))

(define (grift-primitive-effect? p)
  (primitive-effectfull? (grift-primitive p)))

(module+ test
  (require rackunit)
  (check-true (Fn? (grift-primitive->type 'flacos))))
