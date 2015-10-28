#lang racket

(require racket/unsafe/ops)
(require racket/lazy-require)
(require (for-syntax "./macros.rkt"))
(require racket/generic)

(require racket/generic)

(provide form-map form->sexp language-form?)
(define-generics language-form
  #:fast-defaults
  ;; This the null and pair? implementations are of a deep map
  ([null? (define (form-map f nil) '())
          (define (form->sexp nil) '())]
   [pair? (define/generic fmap  form-map)
          (define/generic fsexp form->sexp)
          (define (form-map ptable pair)
            (cons (fmap (car pair) ptable)
                  (fmap (cdr pair) ptable)))
          (define (form->sexp pair)
            (cons (fsexp (car pair))
                  (fsexp (cdr pair))))])
  ;; The generic interface form mapping over language forms
  [form-map ptable language-form]
  [form->sexp language-form])


;; lookup how to handle subforms in a map based on there meta position in the
;; form
(define (ptable-lookup table key [alt (lambda (x) x)])
  (let ([p? (hash-ref table key #f)])
    (or p? alt)))

(define (ptable-dispatch table key value [alt (lambda (x) x)])
  ((ptable-lookup table key alt) value))

(define ((form-map/ptable ptable) language-form)
  (form-map ptable language-form))


;; This file is all for the purpose of making structs
;; that can have generic methods. This should allow
;; us to cut down on the boilerplate required by the
;; compiler to just find individual expressions
;; but using these features will probably make the compiler
;; really slow. Oh well.




;; The following are a series of macro definitions that help with
;; not repeating yourself during the form defininition proccess

;; Helper that enumerates the names that are being concatonated
(define-for-syntax (struct-access-aux n n.f*)
  (define (accessor-name base field)
    (define base-string (symbol->string (syntax->datum base)))
    (define field-string (symbol->string (syntax->datum field)))
    (define base-field-string (string-append base-string "-" field-string))
    (datum->syntax base (string->symbol base-field-string)))
  (syntax-case n.f* ()
    [() #'()]
    [((name . field) . rest)
     #`((#,(accessor-name #'name #'field) . #,n)
        . #,(struct-access-aux (add1 n) #'rest))]))

;; Defines the accessors for a struct given the (struct id (fields ...))
;;
(define-syntax (define-struct-access stx)
  (syntax-case stx ()
    [(_ (id fields ...))
     (let* ([n.f* (syntax->list #'((id . fields) ...))]
            [a.n  (struct-access-aux 0 n.f*)])
       (with-syntax ([((name^ . i) ...) a.n])
         #`(begin
             (define-syntax-rule (name^ v) (unsafe-struct-ref v i))
             ...)))]))

(define-for-syntax (template->sexp template form recur)
  (syntax-case template (unquote)
    [,,op   #`,(op #,form)]
    [,op    #`,(op (#,recur #,form))]
  
    [(a . d) #`(#,(template->sexp #'a form recur)
                . #,(template->sexp #'d form recur))]
    [other   #'other]))

(define-syntax (define-form->sexp stx)
  (syntax-case stx (quasiquote)
    [(_ name `template)
     (with-syntax ([form->sexp (datum->syntax stx 'form->sexp)]
                   [fsexp      (datum->syntax stx 'fsexp)])
       #`(define (name form)
           `#,(template->sexp #'template #'form #'fsexp)))]))

(define-syntax-rule (define-form-map
                      (form-map name (meta-possition field-accessor) ...))
  #'(define (form-map ptable form)
      (Name (ptable-dispatch ptable 'meta-position (field-accessor form))
            ...))) 
       
(define-syntax (map-> stx)
  ))

    
;; Combining the helpers together
(define-syntax (define-untyped-form* stx)
  (syntax-case stx (forall-forms)
    [(_ (forall-forms shared ...) ((name field ...) method ...) ...)
     (let ([->mt (lambda (s) (id-append #:ctx s "MT:" s))])
       (with-syntax ([(mtable ...) (map ->mt (syntax->list #'(name ...)))]
                     [interface    (datum->syntax stx 'gen:language-form stx stx)]
                     )
         
         #'(begin
             (provide (struct-out mtable) ...)
             (lazy-require ("forms-typed.rkt" (name ...)))
             (define-struct-access (name field ...)) ...
             (struct mtable () #:transparent
               #:methods interface [shared ... method ...]) ...)))]))



(define-untyped-form*
  (forall-forms
   (define/generic fmap form-map)
   (define/generic fsexp form->sexp))
  ((Prog annotation program)
   (define (form-map ptable form)
     (Prog (ptable-dispatch ptable 'meta-data (Prog-annotation form))
           (ptable-dispatch ptable 'program (Prog-program form)
                            (form-map/ptable ptable))))
   (define (form->sexp form)
     `(prog ,(Prog-annotation form) ,(fsexp (Prog-program form)))))

  ((Ann expression data)
   (define (form->sexp form)
     `(ann ,(fsexp (Ann-expression form)) ,(fsexp (Ann-data form))))
   (define (form-map ptable form)
     (Ann (ptable-dispatch 'expression (Ann-expression form))
          (ptable-dispatch 'meta-data (Ann-expression form)))))

  ((Lambda formals body)
   (define (form->sexp form)
     `(lambda ,(map fsexp (Lambda-formals form))
        ,(fsexp (Lambda-body form))))
   (define (form-map ptable form)
     (Lambda (ptable-dispatch ptable 'formals (Lambda-formals form))
             (ptable-dispatch ptable 'expression (Lambda-body form)))))

  ((App operator operands)
   (define (form->sexp form)
     `(,(fsexp (App-operator form)) . ,(map fsexp (App-operands form))))
   (define (form-map ptable form)
     (let ([p (ptable-lookup ptable 'expression)])
       (App (p (App-operator form)) (map p (App-operands form))))))
  
  ((If test then else)
   (define (form->sexp form)
     `(if ,(fsexp (If-test form))
          ,(fsexp (If-then form))
          ,(fsexp (If-else form))))
   (define (form-map ptable form)
     (let ([p (ptable-lookup ptable 'expression)])
       (If (p (If-test form)) (p (If-then form)) (p (If-else form))))))
  
  ((Ascribe expression type label)
   (define (form->sexp form)
     `(:@ ,(fsexp (Ascribe-expression form))
          ,(fsexp (Ascribe-type form))
          ,(fsexp (Ascribe-label form))))
   (define (form-map ptable form)
     (Ascribe (ptable-dispatch ptable 'expression (Ascribe-expression form))
              (ptable-dispatch ptable 'type  (Ascribe-type form))
              (ptable-dispatch ptable 'label (Ascribe-label form)))))

  ((Op operator operands)
   (define (form->sexp form)
     `(,(Op-operator form) . ,(map fsexp (Op-operands form))))
   (define (form-map ptable form)
     (let ([p (ptable-lookup ptable 'expression)])
       (Op (Op-operator form) (map p (Op-operands form))))))

  ((Letrec bindings body)
   (define (form->sexp form)
     `(letrec ,(map fsexp (Letrec-bindings form))
        ,(fsexp (Letrec-body form))))
   (define (form-map ptable form)
     (Letrec (ptable-dispatch ptable 'bindings (Letrec-bindings form))
             (ptable-dispatch ptable 'body (Letrec-body form)))))
  
  ((Let bindings body)
   (define (form-map ptable form)
     (Let (ptable-dispatch ptable 'bindings (Let-bindings form))
          (ptable-dispatch ptable 'expression (Let-body form))))
   (define (form->sexp form)
     `(let ,(fsexp (Let-bindings form))
        ,(fsexp (Let-body form)))))

  ((Quote datum)
   (define (form-map ptable form)
     (Quote (ptable-dispatch ptable 'datum (Quote-datum form))))
   (define (form->sexp form)
     `',(Quote-datum form))))

;; Struct that are changing
(provide (struct-out Var))
(struct Var (name)
  #:transparent
  #:guard
  (lambda (name type)
    (unless (Uid? name)
      (error type "Var name must be a unique identifier"))
    (values name))
  #:methods gen:language-form
  [(define (form->sexp f)
     (string->symbol (string-append "v_" (uid->string (Var-name f)))))
   (define (form-map f p)
     (let ([p? (hash-ref p 'var #f)])
       (if p?
           (p? f)
           f)))])

;; Uids
(define (uid->string u)
  (format "~a_~a" (Uid-suffix u) (Uid-prefix u)))
(provide (struct-out Uid))
(struct Uid (prefix suffix)
  #:guard
  (lambda (prefix suffix struct-type)
    (unless (or (string? prefix) (symbol? prefix))
      (error 'schml-uid-guard "uid prefix must be string or symbol"))
    (unless (number? suffix)
      (error 'schml-uid-guard "uid suffix must be a number"))
    (if (string? prefix)
        (values (string->symbol prefix) suffix)
        (values prefix suffix)))
  #:methods gen:language-form
  [(define (form->sexp f) (uid->string f))
   (define (form-map f p)
     (let ([p? (hash-ref 'uid p #f)])
       (if p? (p? f) f)))])


