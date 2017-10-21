#lang typed/racket

(require (for-syntax "macros.rkt"))

;; A language form is any of the possible nodes in AST
;; Theses forms are almost always poly-morphic in order to allow reuse between
;; languages. The definintion is slighly encumbered by typed racket's lack
;; of support for generic struct imports or method-table definitions
;; This can and has been hacked arround by defining a super type for each
;; form in untyped land that has all the methods and performs unsafe access
;; of the struct that is handed to it. For this reason there should be no
;; subtyping of structs allowed. Many of the definitions are highly repedetive
;; so I have written some macros to help the macro definitions are at the end of
;; the file.


;; The following three macros cover the definitions of all completely
;; language forms. There is little use of understanding this macro unless
;; there is something broken in it.

;; f**->tof** read (listof (listof field)) to (listof (listof type-of-field))
;; and it does as it is named it converts the syntax
;; ((id ...) ...) to ((type-of-id ...) ...)
(define-for-syntax (f**->tof** f**)
  (define (f->tof f)
    (id-append #:ctx f "type-of-" f))
  (define (f*->tof* f*)
    (map f->tof (syntax->list f*)))
  (map f*->tof* (syntax->list f**)))

;; Similarly this macro is (listof form) to (listof form-method-table)
(define-for-syntax (f*->fmt* f*)
  (map (lambda (f) (id-append #:ctx f "MT:" f)) (syntax->list f*)))

#|
(define-typed-form*
  (form field1 field2) ...)
==>
(begin
  (struct (type-of-field1) form form-method-table ([field1 : type-of-field1]))
  ...)
|#

(define-syntax (define-typed-form* stx)
  (syntax-case stx ()
    [(_ (form* field** ...) ...)
     (with-syntax
       ([((type-of-field** ...) ...) (f**->tof** #'((field** ...) ...))]
        [(form-method-table* ...) (f*->fmt* #'(form* ...))])
       (syntax/loc stx
         (begin
           #;(require/typed "forms-untyped.rkt"
             [#:struct form-method-table* ()] ...)
           (provide (struct-out form*) ...)
           (struct (type-of-field** ...) form* form-method-table*
             ([field** : type-of-field**] ...)
             #:transparent)
           ...)))]))

(require/typed "forms-untyped.rkt"
               [#:struct MT:Prog ()]
               [#:struct MT:Ann ()]
               [#:struct MT:Let ()]
               [#:struct MT:Lambda ()]
               [#:struct MT:App ()]
               [#:struct MT:If ()]
               [#:struct MT:Ascribe ()]
               [#:struct MT:Op ()]
               [#:struct MT:Letrec ()]
               [#:struct MT:Quote ()]
               )
               

(define-typed-form*
  ;; Top level wrapper for combining compiler state, configuration,
  ;; meta-information and the program AST
  (Prog annotation expression)
  ;; Annotations that may be added to other AST nodes
  (Ann value data)
  ;; Function abstraction
  (Lambda formals body)
  ;; Function application 
  (App operator operands)
  ;; Conditionals
  (If test then else)
  ;; Type ascription
  (Ascribe expression type label)
  ;; Primitive operators application 
  (Op operator operands)
  ;; recursive binding
  (Letrec bindings body)
  ;; non recursive binding
  (Let bindings body)
  ;; sequence operator
  ;;(Begin effects value)
  ;; Perform a No Operation
  ;;(No-Op)
  ;; Effect operations
  ;; Monotonic effects
  ;;(Mbox value)
  ;;(Munbox box)
  ;;(Mbox-set! box value)
  ;;(Mvector value constructor)
  ;;(Mvector-set! vector offset value)
  ;;(Mvector-ref vector offset)
  ;; Guarded effects
  ;;(Gbox value)
  ;;(Gunbox box)
  ;;(Gbox-set! box value)
  ;;(Gvector len init-val)
  ;;(Gvector-set! vector offset value)
  ;;(Gvector-ref vector offset)
  ;; various imediates markers
  (Quote datum)    ;; immediate data in general
  ;; Node that references a piece of code identified by the UID value
  ;;(Code-Label value)
  ;;(Type type)        ;; an atomic type
  ;; Effectfull expressions
  ;; typed bindings annotations
  ;;(Fml identifier type)
  ;;(Bnd identifier type expression)
  ;; Different casts
  ;;(Cast expression type-exp type-cast label)
  ;; TODO Interpreted-Cast
  ;;(Runtime-Cast expression type-exp type-cast label)
  ;;(Fn-Cast expressiong type-exp type-cast label)
  ;;Type Operations
  ;;(Type-tag expression)
  ;;(Type-Fn-arity expression)
  ;;(Type-Fn-arg expression index)
  ;;(Type-Fn-return expression)
  ;;(Type-GRef-to expression)
  ;;(Type-GVect-to expression)
  ;; closure Representation
  ;; (Fn-Caster expression)
  ;; (Closure-Data code caster variables)
  ;; (Closure-code var)
  ;; (Closure-ref this var)
  ;; (Closure-caster this)
  ;; (LetP bindings body)
  ;; (LetC bindings body);; Can create cyclic immutable data
  ;; (Procedure this params code caster bound-vars body)
  ;; ;; represents a set of moves to initialize variables before
  ;; (Code variables body)
  ;; ;; Dyn operations
  ;; (Dyn-tag expression)
  ;; (Dyn-immediate expression)
  ;; (Dyn-type expression)
  ;; (Dyn-value expression)
  ;; (Dyn-make expression type)
  ;; ;; Observational Operations
  ;; (Blame expression)
  ;; (Observe expression type)
  ;; ;; Lambda subforms
  ;; (Castable caster body)
  ;; (Bound closure variables body)
  ;; (Free caster variables body)
  ;; ;; Static Global Binding
  ;; (Labels bindings body)
  ;; ;; Benchmarking tools language forms
  ;; ;; low cost repetition
  ;; (Repeat var start end body)
  ;; ;; TODO figue out an appropriate comment about all forms here
  ;; (Halt)
  ;; (Assign lhs rhs)
  ;; ;; Declares Local Variables
  ;; (Locals names body)
  ;; (Return value)
  ;; (Relop primitive expression1 expression2)
  ;; ;; Uil Memory Ops
  ;; (Malloc expression)
  ;; ;; Uil IO Primitives todo add C Calls and replace these
  ;; (Printf format expressions)
  ;; (Print expression)
  ;; (BinOp primitive expression1 expression2)
  ;; ;; Guarded references IL Representation
  ;; (GRep-proxied? expression)
  ;; (UGbox expression)
  ;; (UGbox-set! expression1 expression2)
  ;; (UGbox-ref expression)
  ;; (UGvect expression1 expression2)
  ;; (UGvect-set! expression1 expression2 expression3)
  ;; (UGvect-ref expression1 expression2)
  ;; (Gproxy for-exp from-exp to-exp blames-exp)
  ;; (Gproxy-for expression)
  ;; (Gproxy-from expression)
  ;; (Gproxy-to expression)
  ;;(Gproxy-blames expression)
  )


(require/typed/provide "forms-untyped.rkt"
                       [#:struct Uid ([prefix : Symbol] [suffix : Number])]
                       [#:struct Var ([name : Uid])])



