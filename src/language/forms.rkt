#lang typed/racket

(require "../helpers.rkt")
(provide (all-defined-out))

#|
All language "Forms" are just polymorphic racket structs.
These are used to define valid ASTs which are passed between
passes of the compiler. There are many forms because we try
to switch forms whenever there is any change in the meaning
of the form.

The macro generates a function for each name in the list plus
an predicate to identify values of this struct.
So (name field1 field2) would create the following definitions.
name? - predicate
name  - constructor
name-field1 - accessor for field1
name-field2 - accessor for field2
And a type constructor "name" expecting the types of field1 and field2 
|#

(define-syntax (define-forms stx)
  (syntax-case stx ()
    [(_ (name* field** ...) ...)
     (let ([f** (syntax->list #'((field** ...) ...))])
       (with-syntax ([((type** ...) ...) (map generate-temporaries f**)])
         #'(begin
             (struct (type** ...) name* ([field** : type**] ...) #:transparent)
             ...)))]))

(define-forms
  ;; Top level wrapper for combining compiler
  ;; state, meta-information, and the program AST
  (Prog annotation expression)
  ;; Annotations that may be added to other AST nodes
  (Ann value data)
  ;; Function abstraction
  (Lambda formals body)
  ;; Function application 
  (App operator operands)
  ;; Variable node
  (Var id)
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
  (Begin effects value)
  ;; Perform a No Operation
  (No-Op)
  ;; Effect operations
  ;; Monotonic effects
  (Mbox value)
  (Munbox box)
  (Mbox-set! box value)
  (Mvector value constructor)
  (Mvector-set! vector offset value)
  (Mvector-ref vector offset)
  ;; Guarded effects
  (Gbox value)
  (Gunbox box)
  (Gbox-set! box value)
  (Gvector len init-val)
  (Gvector-set! vector offset value)
  (Gvector-ref vector offset)
  ;; various imediates markers
  (Quote literal)    ;; immediate data in general
  ;; Node that references a piece of code identified by the UID value
  (Code-Label value)
  (Tag bits)         ;; an tag for an imediate value
  (Type type)        ;; an atomic type
  ;; Effectfull expressions
  ;; typed bindings annotations
  (Fml identifier type)
  (Bnd identifier type expression)
  ;; Different casts
  (Cast expression instruction)
  ;; TODO Interpreted-Cast
  (Interpreted-Cast expression instruction)
  (Fn-Caster expression)
  ;;Type Operations
  (Type-Dyn-Huh exp)
  (Type-Tag expression)
  (Type-Fn-arity expression)
  (Type-Fn-arg expression index)
  (Type-Fn-return expression)
  (Type-Fn-Huh type)
  (Type-GVect-Huh type)
  (Type-GRef-Huh type)
  (Type-GRef-Of expression)
  (Type-GVect-Of expression)
  ;; closure Representation
  (App-Closure code data exprs)
  (Closure-Data code caster variables)
  (Closure-code var)
  (Closure-ref this var)
  (Closure-caster this)
  (TypeId id)
  (LetT* bindings body)
  (LetP bindings body)
  (LetC bindings body);; Can create cyclic immutable data
  (Procedure this params code caster bound-vars body)
  ;; represents a set of moves to initialize variables before
  (Code variables body)
  ;; Dyn operations
  (Dyn-tag expression)
  (Dyn-immediate expression)
  (Dyn-type expression)
  (Dyn-value expression)
  (Dyn-make expression type)
  ;; Observational Operations
  (Blame expression)
  (Observe expression type)
  ;; Lambda subforms
  (Castable caster body)
  (Bound closure variables body)
  (Free caster variables body)
  (GlobDecs vars body) ;; declaration of global variables
  ;; Static Global Binding
  (Labels bindings body)
  (App-Code rand rators)
  (App-Fn rand rators)
  (App/Fn-Proxy-Huh rand rators)
  (App-Fn-or-Proxy cast rand rators)
  ;; Benchmarking tools language forms
  ;; low cost repetition
  (Repeat var start end body)
  ;; TODO figue out an appropriate comment about all forms here
  (Halt)
  (Success)
  (Assign lhs rhs)
  ;; Declares Local Variables
  (Locals names body)
  (Return value)
  (Relop primitive expression1 expression2)
  ;; Uil Memory Ops
  (Malloc expression)
  ;; Uil IO Primitives todo add C Calls and replace these
  (Printf format expressions)
  (Print expression)
  (BinOp primitive expression1 expression2)
  ;; Guarded references IL Representation
  (Unguarded-Box init)
  (Unguarded-Box-Ref box)
  (Unguarded-Box-Set! box value)
  (Unguarded-Vect size value)
  (Unguarded-Vect-Ref vect index)
  (Unguarded-Vect-Set! vect index value)
  (Guarded-Proxy expression representation)
  (Guarded-Proxy-Ref guarded)
  (Guarded-Proxy-Source guarded)
  (Guarded-Proxy-Target guarded)
  (Guarded-Proxy-Blames guarded)
  (Guarded-Proxy-Coercion guarded)
  (Guarded-Proxy-Huh expression)
  #|
  (GRep-proxied? expression)
  (UGbox expression)
  (UGbox-set! expression1 expression2)
  (UGbox-ref expression)
  (UGvect expression1 expression2)
  (UGvect-set! expression1 expression2 expression3)
  (UGvect-ref expression1 expression2)
  (Gproxy for-exp from-exp to-exp blames-exp)
  (Gproxy-for expression)
  (Gproxy-from expression)
  (Gproxy-to expression)
  (Gproxy-blames expression)
  |#
)

(define NO-OP (No-Op))


(define-type Blame-Label String)
(define blame-label? string?)


#| Types throughout the languages |#

;; Schml types
(define-forms
  (Unit)
  (Int)
  (Bool)
  (Dyn)
  (Fn arity fmls ret)
  (MRef  arg)
  (MVect arg)
  (GRef  arg)
  (GVect arg))

;; TODO I am unsure of if these are being used
;; find out and act appropriately
#;
(define-forms
  (String-Ptr)
  (Any-Value)
  (Any-Type)
  (Void-Type)
  (Bottom-Type))

;;Constants for the types
(define UNIT-TYPE (Unit))
(define INT-TYPE (Int))
(define BOOL-TYPE (Bool))
(define DYN-TYPE (Dyn))
(define REF-DYN-TYPE (GRef DYN-TYPE))

;; define type id = t and id* = (c* t) ...
(define-syntax-rule (define-type+ id ([id* c*] ...) t)
  (begin (define-type id t)
	 (define-type id* (c* id)) ...))

#|
(define ANY-TYPE (Any-Type))
(define STRING-PTR (String-Ptr))
(define ANY-VALUE (Any-Value))
(define BOTTOM-TYPE (Bottom-Type))
(define VOID-TYPE (Void-Type))
|#
(define INTxINT-TYPE (list INT-TYPE INT-TYPE))
(define INTxINT->BOOL-TYPE (Fn 2 INTxINT-TYPE BOOL-TYPE))
(define INTxINT->INT-TYPE (Fn 2 INTxINT-TYPE INT-TYPE))
(define ->UNIT-TYPE (Fn 0 '() UNIT-TYPE))

;; Are two types consistent at the top of the types?
(: shallow-consistent? (Any Any -> Boolean))
(define (shallow-consistent? t g)
  ;; Typed racket made me do it
  ;; This uber modular structure speeds up type checking
  (define (both-int? t g) : Boolean (and (Int? t)  (Int? g)))
  (define (both-bool? t g): Boolean (and (Bool? t) (Bool? g)))
  (define (both-fn? t g)  : Boolean
    (and (Fn? t)
         (Fn? g)
         (equal? (Fn-arity t)
                 (Fn-arity g))))
  (define (ref-shallow-consistent? t g) : Boolean
    (or (and (GRef? t) (GRef? g))
        (and (GVect? t) (GVect? g))
        (and (MRef? t) (MRef? t))
        (and (MVect? t) (MVect? t))))
  (or (Dyn? t)
      (Dyn? g)
      (both-int? t g)
      (both-bool? t g)
      (both-fn? t g)
      (ref-shallow-consistent? t g)))


(: completely-static-type? (-> Schml-Type Boolean))
;; Is the type t devoid of dyn?
(define (completely-static-type? t)
  ;; Typed Racket made me do it
  ;; This uber modular structure speeds up type checking
  (define (fn-completely-static? [t : Schml-Type]): Boolean
    (and (Fn? t)
         (andmap completely-static-type? (Fn-fmls t))
         (completely-static-type? (Fn-ret t))))
  (define (ref-completely-static? [t : Schml-Type])
    (or (and (GRef? t) (completely-static-type? (GRef-arg t)))
        (and (MRef? t) (completely-static-type? (MRef-arg t)))
        (and (GVect? t) (completely-static-type? (GVect-arg t)))
        (and (MVect? t) (completely-static-type? (MVect-arg t)))))
  (or (Int? t)
      (Bool? t)
      (fn-completely-static? t)
      (ref-completely-static? t)))


#|
   Unique Identifier
|#

(struct Uid ([prefix : String] [suffix : Natural]) #:transparent)

(define-type Uid* (Listof Uid))

(define FIRST-UID-SUFFIX 0)

(: uid->string (-> Uid String))
(define (uid->string u)
  ;; Rubout all non c identifier characters
  (: help (Char -> Char))
  (define (help c)
    (let ([n (char->integer c)])
      ;;        A-Z          a-z          0-9
      (if (or (<= 65 n 90) (<= 97 n 122) (<= 48 n 57))
          c
          #\_)))
  (string-append
   "u"
   (number->string (Uid-suffix u))
   "_"
   (list->string (map help (string->list (Uid-prefix u))))))

;; Are two uid equal?
(: uid=? (-> Uid Uid Boolean))
(define (uid=? [u : Uid] [v : Uid])
  ;; a little bit of a hack that shouldn't be noticable
  (or #;(eq? u v)
      (= (Uid-suffix u) (Uid-suffix v))))

;; If you are in the state monad you can purely allocate and increment
(define-type Nat Natural)
(: uid-state (String -> (State Nat Uid)))
(define (uid-state (name : String))
  (lambda ((s : Natural))
    (values (Uid name s) (add1 s))))

;; If you are in state passing style you
;; could use this to allocate and increment
(: next-uid (-> String Natural (values Uid Natural)))
(define (next-uid prefix suffix) next-uid
  (values (Uid prefix suffix) (add1 suffix)))

;; A simple macro for helpign with state passing style
;; I am trying to move away from this so that code can
;; be less verbose
(define-syntax let-uid*
  (syntax-rules ()
    [(_ (wrong ...) others ...) (raise-syntax-error 'let-uid "next must always be an identifier")]
    [(_ next () body ...)(let () body ...)]
    [(_ next ([name0 prefix0] [name* prefix*] ...) body ...)
     (let-values ([(name0 next) (next-uid prefix0 next)])
       (let-uid* next ([name* prefix*] ...)
		 body ...))]))




#|-----------------------------------------------------------------------------+
| Types shared by the Schml language family
+-----------------------------------------------------------------------------|#

(define-type Schml-Primitive (U Schml-Prim Schml-Prim!))

#;(: schml-primitive? (-> Any Boolean : Schml-Primitive))
#;
(define (schml-primitive? x)
  (or (schml-prim? x) (schml-prim!? x)))

(define-predicate schml-primitive? Schml-Primitive)

(define-type Schml-Prim
  (U IntxInt->Int-Primitive IntxInt->Bool-Primitive))

(define-predicate schml-prim? Schml-Prim)

#;(: schml-prim? (Any -> Boolean : Schml-Prim))
#;
(define (schml-prim? x)
  (or (IntxInt->Int-primitive? x)
      (IntxInt->Bool-primitive? x)))

(define-type Schml-Prim!
  (U Timer-Primitive))

(define-predicate schml-prim!? Schml-Prim!)
#;(: schml-prim!? (Any -> Boolean : Schml-Prim!))
#;
(define (schml-prim!? x)
  (or (timer-primitive? x)))

(define-type IntxInt->Int-Primitive (U '* '+ '-
                                        'binary-and 'binary-or 'binary-xor
                                        '%/ '%>> '%<<))

(define-type IxI->I-Prim IntxInt->Int-Primitive)

(define-predicate IntxInt->Int-primitive? IntxInt->Int-Primitive)
#;(: IntxInt->Int-primitive? (-> Any Boolean : IntxInt->Int-Primitive))
#;
(define (IntxInt->Int-primitive? x)
  
  #;(memq x '(* + - binary-and binary-or binary-xor %/ %>> %<<))
  )


(define-type IntxInt->Bool-Primitive (U '< '<= '= '> '>=))
(define-type IxI->B-Prim IntxInt->Bool-Primitive)
(define-predicate IntxInt->Bool-primitive? IntxInt->Bool-Primitive)

#;(: IntxInt->Bool-primitive? (Any -> Boolean : IntxInt->Bool-Primitive))
#;
(define (IntxInt->Bool-primitive? x)
  (memq x '(< <= = > >=))
  #;                     
  (or (eq? '< x)
      (eq? '<= x)
      (eq? '= x)
      (eq? '> x)
      (eq? '>= x)))

#|
(define-type IntxNon0->Int-Primitives '/)
(define-type Ix!0->I IntxNon0->Int-Primitives)
(define-predicate IntxNon0->Int-Prim? IntxNon0->Int-Primitives)

(define-type IntxNibble->Int-Primitives (U '<< '>>))
(define-type IxN->I IntxNibble->Int-Primitives)
(define-predicate IntxNibble->Int-Prim? IntxNibble->Int-Primitives)
|#

(define-type Timer-Primitive (U 'timer-start 'timer-stop 'timer-report))

(: timer-primitive? (Any -> Boolean : Timer-Primitive))
(define (timer-primitive? x)
  (or (eq? 'timer-start  x)
      (eq? 'timer-stop   x)
      (eq? 'timer-report x)))

#| Literals of the schml languages
   Only Integers and Booleans in the schml language are first
   class literal constants
|#

(define-type Schml-Literal
  (U Integer Boolean Null))

#;(: platform-integer? (Any -> Boolean : Integer))
#;
(define (platform-integer? x)
  (fixnum? x))

(: schml-literal? (Any -> Boolean : Schml-Literal))
(define (schml-literal? x)
  (or (exact-integer? x)
      (boolean? x)
      (null? x)))

(: schml-literal->type (Schml-Literal -> (U Bool Int Unit)))
(define (schml-literal->type x)
  (cond
    [(boolean? x) BOOL-TYPE]
    [(integer? x) INT-TYPE]
    [(null? x)    UNIT-TYPE]
    [else (error 'language/schml-literal->type "~a" x)]))

;; Types in the schml languages
(define-type  Base-Type (U Int Bool Unit))

(: base-type? (Any -> Boolean : Base-Type))
(define (base-type? x)
  (or (Int? x)
      (Bool? x)
      (Unit? x)))

(define-type+ Schml-Type ([Schml-Type* Listof]
			  [Schml-Type? Option])
  (U Dyn
     Base-Type
     Schml-Fn-Type
     Schml-Ref-Type))

(define-type Schml-Fn-Type
  (Fn Index Schml-Type* Schml-Type))

(define-type Schml-Ref-Type
  (U (GRef  Schml-Type)
     (GVect Schml-Type)
     (MRef  Schml-Type)
     (MVect Schml-Type)))

(define-type Atomic-Schml-Type (U Unit Int Bool Dyn))

(: schml-type? (Any -> Boolean : Schml-Type))
(define (schml-type? x)
  (or (Dyn? x)
      (base-type? x)
      (schml-fn? x)
      (schml-ref? x)))
      

(define-predicate schml-type*? Schml-Type*)
#;(: schml-type*? (Any -> Boolean : Schml-Type*))
#;
(define (schml-type*? x)
  (or (null? x)
      (and (pair? x)
           (schml-type? (car x))
           (schml-type*? (cdr x)))))

(define-predicate schml-fn? Schml-Fn-Type)
#;(: schml-fn? (Any -> Boolean : Schml-Fn-Type))
#;(define (schml-fn? x)
   (and (Fn? x)
    (index? (Fn-arity x))
    (schml-type*? (Fn-fmls x))
    (schml-type? (Fn-ret x))))


(define-predicate schml-ref? Schml-Ref-Type)
#;(: schml-ref? (Any -> Boolean : Schml-Ref-Type))

#;
(define (schml-ref? x)
  (or (and (GRef? x)  (schml-type? (GRef-arg x)))
      (and (GVect? x) (schml-type? (GVect-arg x)))
      (and (MRef? x)  (schml-type? (MRef-arg x)))
      (and (MVect? x) (schml-type? (MVect-arg x)))))

(define-type+ Schml-Fml ([Schml-Fml* Listof])
  (Fml Uid Schml-Type))

(define-type ConsistentT (Schml-Type Schml-Type . -> . Boolean))
(: consistent? ConsistentT)
(define (consistent? t g)
  ;; Typed racket made me structure the code this way.
  ;; TODO change the names to be both-unit? ...
  (: unit? ConsistentT)
  (define (unit? t g) (and (Unit? t) (Unit? g)))
  (: bool? ConsistentT)
  (define (bool? t g) (and (Bool? t) (Bool? g)))
  (: int? ConsistentT)
  (define (int? t g) (and (Int? t) (Int? g)))
  (: fn? ConsistentT)
  (define (fn? t g)
    (and (Fn? t) (Fn? g)
         (= (Fn-arity t) (Fn-arity g))
         (andmap consistent? (Fn-fmls t) (Fn-fmls g))
         (consistent? (Fn-ret t) (Fn-ret g))))
  (: gref? ConsistentT)
  (define (gref? t g)
    (and (GRef? t) (GRef? g)
         (consistent? (GRef-arg t) (GRef-arg g))))
  (: gvect? ConsistentT)
  (define (gvect? t g)
    (and (GVect? t) (GVect? g)
         (consistent? (GVect-arg t) (GVect-arg g))))
  (or (Dyn? t) (Dyn? g)
      (unit? t g)
      (bool? t g)
      (int? t g)
      (fn? t g)
      (gref? t g)
      (gvect? t g)
      


      ;; These will need to be added back but were adding
      ;; considerable type checking time.
      #;
      (and (GVect? t) (GVect? g)
           (consistent? (GVect-arg t) (GVect-arg g)))
      #;
      (and (MRef? t) (MRef? g)
           (consistent? (MRef-arg t) (MRef-arg g)))
      #;
      (and (MVect? t) (MVect? g)
           (consistent? (MVect-arg t) (MVect-arg g)))))

#|
 Join :: type X type -> type
 This is the join of the least precise latice
     ⊑ latice example:
      Int --> Int
         /   \
        /     \
       /       \        Joins ↑
Dyn --> Int Int --> Dyn
       \       /        Meets ↓
        \     /
         \   /
      Dyn --> Dyn
           |
          Dyn
|#

(: join (Schml-Type Schml-Type . -> . Schml-Type))
(define (join t g)
  (cond
    [(Dyn? t) g] [(Dyn? g) t]
    [(and (Unit? t) (Unit? g)) UNIT-TYPE]
    [(and (Int? t) (Int? g)) INT-TYPE]
    [(and (Bool? t) (Bool? g)) BOOL-TYPE]
    [(and (Fn? t) (Fn? g) (= (Fn-arity t) (Fn-arity g)))
     (Fn (Fn-arity t)
         (map join (Fn-fmls t) (Fn-fmls g))
         (join (Fn-ret t) (Fn-ret g)))]
    [(and (GRef? t) (GRef? g))
     (GRef (join (GRef-arg t) (GRef-arg g)))]
    [(and (GVect? t) (GVect? g))
     (join (GVect-arg t) (GVect-arg g))]
    #;
    [(and (MRef? t) (MRef? g))
     (join (MRef-arg t) (MRef-arg g))]
    #;
    [(and (MVect? t) (MVect? g))
    (join (MVect-arg t) (MVect-arg g))]
    [else (error 'join "Types are not consistent")]))


(define-forms
  (Coercion coercion)
  (Twosome type1 type2 lbl)
  ;; TODO Come up with a better name for this
  (Quote-Coercion const)
  (Compose-Coercions fst snd)
  ;; I am swithing to using the Cast and Interpreted Cast for the following
  ;; Forms
  ;(Coerce coercion expression) 
  ;(Interpreted-Coerce coercion expression)
  ;; Identity Cast
  ;; "Calculated No Op Cast"
  (Identity)
  (Id-Coercion-Huh E)
  (Id-Coercion)
  ;; Projection Coercion
  ;; "Project from dynamic at type blaming label if it fails"
  ;; G?ˡ in most papers
  ;; T?ˡ for this compiler
  (Project type label)
  ;; Injection Coercion
  ;; "Inject into the dynamic type at type"
  (Inject type)
  ;; Sequence Coercion
  ;; "Perform the first Coercion and then Another"
  (Sequence fst snd)
  ;; Fail Coercion
  ;; "Fail with the label"
  (Failed label)
  ;; Function Coercion
  ;; "Proxy a function call with args coercions and return coercion"
  ;; I am switching over to using the Fn form for this
  #;(Proxy-Fn arity args return)
  (Fn-Coercion-Arity c)
  (Fn-Coercion args return)
  (Fn-Coercion-Arg coercion index)
  (Fn-Coercion-Return coercion)
  ;; Guarded Reference Coercion
  ;; "Proxy a Guarded Reference's Reads and writes"
  (Ref read write)
  (Ref-Coercion read write)
  (Ref-Coercion-Read expression)
  (Ref-Coercion-Write ref)
  (Fn-Proxy arity coercion closure)
  (Fn-Proxy-Coercion expression)
  (Fn-Proxy-Closure expression)
  (Fn-Proxy-Huh expression)
  (Hybrid-Proxy apply closure coercion)
  (Hybrid-Proxy-Huh clos)
  (Hybrid-Proxy-Closure hybrid)
  (Hybrid-Proxy-Coercion hybrid)
  ;; Coercion Manipulation Stuff
  (Sequence-Coercion-Huh crcn)
  (Sequence-Coercion fst snd)
  (Sequence-Coercion-Fst seq)
  (Sequence-Coercion-Snd seq)
  (Project-Coercion type label)
  (Project-Coercion-Huh crcn)
  (Project-Coercion-Type prj)
  (Project-Coercion-Label prj)
  (Inject-Coercion type)
  (Inject-Coercion-Huh crnc)
  (Inject-Coercion-Type inj)
  (Failed-Coercion label)
  (Failed-Coercion-Huh crcn)
  (Failed-Coercion-Label fld)
  (Ref-Coercion-Huh crcn)
  (Fn-Coercion-Huh crcn)
  (Make-Fn-Coercion make-uid t1 t2 lbl)
  (Compose-Fn-Coercion comp-uid c1 c2))

(define-type Cast-Fml* (Listof Cast-Fml))
(define-type Cast-Fml (Fml Uid Schml-Type))

(define-type Cast-Literal (U Schml-Literal Blame-Label))

(define-type Src srcloc)

(define-type Tag-Symbol (U 'Int 'Bool 'Unit 'Fn 'Atomic 'Boxed 'GRef 'GVect))

(define-type Schml-Coercion
  (Rec C (U Identity
            (Project Schml-Type Blame-Label)
            (Inject Schml-Type)
            (Sequence C C)
            (Failed Blame-Label)
            (Fn Index (Listof C) C)
            (Ref C C))))

(define-type Schml-Coercion* (Listof Schml-Coercion))

(define-type Data-Literal (U Integer String))

#|------------------------------------------------------------------------------
  Compact Types are a sort of compile time hash-consing of types
  They are introduced by hoist types in Language Cast-or-Coerce3.1
------------------------------------------------------------------------------|#

(define-type Compact-Type
  (U (Fn Index (Listof Prim-Type) Prim-Type)
     (GRef Prim-Type) (MRef Prim-Type)
     (GVect Prim-Type) (MVect Prim-Type)))

(define-type Prim-Type (U Atomic-Schml-Type (TypeId Uid)))

(define-type Coercion/Prim-Type
  (Rec C (U Identity
            (Failed Blame-Label)
            (Project Prim-Type Blame-Label)
            (Inject Prim-Type)
            (Sequence C C)
            (Fn Index (Listof C) C)
            (Ref C C))))

(define-type Coercion/Prim-Type* (Listof Coercion/Prim-Type))
