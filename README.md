Prog = Texpr Texpr ...
Texpr = Decl | Expr
Decl = (define ID expr)
ID = 

Imediate types = Dyn | Symbol | Char | Fixnum | Flonum | Bool 
Composite types = (List T) | (Array T) | (Ref T) |  | ( ) | (Fun (T ...) (T ...)) | ($ Type) 
Expr = (type ID Type)
     | (module ....) 
     | (data ((ID (ID Type) ...) ...)) records syntax for structured types
     | (data ((ID Type ...) ...))      unions for structured types
     | (record (ID (ID Type) ...)      record syntax for reference types
     - Type Declarations
     | (:- Expr Type)
     | (-: Type Expr)   
     - Binding Forms
     | (let ((pat type exp)) TExpr ... Expr ...)
     | (let ((PAT expr) ...) TExpr ... Expr ...)
     | (letrec ((pat type exp)) TExpr ... Expr ...)
     | (letrec ((PAT expr) ...) TExpr ... Expr ...)
     | (lambda ((ID type) ...) TExpr ... Expr ...)
     | (lambda (ID ...) TExpr ... Expr ...) | (Expr Expr ...)
     - conidtional branching
     | (case Expr (Pat Expr ...) ...)
     | #t | #f | (if Expr Expr Expr) | (or Expr ...) | (and Expr ...)  | (not Expr) | Boolean?
     | eq? | equal?
     - reference types
     | #<box > | (box Expr) | (set! Expr) | (unbox Expr) | box?
     | #{ } | (make-array Expr)  | (array-ref expr expr) | (array-set! expr expr) 
     | (array-length Expr) | (array? Expr)
     | #<id field-id ...> | (make-id Expr ...) | (id-field-set! Expr Expr) 
     | (id-field Expr) | (id? Expr)
     | #" " | (string-set! expr expr expr) | (string? Expr) | (string-length Expr)
     - numerics though this could be generalized with a typeclass
     | {d}+.{d} | (+. Expr Expr) | (-. Expr Expr) | (*. Expr Expr) | (/. Expr Expr) | Flonum?
     | flo< | flo<= | flo= | flo> | flo>= 
     | {d} | #x{x}+ | #b{b}+ | (+ Expr Expr) | (- Expr Expr) | (* Expr Expr) 
     | (/ Expr Expr) | Fixnum? | fx< | fx<= | fx= | fx> | fx>=
     | < | <= | = | > | >=
     - numerics
     | #\  | (char->num Expr) | (ord Expr) | display | write | printf
     - lists via quasiquote, ";" (strong unquote), "," (dynamic unquote), 
     | (let ((b 5))  (:- `(1 ;b 3) (List Fixnum))) => ok
     | (let ((b int 5))  (:- `(a ,b c) (list dyn))) => ok
     | (let ((b #t)) (:- `(1 ;b 3) (List Fixnum))) => error
     | (let ((b dyn 5)) (:- `(a ,b c) (list dyn))) => ok
     | (let ((b int 5)) (:- '(a b c) (list Symbol))) => ok
     | (:- (cons a b c) (list dyn)) => #()
     - Tuples 
     | #() | (tuple? Expr) | (-: (X Fixnum Bool) #((* 2 3) 5.5)) => #()
     | (fixnum? '1) -> #t | (symbol? '1) -> #f 
     - comments
     | ## line comment
     | #eof end of file comment
     | #| |# enclosing comment that may be nested
     | support for partial application of functions leading to implicit currying
     