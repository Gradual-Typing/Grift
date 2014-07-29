#lang typed/racket
(provide (all-defined-out))
#| The language forms |#

(struct (T U V W) Lambda 
        ([formals : (Listof T)]
         [return-type : U]
	 [body : V]
	 [annotation : W])
        #:transparent)

(struct (T U V) Letrec 
        ([bindings : (Listof T)]
         [body : U]
	 [annotation : V])
        #:transparent)

(struct (T U V) Let 
        ([bindings : (Listof T)]
         [body : U]
	 [annotation : V])
        #:transparent)

(struct (T U) App 
        ([operator : T]
         [operands : (Listof T)]
	 [annotation : U])
        #:transparent)

(struct (T U V) Op 
        ([primitive : T]
         [operands : (Listof U)]
	 [annotation : V])
        #:transparent)

(struct (T U) If 
        ([test : T]
         [then : T]
         [else : T]
	 [annotation : U])
        #:transparent)

(struct (T U V W) Ascribe 
        ([expression : T]
         [type : U]
         [label : V]
	 [annotation : W])
        #:transparent)

(struct (T U V W) Cast
	([expression : T]
	 [exp-type : U]
	 [cast-type : V]
	 [label : W])

(struct (U) Var
        ([ident : Uvar]
	 [annotation : U])
        #:transparent)

(struct (T U) Quote
	([const : T]
	 [annotation : U]))

#| I am making a more primitive match because current patern
   matching seems to erase types |#

#| The type of allowed literals |#

(define-type Literal 
  (U Integer Boolean))

(define-predicate Literal? 
  Literal)

(: literal->type (Literal -> (U Bool Int)))
(define (literal->type x)
  (if (boolean? x)
      Bool-Type
      Int-Type))

(define (int64? x)
  (and (integer? x)
       (>= x 0)
       (<= x (expt 2 64))))

#| The type of Primitives |#

(define-type Prim
  (U IntxInt->Int IntxInt->Bool))

(define-type IntxInt->Int
  (U '* '+ '- 'binary-and 'binary-or '<< '>>))

(define-type IntxInt->Bool
  (U '< '<= '= '> '>=))

(define-predicate IntxInt->Bool? IntxInt->Bool)
(define-predicate IntxInt->Int? IntxInt->Int)
(define-predicate Prim? Prim)



#| Bindings and Formals |#
(struct (T) Fml ([identifier : Uvar] [type : T]))
(struct (T U) Bnd ([identifier : Uvar]
                   [type : U]
                   [expression : T]))

#| Types |#
(struct Int ())
(struct Bool ())
(struct Dyn ())
(struct (T U) Fn ([fml : T] [ret : U]))
(struct (T U) Fn/a ([arity : Fixnum][fml : T] [ret : U]))
(define Int-Type (Int))
(define Bool-Type (Bool))
(define Dyn-Type (Dyn))




(define (shallow-consistent? t g)
  (or (Dyn? t)
      (Dyn? g)
      (and (Int? t) (Int? g))
      (and (Bool? t) (Bool? g))
      (and (Fn/a? t) (Fn/a? g))))

#| Unique Variables |#
(struct Uvar
  ([prefix : String]
   [suffix : Natural])
  #:transparent)

(define (uvar=? [u : Uvar] [v : Uvar])
  (= (Uvar-suffix u) (Uvar-suffix v)))

#| Maybe type |#

(define-type (Maybe T) (U T False)) 

#| Labels |#
(define-type Label String)

