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

(struct (U) Var
        ([ident : Uvar]
	 [annotation : U])
        #:transparent)

(struct (T U) Quote
	([const : T]
	 [annotation : U]))



#| Bindings and Formals |#
(struct (T) Fml ([identifier : Uvar] [type : T]))
(struct (T U) Bnd ([identifier : Uvar]
                   [type : U]
                   [expression : T]))

#| Types |#
(struct Int ())
(struct Bool ())
(struct Dyn ())
(struct (T U) Fn ([from : T]
                  [to : U]))

(define Int-Type (Int))
(define Bool-Type (Bool))
(define Dyn-Type (Dyn))


(struct Uvar
  ([prefix : String]
   [suffix : Natural])
  #:transparent)

(define (uvar=? [u : Uvar] [v : Uvar])
  (= (Uvar-suffix u) (Uvar-suffix v)))


        
