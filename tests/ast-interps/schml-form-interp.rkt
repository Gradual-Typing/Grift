#lang racket

(require "../../src/language.rkt")

(define-syntax-rule (undefined who)
  (error 'who "is undefined right now"))

(module+ test (require rackunit))

(define label? string?) 



;; UD flavor Coercions

;; Types
(struct Int ())
(struct Bool ())
;; i = Int | Bool
(define (base-type? x)
  (or (Int? x) (Bool? x)))

;; A, B, C ::= i | Dyn | A -> B
(struct Dyn ())
(struct Fn (arity domain range))

(define ((fn-type? type?) x)
  (and (Fn? x)
       (exact-nonnegative-integer? (Fn-arity x))
       (andmap type? (Fn-domain x))
       (type? (Fn-range x))))

(define (type? x)
  (or (ground-type? x)
      (Dyn? x)
      ((fn-type? type?) x)))

;; G = i | * -> * 
(define (ground-type? x)
  (or (base-type? x) ((fn-type? Dyn?) x)))

(define-syntax-rule (andlet b . e) (let b (and . e)))

;; Coercions
;; c, d ::= id_A | G! | G?ᵖ | c -> d | c ; d | ⊥ᴳᵖᴴ

(define (coercion? x)
  (or ((id-coercion? type?) x)
      ((injection-coercion? type?) x)
      ((projection-coercion? type? label?) x)
      ((function-coercion? coercion?) x)
      ((composed-coercion? coercion? coercion?) x)
      (fail? x)))

;; Perform an Identity coercion for a specified type
(struct Id (type))

(define ((id-coercion? type?) x)
  (and (Id? x) (type? (Id-type x))))

;; Inject value of type into dynamic
(struct ! (type))

(define ((injection-coercion? type?) x)
  (and (!? x) (type? (!-type x))))

;; Project value of type out of dynamic
(struct ? (type label))

(define ((projection-coercion? type?) x)
  (and (?? x) (type? (?-type x))))

;; Coerce a function to another function type
(struct Fnc (arity domain range))

(define ((function-coercion? c?) x)
  (and (Fnc? x)
       (exact-nonnegative-integer? (Fnc-arity x))
       (andlet ([d (Fnc-domain x)])
         (list? d)
         (andmap c? d))
       (c? (Fn-range x))))

;; Represents doing a coercion and then another
(struct Composed (fst snd))

(define ((composed-coercion? fst? snd?) x)
  (and (Composed? x)
       (fst? (Composed-fst x))
       (snd? (Composed-snd x))))


;; A coercion that will fail when used
(struct Failed (type1 label type2))

(define ((failed-coercion? type1? label? type2?) x)
  (and (Fail? x)
       (type1? (Failed-type1 x))
       (type2? (Failed-type2 x))
       (label? (Failed-label x))))

(define fail? (failed-coercion? ground-type? label? ground-type?))

;; Coercions Normal Forms and Possible Specializations of Coercions
;; space efficient coercions
;; s, t = id* | (G?ᵖ ; i) | i

(define (space-efficient-coercion? x)
  (or (identity/dyn? x)
      (composed-projection? x)
      (intermediate-coercion? x)))

(define identity/dyn? (id-coercion? Dyn?))

(define composed-proj-intermediate?
  (compose-coercion? (proj-coercion? ground-type? label?) intermediate-coercion?))

;; intermediate coercions
;; i ::= (g ; G!) | g | ⊥ᴳᵖᴴ

(define (intermediate-coercion? x)
  (or (composed-ground-injection? x)
      (ground-coercion? x)
      (fail? x)))

(define composed-ground-injection?
  (compose-coercion? ground-coercion? (injection-coercion? ground-type?)))

;; ground coercions
;; g, h ::= idι | (s -> t) 
(define (ground-coercion? x)
  (or (id-coercion? base-type?)
      (fnc-coercion? normal-form-coercion?)))

;; Identity Free Coercions
;; f ::= (G?ᵖ ; i) | (g ; G!) | ⊥ᴳᵖᴴ | (s -> t)
(define (id-free-coercion x)
  (or (compose-proj-intermediate? x)
      (compose-ground-injection? x)
      (fail? x)
      ((fnc-coercion normal-form-coercion?) x)))

;; s & t = r
(define (compose c1 c2)
  (match* (c1 c2)
    ;; Idι & Idι = Idι
    [((Id i1) (Id i2)) #:when (equal? i1 i2) c2]

    ;; Id* & t = t
    [((Id (Dyn)) _) c2]

    ;; (s -> t) & (s' -> t') = (s' & s) -> (t & t')
    [((Fn n1 s1* t1) (Fn n2 s2* t2)) #:when (= n1 n2)
     (Fn n1 (map compose s2* s1*) (compose t1 t2))]

    ;; (g ; G!) & Id* = (g ; G!)
    [((Composed g (! G)) (Id (Dyn))) c1]
    
    ;; (G?ᵖ; i) & t = (G?ᵖ ; (i & t))
    [((Composed (and fst (? G p)) i) t)
     ;; no need to check if i is an intermediate
     (Composed fst (compose i t))]

    ;; g & (h ; H!) = (g & h) ; H!
    [(g (Composed h (and snd (! H)))) #:when (ground-coercion? g)
     (Composed (compose g h) snd)]

    ;; The following two lines cannot be swapped
    ;; (g ; G!) & (G?ᵖ ; i) = g & i
    [((Composed g (! G1)) (Composed (? G2 p) i)) #:when (equal? G1 G2)
     (compose g i)]

    ;; (g; G!) & (H?ᵖ ; i) = ⊥ᴳᵖᴴ
    [((Composed _ (! G)) (Composed (? H p) _))
     (Failed G p H)]

    ;; Failures are idempotent
    ;; ⊥ᴳᵖᴴ & s = ⊥ᴳᵖᴴ
    [((and fail (Failed _ _ _)) _) fail]
    ;; g ; ⊥ᴳᵖᴴ = ⊥ᴳᵖᴴ
    [(_ (and fail (Failed _ _ _))) fail]))


(module+ test
  (check-equal? (compose (Id (Int)) (Id (Int))) (Id (Int))))




