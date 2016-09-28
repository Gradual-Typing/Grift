#lang typed/racket/base
(provide (all-defined-out))

#|-----------------------------------------------------------------------------+
| The Constants for the representation of casts                                |
+-----------------------------------------------------------------------------|#
;; The Representation of functional types is an array
(define FN-TYPE-TAG #b000)
(define FN-ARITY-INDEX 0)
(define FN-RETURN-INDEX 1)
(define FN-FMLS-OFFSET 2)

;; The representation of tagged structure types
;; My thought is that types can be allocated statically
;; so there doesn't really need to be much though put
;; it may even be worth not tagging them and just laying
;; the types explicitly;
(define TYPE-TAG-MASK #b111)
(define TYPE-FN-TAG #b000)
(define TYPE-GREF-TAG #b001)
(define TYPE-GVECT-TAG #b010)
(define TYPE-MREF-TAG #b011)
(define TYPE-MVECT-TAG #b100)
;; Hypothetical extensions to type tags
;; Though more organization could le
;;(define TYPE-IARRAY-TAG #b101)
;;(define TYPE-MU-TAG #b110)

(define TYPE-ATOMIC-TAG #b111) ;; This should be TYPE-IMDT-TAG
;; Immediate types are tagged with #b111
(define TYPE-DYN-RT-VALUE #b0111)
(define TYPE-INT-RT-VALUE #b1111)
(define TYPE-BOOL-RT-VALUE #b10111)
(define TYPE-UNIT-RT-VALUE #b11111)

;; The representation of Dynamic Immediates
(define DYN-TAG-MASK  #b111)
(define DYN-IMDT-SHIFT 3)
(define DYN-BOXED-TAG #b000)
(define DYN-INT-TAG   #b001)
(define DYN-UNIT-TAG  #b010)
(define DYN-BOOL-TAG  #b111)

;; Boxed Dynamics are just a cons cell
(define DYN-BOX-SIZE 2)
(define DYN-VALUE-INDEX 0)
(define DYN-TYPE-INDEX 1)

;; Immediates
(define FALSE-IMDT #b000)
(define TRUE-IMDT #b001)
(define UNIT-IMDT #b000)
;; Unreachable Value
(define UNDEF-IMDT 0)

;; Guarded Representation
(define GREP-TAG-MASK #b111)
(define UGBOX-SIZE 1)
(define UGBOX-VALUE-INDEX 0)
(define UGBOX-TAG #b000)
(define GPROXY-TAG  #b001)
(define GPROXY/COERCION-SIZE 2)
(define GPROXY/TWOSOME-SIZE  4)
(define GPROXY-FOR-INDEX 0)
(define GPROXY-COERCION-INDEX 1)
(define GPROXY-FROM-INDEX 1)
(define GPROXY-TO-INDEX 2)
(define GPROXY-BLAMES-INDEX 3)
(define UGVECT-SIZE #f)
(define UGVECT-TAG #b000)
(define UGVECT-SIZE-INDEX 0)
(define UGVECT-OFFSET 1)

;; CastedValue Representation

(define CV-TAG-MASK #b111)
(define CASTEDVALUE-TAG #b010) ;; this tag should not conflict with any other tagged value
(define CASTEDVALUE/TWOSOME-SIZE 4)
(define CASTEDVALUE/COERCION-SIZE 1)
(define CASTEDVALUE-COERCION-INDEX 1)
(define CASTEDVALUE-FOR-INDEX 0)
(define CASTEDVALUE-FROM-INDEX 1)
(define CASTEDVALUE-TO-INDEX 2)
(define CASTEDVALUE-BLAMES-INDEX 3)

;; Monotonic Representation
(define MBOX-SIZE 2)
(define MBOX-VALUE-INDEX 0)
(define MBOX-RTTI-INDEX 1)
(define MBOX-TAG #b000) ;; no tags, one concrete value

(define MVECT-SIZE #f)
(define MVECT-SIZE-INDEX 0)
(define MVECT-RTTI-INDEX 1)
(define MVECT-OFFSET 2)
(define MVECT-TAG #b000) ;; no tags, one concrete value

;; GREF Type Representation
(define TYPE-GREF-SIZE  1)
(define GREF-TO-INDEX 0)

;; GVECT Type Representation
(define TYPE-GVECT-SIZE  1)
(define GVECT-TO-INDEX 0)

;; MRef Type Representation
(define TYPE-MREF-SIZE  1)
(define MREF-TO-INDEX 0)

;; MVECT Type Representation
(define TYPE-MVECT-SIZE  1)
(define MVECT-TO-INDEX 0)

;; Closure representation
(define CLOS-CODE-INDEX 0)
(define CLOS-CSTR-INDEX 1)
(define CLOS-FVAR-OFFSET 2)

;; Function Proxy Representation
(define HYBRID-PROXY-CRCN-SIZE 3)
(define HYBRID-PROXY-CODE-INDEX 0)
(define HYBRID-PROXY-CLOS-INDEX 1)
(define HYBRID-PROXY-CRCN-INDEX 2)
