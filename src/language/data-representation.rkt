#lang typed/racket
(require (for-syntax racket/syntax))
(require "../language/forms.rkt")
(provide (all-defined-out))

(define-syntax (define-constants stx)
  (syntax-case stx ()
    [(_ (name val) ...)
     (let ([fmt (lambda (x) (format-id stx "data:~a" x))])
       (with-syntax ([(fmt-name ...) (map fmt (syntax->list #'(name ...)))])
         #'(begin
             (define fmt-name val) ...
             (define name (Quote fmt-name)) ...)))]))

(define-constants
  ;; types
  (TYPE-TAG-MASK                  #b111)  
  ;; unallocated types
  (TYPE-ATOMIC-TAG #b111) ;; This should be TYPE-IMDT-TAG
  ;; Immediate types are tagged with #b111
  (TYPE-DYN-RT-VALUE   #b000111)
  (TYPE-INT-RT-VALUE   #b001111)
  (TYPE-BOOL-RT-VALUE  #b010111)
  (TYPE-UNIT-RT-VALUE  #b011111)
  (TYPE-FLOAT-RT-VALUE #b100111)
  (TYPE-CHAR-RT-VALUE  #b101111)
  (TYPE-MAX-ATOMIC-RT-VALUE #b111111)

  (HC-PRJ-TAG-MASK  #b10)
  (HC-INJ-TAG-MASK  #b01)
  (HC-TAG-MASK      #b11)
  (HC-T1-INDEX      0)
  (HC-LABEL-INDEX   1)
  (HC-T2-INDEX      2)
  (HC-MED-INDEX     3)
  ;; coercions
  (COERCION-TAG-MASK              #b111) ;; the same for primary and secondary tags
  ;; project coercion representation
  (COERCION-PROJECT-TAG           #b000)
  (COERCION-PROJECT-TYPE-INDEX    0)
  (COERCION-PROJECT-LABEL-INDEX   1)
  ;; inject coercion representation
  (COERCION-INJECT-TAG            #b001)
  (COERCION-INJECT-TYPE-INDEX     0)
  ;; sequence coercion representation
  (COERCION-SEQUENCE-TAG          #b010)
  (COERCION-SEQUENCE-FST-INDEX    0)
  (COERCION-SEQUENCE-SND-INDEX    1)
  ;; identity coercion representation
  (COERCION-IDENTITY-TAG          #b011)
  (COERCION-IDENTITY-IMDT         #b011)
  ;; fail coercion representation
  (COERCION-FAILED-TAG            #b110)
  (COERCION-FAILED-LABEL-INDEX    0)
  ;; mediating coercion representation
  (COERCION-SECOND-TAG-SHIFT      3)
  (COERCION-MEDIATING-TAG         #b100)
  ;; function coercion representation
  (COERCION-FN-SECOND-TAG         #b001)
  (COERCION-FN-ARITY-INDEX        0)
  (COERCION-FN-RETURN-INDEX       1)
  (COERCION-FN-FMLS-OFFSET        2)
  ;; tuple coercion representation
  (COERCION-TUPLE-SECOND-TAG      #b010)
  (COERCION-TUPLE-COUNT-INDEX     0)
  (COERCION-TUPLE-ELEMENTS-OFFSET 1)
  ;; monotonic coercion representation
  (COERCION-MREF-SECOND-TAG       #b011)
  (COERCION-MREF-TAG-INDEX        0)
  (COERCION-MREF-TYPE-INDEX       1)
  ;; monotonic vector coercion representation
  (COERCION-MVECT-SECOND-TAG      #b100)
  (COERCION-MVECT-TAG-INDEX       0)
  (COERCION-MVECT-TYPE-INDEX      1)
  ;; guarded coercion representation
  (COERCION-REF-SECOND-TAG        #b000)
  (COERCION-REF-TAG-INDEX         0)
  (COERCION-REF-READ-INDEX        1)
  (COERCION-REF-WRITE-INDEX       2)
  ;; values
  ;; simple dynamic value representation
  (DYN-TAG-MASK                   #b111)
  ;; Note: specify representation relies on the fact that boxes are tagged zero and
  ;; do not clear this tag. (If this changes that will also need to change)
  (DYN-BOXED-TAG                  #b000)
  (DYN-INT-TAG                    #b001)
  (DYN-BOOL-TAG                   #b111)
  (DYN-UNIT-TAG                   #b010)
  (DYN-CHAR-TAG                   #b011)
  (DYN-IMDT-SHIFT                 3)
  ;; allocated dynamic value representation
  (DYN-BOX-SIZE                   2)
  (DYN-VALUE-INDEX                0)
  (DYN-TYPE-INDEX                 1)
  ;; bool value representation
  (FALSE-IMDT                     #b000)
  (TRUE-IMDT                      #b001)
  ;; unit representation
  (UNIT-IMDT                      #b000)
  (DYN-UNIT-IMDT                  (bitwise-ior
                                   (arithmetic-shift data:UNIT-IMDT data:DYN-IMDT-SHIFT)
                                   data:DYN-UNIT-TAG)) 
  (GREP-TAG-MASK                  #b111)
  ;; 0
  (UNDEF-IMDT                     0)
  (ZERO-IMDT                      0)
  ;; guarded values representation
  (UGBOX-TAG                      #b000)
  (UGBOX-SIZE                     1)
  (UGBOX-VALUE-INDEX              0)
  (UGVECT-SIZE-INDEX              0)
  (UGVECT-OFFSET                  1)
  (GPROXY-TAG                     #b001)
  (GPROXY/TWOSOME-SIZE            4)
  (GPROXY/COERCION-SIZE           2)
  (GPROXY-COERCION-INDEX          1)
  (GPROXY-FOR-INDEX               0)
  (GPROXY-FROM-INDEX              1)
  (GPROXY-TO-INDEX                2)
  (GPROXY-BLAMES-INDEX            3)
  ;; monotonic values representation
  (MONO-RTTI-INDEX                0)
  (MBOX-SIZE                      2)
  (MBOX-VALUE-INDEX               1)
  (MBOX-TAG                       #b000)
  (MVECT-SIZE-INDEX               1)
  (MVECT-OFFSET                   2)
  (MVECT-TAG                      #b000)
  ;; function value representation
  (CLOS-CODE-INDEX                0)
  (CLOS-CSTR-INDEX                1)
  (CLOS-FVAR-OFFSET               2)
  (FN-TAG-MASK                    #b111)
  (CLOSURE-VALUE-MASK             -8) ;; signed long compliment of fn tag mask
  (FN-PROXY-TAG                   #b001)
  (FN-PROXY-CRCN-INDEX            1)
  (FN-PROXY-CLOS-INDEX            0)
  (HYBRID-PROXY-TAG               #b001)
  (HYBRID-PROXY-CRCN-INDEX        2)
  (HYBRID-PROXY-CLOS-INDEX        1))
