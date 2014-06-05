#lang racket

(require Schml/framework/pprint)

(define (lambda->doc o)
  (hang
   lambda-indent-size
   (doc-list
    (v-append
     (vs-append (text "lambda")
                (doc-list (vs-concat (map ->doc (Lambda-fmls o)))))
     (let ((ty (Lambda-ty o))
           (exp (->doc (Lambda-exp o))))
       (if ty
           (v-append (hs-append (->doc ty)) exp)
           exp))))))

(struct Lambda (fmls exp)
        #:methods gen:pretty
        [(define ->doc lambda->doc)]
        #:transparent)

(module+ type
  (require (prefix-in super: Schml/language/lambda))
  (require Schml/framework/pprint)
  (define (lambda->doc o)
    (hang
     lambda-indent-size
     (doc-list
      (v-append
     (vs-append (text "lambda")
                (doc-list (vs-concat (map ->doc (super:Lambda-fmls o)))))
     (let ((ty (Lambda-ty o))
           (exp (->doc (super:Lambda-exp o))))
       (if ty
           (v-append (hs-append (->doc ty)) exp)
           exp))))))
  (struct Lambda super:Lambda (ty)))

(module+ srcloc
  (require (prefix-in super: Schml/language/lambda))
  (struct Lambda super:Lambda (src)
          #:transparent
          #:methods gen:pretty
          [(define ->doc super:lambda->doc)]))
