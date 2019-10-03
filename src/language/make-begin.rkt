#lang racket/base

(require racket/match "forms.rkt")
(provide make-begin)


(define (make-begin eff* value)
  (define (splice-eff eff rest)
    (match eff
      [(No-Op) rest]
      [(Begin eff-eff* eff-value)
       (foldr splice-eff (splice-eff eff-value rest) eff-eff*)]
      [else (cons eff rest)]))
    (match* ((foldr splice-eff '() eff*) value) 
      [('() value) value]
      [((list eff) (No-Op)) eff] 
      [(eff* (Begin value-eff* value-value))
       (Begin (append eff* value-eff*) value-value)]
      [(eff* value) (Begin eff* value)]))


