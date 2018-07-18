#lang typed/racket/base

(provide
 ;; A curried varient of map
 cmap)

(: cmap (All (A B) (A -> B) -> ((Listof A) -> (Listof B))))
(define ((cmap f) xs) (map f xs))
