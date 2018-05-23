#lang typed/racket/base



(require "types.rkt")

(require/typed
 "untyped-use.rkt"
   (t3 (T3 'A 'B 'C)))
