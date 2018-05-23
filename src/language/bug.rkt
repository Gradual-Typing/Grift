#lang typed/racket

(struct foo ()
  #:type-name Foo)
(struct bar foo ())
