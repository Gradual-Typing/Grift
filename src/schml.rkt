#lang typed/racket

(require "./compile.rkt")

(command-line
 #:program "schml"
 #:args (path)
 (cond
  [(not (string? path))
   (printf
    "schml: Expected an source code path as an argument got ~a" 
    path)
   (exit -1)]
  [else  (compile path) (void)]))
