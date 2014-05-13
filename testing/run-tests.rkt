#lang racket
(require rackunit rackunit/text-ui
         Schml/compiler/compile)
         
(define tests
  (test-suite "compiler"
    (let ()
      (local-require Schml/framework/compiler/compile)
      (test-not-exn "1" (lambda () (compile #:file "1.schml" 
                                       #:debug-passes 'all
                                       #:cast-semantics 'ld)))
      (test-not-exn "2" (lambda () (compile #:file "2.schml" 
                                       #:debug-passes 'all
                                       #:cast-semantics 'ld)))
      (test-not-exn "3" (lambda () (compile #:file "3.schml" 
                                       #:debug-passes 'all
                                       #:cast-semantics 'ld)))
      (test-not-exn "4" (lambda () (compile #:file "4.schml" 
                                       #:debug-passes 'all
                                       #:cast-semantics 'ld)))
      (test-not-exn "5" (lambda () (compile #:file "5.schml" 
                                       #:debug-passes 'all
                                       #:cast-semantics 'ld))))))

(module+ main
  (run-tests tests))

     
