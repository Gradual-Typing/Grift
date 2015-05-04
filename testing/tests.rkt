#lang typed/racket/no-check ;; Not actually type-checked

(require rackunit
         rackunit/text-ui
	 schml/testing/values
         schml/testing/test-compile
         schml/testing/paths
         schml/src/helpers)

(provide (all-defined-out))

;; sets up a variable with the path to the test suite

(define-syntax test-file
  (syntax-rules ()
    ((_ p ... n e)
     (ann (test-case n (test-compile (simplify-path (build-path test-suite-path p ... n)) e)) Test))))


(define test-data-base : Test
  (test-suite
   "all tests"
   ;; Bools
   (test-file "const-false.schml" (bool #f))
   (test-file "const-true.schml"  (bool #t))
   ;; Ints
   (test-file "const-one.schml"   (int 1))
   (test-file "const-ninetynine.schml" (int 99))
   (test-file "const-larg-int.schml" (int 123456))
   ;; Primitive operators TODO (should add more to test corners)
   (test-file "prim-plus.schml"  (int 10))
   (test-file "prim-minus.schml" (int 10))
   (test-file "prim-times.schml" (int 10))
   (test-file "prim-divides.schml" (int 10))
   (test-file "prim-band.schml" (int 10))
   (test-file "prim-bor.schml" (int 10))
   (test-file "prim-shiftl.schml" (int 10))
   (test-file "prim-shiftr.schml" (int 10))

   ;; Primitive relational operators should test more corner cases
   (test-file "prim-eq.schml" (bool #t))
   (test-file "prim-lt.schml" (bool #t))
   (test-file "prim-gt.schml" (bool #t))
   (test-file "prim-le.schml" (bool #t))
   (test-file "prim-ge.schml" (bool #t))

   ;; If
   (test-file "if0.schml" (int 0))
   (test-file "if1.schml" (int 0))
   (test-file "if2.schml" (int 1))
   (test-file "if3.schml" (int 4))

   ;; Ascription
   (test-file "int-dyn-int.schml"   (int 1))
   (test-file "ascribe-dyn.schml"       (dyn))
   (test-file "ascribe-int-good.schml"  (int 10))
   (test-file "ascribe-bool-good.schml" (bool #t))
   (test-file "ascribe-int-bad.schml"   (blame #t "Pass"))
   (test-file "ascribe-bool-bad.schml"  (blame #t "Pass"))

   ;; Lambda
   (test-file "lambda1.schml" (function))
   (test-file "lambda2.schml" (function))
   (test-file "lambda3.schml" (function))
   (test-file "lambda4.schml" (function))
   (test-file "lambda5.schml" (function))
   (test-file "lambda6.schml" (function))
   (test-file "lambda7.schml" (function))
   (test-file "lambda8.schml" (function))

   ;; Let
   (test-file "let0.schml" (bool #t))
   (test-file "let1.schml" (int 2))
   (test-file "let2.schml" (bool #f))
   (test-file "let3.schml" (int 1))
   (test-file "let4.schml" (int 2))
   (test-file "let5.schml" (int 4))
   (test-file "let6.schml" (int 0))
   (test-file "let7.schml" (dyn))
   (test-file "let8.schml" (int 0))
   (test-file "let9.schml" (int 100))
   (test-file "let10.schml" (dyn))
   (test-file "let11.schml" (bool #f))
   (test-file "let12.1.schml" (dyn))
   (test-file "let12.2.schml" (dyn))
   (test-file "let12.3.schml" (blame #f #f))
   (test-file "let12.schml" (dyn))
   (test-file "let13.schml" (dyn))
   (test-file "let14.schml" (int 5))
   (test-file "let15.schml" (function))
   (test-file "let16.schml" (int 7))
   (test-file "let17.schml" (bool #f))
   (test-file "let18.schml" (bool #f))
   (test-file "let19.schml" (bool #f))
   (test-file "let20.schml" (bool #t))

   ;; Letrec
   (test-file "letrec1.schml" (dyn))
   (test-file "letrec2.schml" (function))
   (test-file "letrec3.schml" (dyn))
   (test-file "letrec4.schml" (bool #t))
   (test-file "letrec5.schml" (bool #t))
   (test-file "letrec6.schml" (int 1))

   ;; Gaurded Boxes
   (test-file "gbox0.schml" (gbox))
   (test-file "gbox1.schml" (gbox))
   (test-file "gbox2.schml" (int 2))
   (test-file "gbox3.schml" (int 3))
   (test-file "gbox4.schml" (int 4))
   (test-file "gbox5.schml" (dyn))
   (test-file "gbox6.schml" (int 6))
   (test-file "gbox7.schml" (int 7))
   (test-file "gbox8.schml" (bool #t))
   (test-file "gbox9.schml" (bool #f))
   ;;

   ;; Are we blaming the correct label
   (test-file "blame1.schml" (int 2))
   (test-file "blame2.schml" (blame #t "Right"))
   (test-file "blame3.schml" (blame #f "Correct"))
   (test-file "blame4.schml" (blame #t #f))
   (test-file "blame5.schml" (blame #f (not-lbl "Fail")))
   (test-file "blame6.schml" (blame #f (not-lbl "Fail")))
   (test-file "blame7.schml" (int 2))
   (test-file "blame8.schml" (blame #t #f))
   (test-file "blame9.schml" (blame #t "Pass"))
   ;; Multi  arg function
   (test-file "blame10.schml" (blame #f (not-lbl "Fail")))
   (test-file "blame11.schml" (blame #f (not-lbl "Fail")))
   (test-file "blame12.schml" (blame #f "Pass"))
   (test-file "blame13.schml" (blame #f "Pass"))

   ;; basic computations that may make it to being bench marks
   ;; factorial
   (test-file "fact5.schml" (int 120))
   (test-file "fact7.schml" (int 5040))
   (test-file "fact10.schml" (int 3628800))

   (test-file "fact-dyn-6.schml" (int 720))
   (test-file "fact-static-6.schml" (int 720))
   ;; even and odd
   (test-file "odd-20-static.schml" (bool #f))
   (test-file "odd-20-hybrid1.schml" (bool #f))
   (test-file "odd-20-hybrid2.schml" (bool #f))
   (test-file "odd-20-hybrid3.schml" (bool #f))
   (test-file "odd-20-hybrid4.schml" (bool #f))
   (test-file "odd-20-hybrid5.schml" (bool #f))
   (test-file "odd-20-dynamic.schml" (dyn))
   (test-file "even-odd-cps-herman.schml" (bool #t))
   ;; ackermans these are too long for the test suite
   (test-file "ack-1-2-static.schml" (int 4))
   (test-file "ack-2-3-static.schml" (int 9))
   ;; These are too long perhaps make an long flag or something
   ;;(test-file "ack-3-10-static.schml" (int 125))
   ;;(test-file "ack-4-1-static.schml"  (int 65533))
   ;;(test-file "ack-static.schml"      (bool #t))
   ))



(: find-test* (-> Path (Listof Test) (Listof Test)))
(define (find-test* path tests)
  (cond
    [(schml-file? path) (cons (make-compiler-test path) tests)]
    [(directory-exists? path)
     (let* ([name (file-name-from-path path)]
            [name (if name (path->string name) "Unkown Test")])
       (cons (make-test-suite
              name
              (foldl find-test* '() (get-directory path)))
             tests))]
    [else tests]))

(define (schml-file? (path : Path)) : Boolean
  (let ([ext (filename-extension path)])
    (if ext (bytes=? ext #"schml") #f)))

(define (make-compiler-test (path : Path)) : Test
  (let ([path (simplify-path path)])
    (let-values ([(base name dir) (split-path path)])
      (if (and (path? base) (path? name))
          (let ([test (test-case "hi" (test-compile path (dyn)))])
            test)
          (error 'make-compiler-test "~a ~a" base name)))))

;; like directory-list but with full file paths
(: get-directory (-> Path (Listof Path)))
(define (get-directory path)
  (for/list : (Listof Path) ([file : Path (in-directory path)])
    (build-path path file)))

#| The main of this file is inlined |#

;; make sure that there is an unobtrusive place for temp files
(unless (directory-exists? test-tmp-path)
    (make-directory test-tmp-path))

(call-with-output-file
    (build-path test-tmp-path "t.log.txt") #:exists 'replace
  (lambda ([f : Output-Port])
    (parameterize ([compiler-config (Config 'Lazy-D
                                            (build-path test-tmp-path "t.out")
                                            (build-path test-tmp-path "t.c")
                                            '())]
                   [current-log-port f]
                   ;;[traces '(All Vomit)]
                   ;;[current-error-port f]
                   )
      (command-line #:program "schml-test-suite"
                    #:args ()
                    (run-tests test-data-base)))))
