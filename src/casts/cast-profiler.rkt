#|
Author:      Deyaaeldeen Almahallawi (dalmahal@indiana.edu)
Description: Provides helpers for instrumenting the AST with cast profiling code
|#
#lang typed/racket/base

(require (for-syntax racket racket/syntax "../language/c-helpers.rkt")
         "../configuration.rkt"
         "../language/syntax.rkt"
         "../language/forms.rkt")

(provide (all-defined-out))


(define-syntax (define-counters stx)
  (syntax-case stx ()
    [(_ name ...)
     (let* ([ids (box '())]
            [single-ids (box '())]
            [max-ids (box '())]
            [total-ids (box '())]
            [uncasted-ids (box '())]
            [casts-ids (box '())]
            [uses-ids (box '())]
            [fmt
            (lambda (f b)
              (lambda (x)
                (let ([var-str (format (string-replace f "-" "_") (syntax-e x))])
                  (set-box! ids (cons var-str (unbox ids)))
                  (set-box! b (cons var-str (unbox b)))
                  (cons (format-id stx f x) var-str))))])
       (with-syntax ([((single-use-name . single-use-val) ...)
                      (map (fmt "single-~a-proxies-accessed" single-ids) (syntax->list #'(name ...)))]
                     [((max-use-name . max-use-val) ...)
                      (map (fmt "max-~a-proxies-accessed" max-ids) (syntax->list #'(name ...)))]
                     [((total-use-name . total-use-val) ...)
                      (map (fmt "total-~a-proxies-accessed" total-ids) (syntax->list #'(name ...)))]
                     [((value-use-name . value-use-val) ...)
                      (map (fmt "uncasted-~a-values" uncasted-ids) (syntax->list #'(name ...)))]
                     [((casts-name . casts-val) ...)
                      (map (fmt "~a-casts" casts-ids) (syntax->list #'(name ...)))]
                     [((uses-name . uses-val) ...)
                      (map (fmt "~a-uses" uses-ids) (syntax->list #'(name ...)))]
                     [external-c-decl* (format-id stx "~a" "cast-profiler/external-c-decl*")])
         (define decl* (string-append "int64_t " (string-join (unbox ids) ", ") ";\n"))
         (define r-total-ids (reverse (unbox total-ids)))
         (define r-uses-ids (reverse (unbox uses-ids)))
         (create-cast-profiler.h
          (lambda (in)
            (displayln
             (string-append
              (string-join (list
                            "#ifndef CASTPROFILER_INCLUDED"
                            "#define CASTPROFILER_INCLUDED"
                            "#include <stdint.h>"
                            "\n\n")
                           "\n")
              decl* "\n"
              "void print_cast_profiler_results();\n"
              "#endif")
             in)))
         (create-cast-profiler.c
          (lambda (in)
            (displayln
             (string-append
              (string-join (list
                            "#include <stdint.h>"
                            "#include <inttypes.h>"
                            "#include <stdio.h>"
                            "#include \"castprofiler.h\""
                            "\n\n")
                           "\n")
              "int64_t " (string-join (unbox ids) " = 0;\nint64_t ") " = 0;\n\n"
              "void print_cast_profiler_results() {\n"
              "  printf(\"------------------------------Cast Profiler------------------------------\\n\");\n"
              "  printf(\"                               %-12s%-12s%-12s%-12s\\n\","
              (string-join (map (lambda (x) (string-append "\"" (symbol->string x) "\""))
                                (map syntax-e (syntax->list #'(name ...)))) ", ") ");\n"
              ;; "  printf(\"                               functions   vectors     refs        tuples\\n\");\n"
              "  printf(\"total values allocated:        %-12\"PRId64\"%-12\"PRId64\"%-12\"PRId64\"%-12\"PRId64\"\\n\","
              (string-join (reverse (unbox uncasted-ids)) ", ") ");\n"
              "  printf(\"total casts:                   %-12\"PRId64\"%-12\"PRId64\"%-12\"PRId64\"%-12\"PRId64\"\\n\","
              (string-join (reverse (unbox casts-ids)) ", ") ");\n"
              "  printf(\"longest proxy chain:           %-12\"PRId64\"%-12\"PRId64\"%-12\"PRId64\"%-12\"PRId64\"\\n\","
              (string-join (reverse (unbox max-ids)) ", ") ");\n"
              "  printf(\"total proxies accessed:        %-12\"PRId64\"%-12\"PRId64\"%-12\"PRId64\"%-12\"PRId64\"\\n\","
              (string-join r-total-ids ", ") ");\n"
              "  printf(\"total uses:                    %-12\"PRId64\"%-12\"PRId64\"%-12\"PRId64\"%-12\"PRId64\"\\n\","
              (string-join r-uses-ids ", ") ");\n"
              "  printf(\"proxies accesses / value uses: %-12.2f%-12.2f%-12.2f%-12.2f\\n\","
              (string-join (map (lambda (x y) (string-append y " == 0 ? 0 : (float)" x "/(float)" y))
                                r-total-ids r-uses-ids) ", ") ");\n"
              "}")
             in)))
         #`(begin
             (define single-use-name single-use-val) ...
             (define max-use-name max-use-val) ...
             (define total-use-name total-use-val) ...
             (define value-use-name value-use-val) ...
             (define casts-name casts-val) ...
             (define uses-name uses-val) ...
             (define external-c-decl* (string-append "extern " #,decl*))
             )))]))

(define-counters
  function
  vector
  ref
  tuple
  )

(define-syntax (cast-profile/max-chain$ stx)
  (syntax-case stx ()
    [(_ v1 v2 exp)
     #'(let ([e exp])
         (if (cast-profiler?)
             (let*$ ([v e])
               (If (op$ > (Global v1) (Global v2))
                   (Assign v2 (Global v1))
                   NO-OP)
               (Assign v1 (Quote 0))
               v)
             e))]))

(define-syntax-rule (cast-profile/max-function-chain$ e)
  (cast-profile/max-chain$
   single-function-proxies-accessed max-function-proxies-accessed e))

(define-syntax-rule (cast-profile/max-vector-chain$ e)
  (cast-profile/max-chain$
   single-vector-proxies-accessed max-vector-proxies-accessed e))

(define-syntax-rule (cast-profile/max-ref-chain$ e)
  (cast-profile/max-chain$
   single-ref-proxies-accessed max-ref-proxies-accessed e))

(define-syntax (cast-profile/inc-counter$ stx)
  (syntax-case stx ()
    [(_ v ... exp)
     #'(let ([e exp])
         (if (cast-profiler?)
             (begin$
               (Assign v (op$ + (Global v) (Quote 1))) ...
               e)
             e))]))

(define-syntax-rule (cast-profile/inc-function-casts$ e)
  (cast-profile/inc-counter$ function-casts e))

(define-syntax-rule (cast-profile/inc-function-proxies-accessed$ e)
  (cast-profile/inc-counter$
   total-function-proxies-accessed single-function-proxies-accessed e))

(define-syntax-rule (cast-profile/inc-uncasted-function-values$ e)
  (cast-profile/inc-counter$ uncasted-function-values e))

(define-syntax-rule (cast-profile/inc-function-uses$ e)
  (cast-profile/inc-counter$ function-uses e))

(define-syntax-rule (cast-profile/inc-ref-casts$ e)
  (cast-profile/inc-counter$ ref-casts e))

(define-syntax-rule (cast-profile/inc-ref-proxies-accessed$ e)
  (cast-profile/inc-counter$
   total-ref-proxies-accessed single-ref-proxies-accessed e))

(define-syntax-rule (cast-profile/inc-uncasted-ref-values$ e)
  (cast-profile/inc-counter$ uncasted-ref-values e))

(define-syntax-rule (cast-profile/inc-ref-uses$ e)
  (cast-profile/inc-counter$ ref-uses e))

(define-syntax-rule (cast-profile/inc-vector-casts$ e)
  (cast-profile/inc-counter$ vector-casts e))

(define-syntax-rule (cast-profile/inc-vector-proxies-accessed$ e)
  (cast-profile/inc-counter$
   total-vector-proxies-accessed single-vector-proxies-accessed e))

(define-syntax-rule (cast-profile/inc-uncasted-vector-values$ e)
  (cast-profile/inc-counter$ uncasted-vector-values e))

(define-syntax-rule (cast-profile/inc-vector-uses$ e)
  (cast-profile/inc-counter$ vector-uses e))

(define-syntax-rule (cast-profile/inc-tuple-casts$ e)
  (cast-profile/inc-counter$ tuple-casts e))

(define-syntax-rule (cast-profile/inc-uncasted-tuple-values$ e)
  (cast-profile/inc-counter$ uncasted-tuple-values e))

(define-syntax-rule (cast-profile/inc-tuple-uses$ e)
  (cast-profile/inc-counter$ tuple-uses e))
