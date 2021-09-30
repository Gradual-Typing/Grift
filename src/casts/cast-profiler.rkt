#|
Author:      Deyaaeldeen Almahallawi (dalmahal@indiana.edu)
Description: Provides helpers for instrumenting the AST with cast profiling code
|#
#lang typed/racket/base/no-check

(require (for-syntax racket racket/syntax "../language/c-helpers.rkt")
         "../configuration.rkt"
         "../language/syntax.rkt"
         "../language/forms.rkt")

(provide (all-defined-out))


(define-syntax (define-counters stx)
  (syntax-case stx ()
    [(_ name ...)
     (let* ([single-ids (box '())]
            [max-ids (box '())]
            [total-ids (box '())]
            [uncasted-ids (box '())]
            [casts-ids (box '())]
            [uses-ids (box '())]
            [ls (syntax->list #'(name ...))]
            [projects-casts-str "projects_casts"]
            [injects-casts-str "injects_casts"]
            [ids (box `(,projects-casts-str ,injects-casts-str))]
            [fmt
            (lambda (f b)
              (lambda (x)
                (let ([var-str (format (string-replace f "-" "_") (syntax-e x))])
                  (set-box! ids (cons var-str (unbox ids)))
                  (set-box! b (cons var-str (unbox b)))
                  (cons (format-id stx f x) var-str))))])
       (with-syntax ([((single-use-name . single-use-val) ...)
                      (map (fmt "single-~a-proxies-accessed" single-ids) ls)]
                     [((max-use-name . max-use-val) ...)
                      (map (fmt "max-~a-proxies-accessed" max-ids) ls)]
                     [((total-use-name . total-use-val) ...)
                      (map (fmt "total-~a-proxies-accessed" total-ids) ls)]
                     [((value-use-name . value-use-val) ...)
                      (map (fmt "uncasted-~a-values" uncasted-ids) ls)]
                     [((casts-name . casts-val) ...)
                      (map (fmt "~a-casts" casts-ids) ls)]
                     [((uses-name . uses-val) ...)
                      (map (fmt "~a-uses" uses-ids) ls)]
                     [external-c-decl*
                      (format-id stx "~a" "cast-profiler/external-c-decl*")]
                     [projects-casts-id (format-id stx "~a" "projects-casts")]
                     [injects-casts-id (format-id stx "~a" "injects-casts")])
         (define decl*
           (string-append "int64_t " (string-join (unbox ids) ", ") ";\n"))
         (define r-total-ids (reverse (unbox total-ids)))
         (define r-uses-ids (reverse (unbox uses-ids)))

         (define (build-results-str call width sep)
           (define dw "30")
           (define w (number->string width))
           (define (build-printf-str desc-str expr*)
             (define printf-format-str (box '()))
             (define printf-args (box (list (string-append "\"" desc-str "\""))))
             (define (format-expr x)
               (define (arg-formatter x) (string-append "%" w x))
               (let ([expr (car x)] [t (cdr x)])
                 (match t
                   ;; a 64-bit integer expression 
                   ['int64
                    (begin
                      (set-box!
                       printf-format-str (cons (arg-formatter "\"PRId64\"")
                                               (unbox printf-format-str)))
                      (set-box! printf-args (cons expr (unbox printf-args))))]
                   ;; a string literal
                   ['string
                    (begin
                      (set-box!
                       printf-format-str (cons (arg-formatter "s")
                                               (unbox printf-format-str)))
                      (set-box! printf-args
                                (cons (string-append "\"" expr "\"")
                                      (unbox printf-args))))]
                   ;; a float expression
                   ['float
                    (begin
                      (set-box!
                       printf-format-str (cons (arg-formatter ".5f")
                                               (unbox printf-format-str)))
                      (set-box! printf-args (cons expr (unbox printf-args))))])))
             
             (for-each format-expr expr*)
             (string-append
              call "\"%" dw "s " sep
              (string-join (reverse (unbox printf-format-str)) sep) "\\n\", "
              (string-join (reverse (unbox printf-args)) ", ") ");\n"))
           (define na (cons "N/A" 'string))
           (define (tag-int64 l)
             (map (lambda (x) (cons x 'int64)) l))
           (string-append
            "  "
            (string-join
             (list
              (build-printf-str "types"
                                (map (lambda (x) (cons x 'string))
                                     (append (map symbol->string
                                                  (map syntax-e ls))
                                             (list "injects" "projects"))))
              (build-printf-str "total values allocated:"
                                (append (tag-int64 (reverse (unbox uncasted-ids)))
                                        (list na na)))
              (build-printf-str "total casts:"
                                (tag-int64 (append (reverse (unbox casts-ids))
                                                   (list injects-casts-str
                                                         projects-casts-str))))
              (build-printf-str "longest proxy chain:"
                                (append (tag-int64 (reverse (unbox max-ids)))
                                        (list na na)))
              (build-printf-str "total proxies accessed:"
                                (append (tag-int64 r-total-ids) (list na na)))
              (build-printf-str "total uses:"
                                (append (tag-int64 r-uses-ids) (list na na)))
              (build-printf-str "proxies accesses / value uses:"
                                (append (map (lambda (x) (cons x 'float))
                                             (map (lambda (x y)
                                                    (string-append
                                                     y " == 0 ? 0 : (float)" x
                                                     "/(float)" y))
                                                  r-total-ids r-uses-ids))
                                        (list na na))))
             "  ")))

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
              "extern " decl* "\n"
              "void print_cast_profiler_results();\n"
              "void write_cast_profiler_results_file(char*);\n"
              "#endif")
             in)))
         (create-cast-profiler.c
          (lambda (in)
            (displayln
             (string-append
              (string-join (list
                            "#include <inttypes.h>"
                            "#include <libgen.h>"
                            "#include <stdint.h>"
                            "#include <stdio.h>"
                            "#include <stdlib.h>"
                            "#include <string.h>"
                            "#include <unistd.h>"
                            "#include \"castprofiler.h\""
                            "\n\n")
                           "\n")

              "int64_t " (string-join (unbox ids) " = 0;\nint64_t ") " = 0;\n\n"

              "void print_cast_profiler_results() {\n"
              "  printf(\"-------------------------------------------"
              "Cast Profiler-------------------------------------------\\n\");\n"
              (build-results-str "printf(" -12 "")
              "}\n\n"

              "void write_cast_profiler_results_file(char* f) {\n"
              "  char* file_name;\n"
              "  char cwd[1024];\n"
              "  if (getcwd(cwd, sizeof(cwd)) == NULL)\n"
              "    fprintf(stderr,\"castprofiler.c/write_cast_profiler_results_"
              "file: getcwd failed!\\n\");\n"
              "  if((file_name = malloc(strlen(f)+strlen(cwd)+1+5+1)) != NULL) {\n"
              "    file_name[0] = '\\0';\n"
              "    strcat(file_name, cwd);\n"
              "    strcat(file_name, \"/\");\n"
              "    strcat(file_name, basename(f));\n"
              "    strcat(file_name, \".prof\");\n  } else {\n"
              "    fprintf(stderr,\"castprofiler.c/write_cast_profiler_results_"
              "file: malloc failed!\\n\");\n  }"
              "  FILE *h = fopen(file_name, \"w\");\n"
              "  if (h == NULL) {\n    printf(\"Error writing cast profiler "
              "results to disk!\\n\");\n    exit(1);\n  }\n"
              (build-results-str "fprintf(h, " 1 ",")
              "  fclose(h);\nfree(file_name);\n}"
              )
             in)))
         #`(begin
             (define single-use-name single-use-val) ...
             (define max-use-name max-use-val) ...
             (define total-use-name total-use-val) ...
             (define value-use-name value-use-val) ...
             (define casts-name casts-val) ...
             (define uses-name uses-val) ...
             (define projects-casts-id #,projects-casts-str)
             (define injects-casts-id #,injects-casts-str)
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

(define-syntax (cast-profile/max-function-chain$ stx)
  (syntax-case stx ()
    [(_ exp)
     #'(let ([e exp])
         (if (cast-profiler?)
             (begin$
               (If (op$ > (Global single-function-proxies-accessed)
                        (Global max-function-proxies-accessed))
                   (Assign max-function-proxies-accessed
                     (Global single-function-proxies-accessed))
                   NO-OP)
               (Assign single-function-proxies-accessed (Quote 0))
               e)
             e))]))

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

(define-syntax-rule (cast-profile/inc-injects-casts$ e)
  (cast-profile/inc-counter$ injects-casts e))

(define-syntax-rule (cast-profile/inc-projects-casts$ e)
  (cast-profile/inc-counter$ projects-casts e))

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
