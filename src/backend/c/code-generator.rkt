#lang typed/racket/no-check

(require 
 "../../configuration.rkt"
 "./convert-representation.rkt"
 "./generate-c.rkt"
 syntax/location
 (for-syntax racket/system)
 (for-syntax "../runtime-location.rkt")
 "../runtime-location.rkt"
 "../lib.rkt")

(provide (all-defined-out))

(define (find-unused-path [suffix : String]) : Path
  (make-temporary-file (format "tmp~~a~a" suffix)))

;; Basic driver for the entire backend
(define (generate-code ast)
  (let* ([uil (convert-representation ast)]
         [o-path (get-write-file (build-path "a") "" (output-path))]
         [keep-c? (ir-code-path)]
         [c-path (get-write-file (find-unused-path ".c") ".c" keep-c?)])
    ;; Write the C code to a file
    (with-output-to-file c-path #:mode 'text #:exists 'replace
      (lambda ()
        (generate-c uil c-path)))

    ;; if clang-format is present and we keep c then clean up the file
    (when keep-c?
      (define clang-format? (find-executable-path "clang-format"))
      (when clang-format?
        (system* clang-format? "-i"(path->string c-path))))

    ;; Invoke the system cc compiler on the file
    (invoke-c-compiler c-path o-path)

    (unless keep-c? (delete-file c-path))
    o-path))
  
;; call the host system's cc function
(: invoke-c-compiler (-> Path Path Path))
(define (invoke-c-compiler c-path o-path)
  (let* ([in   (path->string c-path)]
         [out  (path->string o-path)]
         [rt?  (runtime-path)] 
         [flags (append-flags (c-flags))])
    (let* ([keep-s? (s-path)])
      (when keep-s?
        (system (format "cc ~a -S -o ~a ~a" in (path->string keep-s?) flags))))
    (cond
      [rt? (cc/runtime out in flags #:runtime rt?)]
      [else (cc/runtime out in flags)])
    o-path))




(: cc/runtime (->* (String String String) (#:runtime Path
                                           #:runtime-entry Path)
                   Void))
(define (cc/runtime out in flags
                    #:runtime [rt runtime.o-path]
                    #:runtime-entry [entry ""])
  (define gc
    (match (garbage-collector)
      ['Boehm
       (define gc-link-path (path->string boehm-gc.a-path))
       (match (system-type 'os)
         ['unix (string-append  gc-link-path " -pthreads")]
         ['macosx gc-link-path]
         ['windows (error 'unsupported-os)])]
      ['None  (path->string none-gc.o-path)]))
  (define rt-math
    (match (system-type 'os)
     ['unix "-lm"]
     ['macosx ""]))
  (define rt-cast-profiler
    (if (cast-profiler?)
        (path->string cast-profiler.o-path)
        ""))
  (define debug-flag
    (cond
      [(with-debug-symbols) "-g"]
      [else ""]))
  (define cmd
    (string-join
     (list "clang -o"
           (if (path? out)   (path->string   out)   out)
           (if (path?  in)   (path->string    in)    in)
           (if (path? entry) (path->string entry) entry)
           (if (path?  rt)   (path->string    rt)    rt)
           rt-math gc debug-flag rt-cast-profiler flags)
     " "))
  (flush-output (current-error-port))
  (flush-output)
  (unless (system cmd)
    (error 'grift/backend/c/invoke-c-compiler
           "failed to compile with: ~a"
           cmd)))

(: append-flags : (Listof String) -> String)
(define (append-flags s)
  (if (null? s)
      (default-flags)
      ;; The loop that follows writes to a string using display
      ;; This avoids quadratic behavior of repeatedly appending strings.
      ;; This may be overkill as these strings are small in general,
      ;; but we will be deprecating this back end soon.
      (with-output-to-string
        (lambda ()
          (let loop ([s s])
            (unless (null? s)
              (display (car s))
              (display " ")
              (loop (cdr s))))
          (display (default-flags))))))

;; C compiler warnings written with regard to clang
;; I need to check to make sure that these option will work with gcc
(define-type Opt-Warning (Parameter (Pair Boolean String)))

(define warn-format : Opt-Warning
  (make-parameter '(#f . "format")))

(define warn-int-conversion : Opt-Warning
  (make-parameter '(#f . "int-conversion")))

(define warn-unused-value : Opt-Warning
  (make-parameter '(#f . "unused-value")))

(define warn-parentheses-equality : Opt-Warning
  (make-parameter '(#f . "parentheses-equality")))




(: emit-opt-warning (-> Opt-Warning Void))
(define (emit-opt-warning warning)
  (let* ([option (warning)]
         [switch (car option)]
         [flag   (cdr option)])
    (display "-W")
    (unless switch
      (display "no-"))
    (display flag)
    (display " ")))

(define (emit-optimize-c)
  (display " -O3 "))

(define (default-flags)
  (with-output-to-string
    (lambda ()
      (emit-optimize-c)
      (emit-opt-warning warn-format)
      (emit-opt-warning warn-int-conversion)
      (emit-opt-warning warn-unused-value)
      (emit-opt-warning warn-parentheses-equality))))
