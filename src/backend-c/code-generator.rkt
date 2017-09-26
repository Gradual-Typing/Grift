#lang typed/racket

(require ;;racket/path
         "../configuration.rkt"
         "../language/data5.rkt"
         "../helpers.rkt"
         "./generate-c.rkt"
         syntax/location
         (for-syntax racket/system)
         (for-syntax "runtime-location.rkt")
         "runtime-location.rkt")

(provide (all-defined-out))

(define (find-unused-path [suffix : String]) : Path
  (let loop ([i 0])
    (define tmp-path
      (build-path (string-append "tmp" (number->string i) suffix)))
    (cond
      [(not (file-exists? tmp-path)) tmp-path]
      [else (loop (add1 i))])))

;; Basic driver for the entire backend
(: c-backend-generate-code (Data5-Lang . -> . Path))
(define (c-backend-generate-code uil) 
  (let* ([keep-c? (c-path)]
         [c-path (or keep-c? (find-unused-path ".c"))]
         [c-path (normalize-path c-path)]
         [o-path (or (output-path) (build-path "a.out"))]
         [o-path (normalize-path o-path)])
    ;; Write the C code to a file
    (logging c-backend-generate-code (Vomit) "~v" c-path)
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
         [hc?  (hashcons-path)]
         [flags (append-flags (c-flags))])
    (let* ([keep-s? (s-path)])
      (when keep-s?
        (system (format "cc ~a -S -o ~a ~a" in (path->string keep-s?) flags))))
    (cond
      [(and rt? hc?) (cc/runtime out in flags #:runtime rt? #:hashcons hc?)]
      [rt? (cc/runtime out in flags #:runtime rt?)]
      [hc? (cc/runtime out in flags #:hashcons hc?)]
      [else (cc/runtime out in flags)])
    o-path))




(: cc/runtime (->* (String String String) (#:runtime Path #:hashcons Path)
                   Void))
(define (cc/runtime out in flags
                    #:runtime [rt runtime.o-path]
                    #:hashcons [hc hashcons.o-path])
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
  (define cmd
    (format "clang -o ~a ~a ~a ~a ~a ~a ~a" out in rt hc rt-math gc flags))
  (when (trace? 'Vomit)
    (logf "System call: ~a" cmd))
  (flush-output (current-log-port))
  (flush-output (current-error-port))
  (flush-output)
  (unless (system cmd)
    (error 'schml/backend-c/invoke-c-compiler
           "failed to compile with: ~a"
           cmd)))

(: append-flags : (Listof String) -> String)
(define (append-flags s)
  (if (null? s)
      (default-flags)
      (with-output-to-string
        (lambda ()
          (let loop ([s s])
            (unless (null? s)
              (display (car s))
              (display " ")
              (loop (cdr s))))))))

;; C compiler warnings written with regard to clang
;; I need to check to make sure that these option will work with gcc
(define-type Opt-Warning (Parameter (Pair Boolean String)))

(define warn-format : Opt-Warning
  (make-parameter '(#f . "format")))

(define warn-int-conversion : Opt-Warning
  (make-parameter '(#f . "int-conversion")))

(define warn-unused-value : Opt-Warning
  (make-parameter '(#f . "unused-value")))




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
      (emit-opt-warning warn-unused-value))))
