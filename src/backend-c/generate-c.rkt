#lang typed/racket
#|------------------------------------------------------------------------------+
|pass: src/generate-c
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
| Description:
+-------------------------------------------------------------------------------+
| Grammer:
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require "../helpers.rkt"
         "../errors.rkt"
         "../configuration.rkt"
         "../language/data5.rkt"
         "../macros.rkt"
         "runtime-location.rkt")

;; Only the pass is provided by this module
(provide generate-c)

(define display-mem-statistics? : (Parameterof Boolean)
  (make-parameter #f))

(define timer-uses-process-time? : (Parameterof Boolean)
  (make-parameter #f))

(: IMDT-C-TYPE String)
(define IMDT-C-TYPE "int64_t")

(: ~exit : Any -> String)
(define (~exit error)
  (format "exit(~a)" error))

(define C-EXIT : String (~exit "EXIT_FAILURE"))
(define ERROR-OUT-OF-MEMORY : String "EXIT_FAILURE")
(define ERROR-INDEX-OUT-OF-BOUNDS : String "EXIT_FAILURE")
(define ERROR-CAST-ERROR : String "EXIT_FAILURE")

(: C-INCLUDES (Listof String))
;; The runtime head plus any requirements that change based on how we
;; compile the program.
(define C-INCLUDES
  (let ([timer (if (timer-uses-process-time?)
                   "<time.h>"
                   "<sys/time.h>")]
        [runtime (format "\"~a\"" runtime.h-path)])
    (list timer runtime)))

;; TODO consider if this in needed (runtime.h provides this declaration)
(: C-DECLARATIONS (Listof String))
(define C-DECLARATIONS
  '("int64_t read_int();"))

(: generate-c (-> Data5-Lang Path Void))
(define (generate-c prgm out-path)
  (match-let ([(Prog (list name count type) (GlobDecs d* (Labels lbl* exp))) prgm])
    (call-with-output-file out-path #:exists 'replace #:mode 'text
      (lambda ([p : Output-Port])
        (parameterize ([current-output-port p])
          (emit-program name type lbl* d* exp))))))

(: emit-program (-> String Schml-Type D5-Bnd-Code* (Listof Uid) D5-Body Void))
(define (emit-program name type code* d* body)
  (emit-source-comment name type)
  (emit-boiler-plate)
  (emit-var-declarations d*)
  (emit-declarations code*)
  (emit-main body)
  (emit-subroutines code*))


(: initialize-garbage-collector (-> Void))
(define (initialize-garbage-collector)
  (match (garbage-collector)
    ['Boehm (display "GC_INIT();\n")]
    ['None (printf "nonegc_init(~a);\n" (* (init-heap-kilobytes) 1024))]))


;; TODO make these C Macros
(define-values (timer-start timer-stop timer-report timer-boiler-plate)
  (cond
    [(timer-uses-process-time?)
     (values
      "timer_started = clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &timer_start_time)"
      "timer_stopped = clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &timer_stop_time)"
      "timer_report()"
      (concat-string-literal
       "//This is the global state for the timer\n"
       "struct timespec timer_start_time;\n"
       "struct timespec timer_stop_time;\n"
       "struct timespec timer_result_time;\n"
       "int timer_started = 1;\n"
       "int timer_stopped = 1;\n\n"
       "void timer_report(){\n\n"
       "    // some very minor error checking\n"
       "    if(timer_started){\n"
       "        printf(\"error starting timer\");\n"
       "        exit(-1);\n"
       "    }\n"
       "    if(timer_stopped){\n"
       "        printf(\"error stopping timer\");\n"
       "        exit(-1);\n"
       "    }\n\n"
       "double t1 = timer_start_time.tv_sec + (timer_start_time.tv_nsec / 1.0e9);\n"
       "double t2 = timer_stop_time.tv_sec  + (timer_stop_time.tv_nsec  / 1.0e9);\n"
       "printf(\"time (sec): %lf\\n\", t2 - t1);\n"
       "}\n"))]
    [else
     (values
      "timer_started = gettimeofday(&timer_start_time, NULL)"
      "timer_stopped = gettimeofday(&timer_stop_time, NULL)"
      "timer_report()"
      (concat-string-literal
       "//This is the global state for the timer\n"
       "struct timeval timer_start_time;\n"
       "struct timeval timer_stop_time;\n"
       "struct timeval timer_result_time;\n"
       "int timer_started = 1;\n"
       "int timer_stopped = 1;\n\n"
       "void timer_report(){\n\n"
       "    // some very minor error checking\n"
       "    if(timer_started){\n"
       "        printf(\"error starting timer\");\n"
       "        exit(-1);\n"
       "    }\n"
       "    if(timer_stopped){\n"
       "        printf(\"error stopping timer\");\n"
       "        exit(-1);\n"
       "    }\n\n"
       "double t1 = timer_start_time.tv_sec + (timer_start_time.tv_usec / 1.0e6);\n"
       "double t2 = timer_stop_time.tv_sec + (timer_stop_time.tv_usec / 1.0e6);\n"
       "printf(\"time (sec): %lf\\n\", t2 - t1);\n"
       "}\n"))]))


(define (emit-source-comment name type)
  (printf "// ~a should return ~a generated by the schml compiler.\n" name type))

(: emit-boiler-plate (-> Void))
(define (emit-boiler-plate)
  (display "\n//This is the boiler plate\n")
  (for ([i : String C-INCLUDES])
    (printf "#include ~a\n" i))
  (match (garbage-collector)
    ['Boehm
     (display
      (string-append
       (format "#define GC_INITIAL_HEAP_SIZE ~a\n" (* (init-heap-kilobytes) 1024))
       (format "#include \"~a\"\n" boehm-gc.h-path)))]
    ['None (printf "#include \"~a\"" none-gc.h-path)])
  ;; TODO remove this and C-DECLATIONS definition if this works
  #;(newline)
  #;(for ([d : String C-DECLARATIONS])
      (display d)
      (newline))
  (newline)
  (display timer-boiler-plate)
  (newline))

(: emit-var-declarations (-> (Listof Uid) Void))
(define (emit-var-declarations d*)
  (display "\n//These are the variable declarations\n")
  (display "\ntable types_ht;\nint64_t types_unique_index_counter;")
  (display-seq (map uid->string d*) "" (string-append IMDT-C-TYPE " ") "" ";\n" ""))

(: emit-declarations (-> D5-Bnd-Code* Void))
(define (emit-declarations code*)
  (display "\n//These are the declarations\n")
  (for : Void ([bnd : D5-Bnd-Code code*])
       (match-let ([(cons lbl (Code var* exp)) bnd])
         (emit-function-prototype IMDT-C-TYPE (uid->string lbl) (map uid->string var*))
         (display ";\n"))))

(: initialize-types-table (-> Void))
(define (initialize-types-table)
  (printf "types_ht = alloc_hash_table(~a, ~a);\n"
          (init-types-hash-table-slots) (types-hash-table-load-factor))
  (display "types_unique_index_counter = 0;"))

(: emit-main (-> D5-Body Void))
(define (emit-main b)
  (display "\n//Obviously this is the main function\n")
  (match-let ([(Locals local-var* tail) b])
    (emit-function-prototype "int" "main" '())
    (let ([local-var* (map uid->string local-var*)])
      (display "{\n")
      (display-seq local-var* "" (string-append IMDT-C-TYPE " ") "" ";\n" "")
      (initialize-garbage-collector)
      (initialize-types-table)
      (newline)
      (emit-main-tail tail)
      (when (display-mem-statistics?)
        (display "printf(\"%lu bytes allocated\\n\",allocd_mem);\n"))
      (display "return 0;")
      (display "}"))
    (newline)
    (newline)))

;; This is dumb why do we have this and emit tail -andre
(: emit-main-tail (-> D5-Tail Void))
(define (emit-main-tail tail)
  (logging emit-tail (Vomit) tail)
  (match tail
    [(Begin s* t)
     (begin (for ([s : D5-Effect s*])
              (emit-effect s)
              (display ";\n"))
            (emit-main-tail t))]
    [(If t c a)
     (begin (display "if ")
            (emit-pred t)
            (emit-block '() c)
            (display " else ")
            ;; emit-block calls emit-tail which will cause problems if
            ;; main branches around return
            (emit-block '() a)
            (newline))]
    [(Switch t c* d)
     (begin (display "switch (") (emit-value t) (display "){\n")
            (for ([c c*])
              (let loop : Void ([l* (car c)])
                   (match l* 
                     [(list l)
                      (printf "  case ~a:\n    " l)
                      (emit-tail (cdr c))
                      (display "    break;")]
                     [(cons l l*)
                      (printf "  case ~a:\n" l)
                      (loop l*)]
                     [(list) (error 'switch)])))
            (display "  default:\n")
            (emit-tail d)
            (display "}\n"))]
    [(Return e) (display "")]))

(: emit-subroutines (-> D5-Bnd-Code* Void))
(define (emit-subroutines code*)
  (display "\n//Here are all the definitions for Subroutines\n")
  (for : Void ([bnd : D5-Bnd-Code code*])
       (match-let ([(cons lbl (Code var* body)) bnd])
         (emit-function IMDT-C-TYPE (uid->string lbl) var* body))))

(: emit-function-prototype (-> String String (Listof String) Void))
(define (emit-function-prototype returns name vars)
  (display returns)
  (display #\space)
  (display name)
  (display-seq vars "( " (string-append IMDT-C-TYPE " ") "," " " ")"))

(: emit-function (-> String String Uid* D5-Body Void))
(define (emit-function returns name args body)
  (match-let ([(Locals local-var* tail) body])
    (emit-function-prototype returns name (map uid->string args))
    (emit-block local-var* tail)
    (newline)
    (newline)))

(: emit-block (-> Uid* D5-Tail Void))
(define (emit-block local-var* tail)
  (let ([local-var* (map uid->string local-var*)])
    (display "{\n")
    (display-seq local-var* "" (string-append IMDT-C-TYPE " ") "" ";\n" "")
    (emit-tail tail)
    (display "}")))

(: emit-tail (-> D5-Tail Void))
(define (emit-tail tail)
  (logging emit-tail (Vomit) tail)
  (match tail
    [(Begin s* t)
     (begin (for ([s : D5-Effect s*])
              (emit-effect s)
              (display ";\n"))
            (emit-tail t))]
    [(If t c a)
     (begin (display "if ")
            (emit-pred t)
            (emit-block '() c)
            (display " else ")
            (emit-block '() a)
            (newline))]
    [(Switch t c* d)
     (begin (display "switch (") (emit-value t) (display "){\n")
            (for ([c c*])
              (let loop : Void ([l* (car c)])
                   (match l* 
                     [(list l)
                      (printf "  case ~a:\n    " l)
                      (emit-tail (cdr c))
                      (display "    break;")]
                     [(cons l l*)
                      (printf "  case ~a:\n" l)
                      (loop l*)]
                     [(list) (error 'switch)])))
            (display "  default:\n")
            (emit-tail d)
            (display "}\n"))]
    [(Return e)
     (if (Success? e)
         (display "return EXIT_SUCCESS;")
         (begin
           (display "return ")
           (emit-value e)
           (display ";\n")))]))

(: emit-pred (-> D5-Pred Void))
(define (emit-pred r)
  (match r
    #;[(If t c a) (emit-ternary (emit-pred t) (emit-pred c) (emit-pred a))]
    #;[(Begin stm* pred) (emit-begin stm* (emit-pred pred))]
    #;[(App v v*) (emit-function-call v v*)]
    [(Relop p e1 e2) (emit-op p (list e1 e2))]))

(: emit-value (-> D5-Value Void))
(define (emit-value e)
  (match e
    [(If t c a)       (emit-ternary (emit-pred t) (emit-value c) (emit-value a))]
    #;[(Begin stm* exp) (emit-begin stm* (emit-value exp))]
    [(App-Code v v*)       (emit-function-call v v*)]
    [(Op p exp*)      (emit-op p exp*)]
    [(Var i)          (display (uid->string i))]
    [(Quote (? inexact-real? f))  (printf "float_to_imdt(~a)" f)]
    [(Quote (? char? c))
     (if (<= 0 (char->integer c) 255)
         (printf "'\\x~a'" (number->string (char->integer c) 16))
         (error 'generate-c/quote-char "currently only supports ASCII"))]
    [(Quote k)            (print k)]
    ;; TODO Consider changing how Halt is handled
    [(Halt)           (display "exit(-1),-1")]
    [(Code-Label i)  (begin
                       (display "((")
                       (display "int64_t)")
                       (display (uid->string i))
                       (display ")"))]
    [other   (error 'generate-c-emit-value-match)]))



(: emit-effect (-> D5-Effect Void))
(define (emit-effect s)
  (match s
    [(If t (Begin c _) (Begin a _))
     (begin (display "if")
            (emit-pred t)
            (display "{")
            (emit-begin c (void))
            (display "} else {")
            (emit-begin a (void))
            (display "}")
            (newline))]
    [(Switch t c* d)
     (begin (display "switch (") (emit-value t) (display "){\n")
            (for ([c c*])
              (let loop : Void ([l* (car c)])
                   (match l* 
                     [(list l)
                      (printf "  case ~a:\n    " l)
                      (emit-begin (Begin-effects (cdr c)) (void))
                      (display "    break;")]
                     [(cons l l*)
                      (printf "  case ~a:\n" l)
                      (loop l*)]
                     [(list) (error 'switch)])))
            (display "  default:\n")
            (emit-begin (Begin-effects d) (void))
            (display "}\n"))]
    [(Repeat i st sp #f #f (Begin e* _))
     (begin (display "for(")
            (display (uid->string i))
            (display " = ")
            (emit-value st)
            (display " ; ")
            (display (uid->string i))
            (display " < ")
            (emit-value sp)
            (display " ; ")
            (display (uid->string i))
            (display " += 1 ) {\n")
            ;;(display "__asm__(\"\");")
            (emit-begin e* (void))
            (display "}\n"))]
    [(Op p exp*)
     (emit-op p exp*)]
    [(Halt) (display C-EXIT)]
    [(Break-Repeat) (display "break;")];; FIXME: fix that
    [(Assign uid exp)
     (begin (display (uid->string uid))
            (display " = ")
            (emit-value exp))]
    [(No-Op) (display " 0 ")]))

(define-type Emitter (-> Void))
(define-type Caster  (Emitter -> Void))

(: emit-easy-op :
   (Listof D5-Value) Implementation-Type String
   (Listof Caster) (Listof Caster) -> Void)
(define (emit-easy-op args type op arg-cast return-cast)
  (define (expr-th)
    (match* (type args arg-cast)
      [('infix-op (list e1 e2) (list c1 c2))
       (c1 (thunk (emit-value e1)))
       (display #\space) (display op) (display #\space)
       (c2 (thunk (emit-value e2)))]
      [('function e*  c*)
       (define e.c* (map (inst cons D5-Value Caster) e* c*))
       (: emit-e.c : ((Pair D5-Value Caster) -> Void))
       (define/match (emit-e.c e.c)
         [((cons e c)) (c (thunk (emit-value e)))])
       (display op) (sequence emit-e.c e.c* display "(" "" "," "" ")")]
      [('identity (list e) (list c))
       (c (thunk (emit-value e)))]
      [('prefix-op (list e1) (list c1))
       (display #\space) (display op) (display #\space)
       (c1 (thunk (emit-value e1)))]
      [(other wi se) (error 'emit-easy-op/expr-th "unmatched ~v" other)]))
  (emit-wrap
   (if (pair? return-cast)
       ((car return-cast) expr-th)
       (expr-th))))

(: float->imdt Caster)
(define (float->imdt th) (display "float_to_imdt")(emit-wrap (th)))
(: imdt->float Caster)
(define (imdt->float th) (display "imdt_to_float")(emit-wrap (th)))

(: imdt->float->int Caster)
(define (imdt->float->int th)
  (emit-wrap (display IMDT-C-TYPE))
  (display "imdt_to_float") (emit-wrap (th)))

(: imdt->int->float Caster)
(define (imdt->int->float th)
  (display "float_to_imdt")
  (emit-wrap (emit-wrap (display "double"))
             (emit-wrap (th))))

(: no-cast Caster)
(define (no-cast th) (th))
(define imdt->int   no-cast)
(define int->imdt   no-cast)
(define bool->imdt  no-cast)
(define imdt->bool  no-cast)
(define char->imdt  no-cast)
(define imdt->char  no-cast)
(: imdt->string Caster)
(define (imdt->string th) (display "(char*)") (th))
(define-type Implementation-Type (U 'function 'infix-op 'prefix-op 'identity))
(define-type  IMPL
  (List Implementation-Type String (Listof Caster) (Listof Caster)))
(define prim-impl : (HashTable Symbol IMPL)
  ;; TODO If we keep types we could drop these type casts at primitives and
  ;; possibly generate cleaner more understandable code.
  ;; A possible intermediate step would be to generate this table from
  ;; a operator giving type information
  (make-immutable-hash
   `((+          infix-op  "+"     (,imdt->int ,imdt->int) (,int->imdt))
     (-          infix-op  "-"     (,imdt->int ,imdt->int) (,int->imdt))
     (*          infix-op  "*"     (,imdt->int ,imdt->int) (,int->imdt))
     (%<<        infix-op  "<<"    (,imdt->int ,imdt->int) (,int->imdt))
     (%>>        infix-op  ">>"    (,imdt->int ,imdt->int) (,int->imdt))
     (%/         infix-op  "/"     (,imdt->int ,imdt->int) (,int->imdt))
     (%%         infix-op  "%"     (,imdt->int ,imdt->int) (,int->imdt))
     (binary-and infix-op  "&"     (,imdt->int ,imdt->int) (,int->imdt))
     (binary-or  infix-op  "|"     (,imdt->int ,imdt->int) (,int->imdt))
     (binary-xor infix-op  "^"     (,imdt->int ,imdt->int) (,int->imdt))
     (binary-not prefix-op "~"     (,imdt->int) (,int->imdt))
     (<          infix-op  "<"     (,imdt->int ,imdt->int) (,bool->imdt))
     (<=         infix-op  "<="    (,imdt->int ,imdt->int) (,bool->imdt))
     (=          infix-op  "=="    (,imdt->int ,imdt->int) (,bool->imdt))
     (>          infix-op  ">"     (,imdt->int ,imdt->int) (,bool->imdt))
     (>=         infix-op  ">="    (,imdt->int ,imdt->int) (,bool->imdt))
     (and        infix-op  "&&"    (,imdt->bool ,imdt->bool) (,bool->imdt))
     (or         infix-op  "||"    (,imdt->bool ,imdt->bool) (,bool->imdt))
     (not        prefix-op "!"     (,imdt->bool) (,bool->imdt))
     (quotient   infix-op  "/"     (,imdt->int ,imdt->int) (,int->imdt))
     (fl+        infix-op  "+"     (,imdt->float ,imdt->float) (,float->imdt))
     (fl-        infix-op  "-"     (,imdt->float ,imdt->float) (,float->imdt))
     (fl*        infix-op  "*"     (,imdt->float ,imdt->float) (,float->imdt))
     (fl/        infix-op  "/"     (,imdt->float ,imdt->float) (,float->imdt))
     (flmodulo   function  "fmod"  (,imdt->float ,imdt->float) (,float->imdt))
     (flmin      function  "fmin"  (,imdt->float ,imdt->float) (,float->imdt))
     (flmax      function  "fmax"  (,imdt->float ,imdt->float) (,float->imdt))
     (fl<        infix-op  "<"     (,imdt->float ,imdt->float) (,bool->imdt))
     (fl<=       infix-op  "<="    (,imdt->float ,imdt->float) (,bool->imdt))
     (fl=        infix-op  "=="    (,imdt->float ,imdt->float) (,bool->imdt))
     (fl>        infix-op  ">"     (,imdt->float ,imdt->float) (,bool->imdt))
     (fl>=       infix-op  ">="    (,imdt->float ,imdt->float) (,bool->imdt))
     (flquotient infix-op  "/"     (,imdt->float ,imdt->float) (,int->imdt))
     (flround    function  "round" (,imdt->float) (,float->imdt))
     (flnegate   prefix-op "-"    (,imdt->float) (,float->imdt))
     (flabs      function  "fabs"  (,imdt->float) (,float->imdt))
     (flfloor    function  "floor" (,imdt->float) (,float->imdt))
     (flceiling  function  "ceil"  (,imdt->float) (,float->imdt))
     (flcos      function  "cos"   (,imdt->float) (,float->imdt))
     (fltan      function  "tan"   (,imdt->float) (,float->imdt))
     (flsin      function  "sin"   (,imdt->float) (,float->imdt))
     (flasin     function  "asin"  (,imdt->float) (,float->imdt))
     (flacos     function  "acos"  (,imdt->float) (,float->imdt))
     (flatan     function  "atan"  (,imdt->float) (,float->imdt))
     (flatan2    function  "atan2" (,imdt->float ,imdt->float) (,float->imdt)) 
     (fllog      function  "log"   (,imdt->float) (,float->imdt))
     (flexp      function  "exp"   (,imdt->float) (,float->imdt))
     (flsqrt     function  "sqrt"  (,imdt->float) (,float->imdt))
     (Print      function  "printf"  (,imdt->string) ())
     (Exit       function  "exit"  (,no-cast) ())
     (int->float identity  "none"  (,imdt->int->float) ())
     (float->int identity  "none"  (,imdt->float->int) ())
     (read-int   function  "read_int"   () (,int->imdt))
     (read-float function  "read_float" () (,float->imdt))
     (read-char  function  "read_ascii_char" () (,char->imdt))
     (print-char function  "print_ascii_char" (,imdt->char) ())
     (display-char function "display_ascii_char" (,imdt->char) ())
     (int->char  identity   "none"  (,imdt->char) ())
     (char->int  identity   "none"  (,char->imdt) ()))))

(define (easy-p? [s : Symbol]) : Boolean
  (if (hash-ref prim-impl s #f) #t #f))
(define (easy-p->impl [s : Symbol]) : IMPL
  (define (err) (error 'easy-p->impl))
  (hash-ref prim-impl s err))

(: emit-op (-> Symbol D5-Value* Void))
(define (emit-op p exp*)
  (match* (p exp*)
    [('Array-set! (list a o v))
     (begin
       (emit-wrap (display "(int64_t*)")
                  (emit-value a))
       (display "[")
       (emit-value o)
       (display "] = ")
       (emit-value v))]
    [('Array-ref (list a o))
     (begin
       (emit-wrap (display "(int64_t*)")
                  (emit-value a))
       (display "[")
       (emit-value o)
       (display "]"))]
    [('print-float (list f p))
     (display "printf")
     (emit-wrap (display "\"%.*lf\",")
                (emit-value p)
                (display ", ")
                (display "imdt_to_float")
                (emit-wrap (emit-value f)))]
    [('print-int (list d))
     (display "printf")
     (emit-wrap (display "\"%ld\",") (emit-value d))]
    [('Printf (cons fmt exp*))
     (begin (display "printf")
            (emit-wrap
             (emit-value fmt)
             (unless (null? exp*)
               (display ", ")
               (sequence emit-value exp* display "" "(void *)" "," "" ""))))]
    [('Alloc (list exp))
     ;; TODO this could be implemented in the C Code as a layer of macros
     (match (garbage-collector)
       ['Boehm (display "GC_MALLOC(8 * ")
               (emit-value exp)
               (display ")")]
       ['None (display "nonegc_malloc(8 * ")
              (emit-value exp)
              (display ")")])]
    [('Types-hashcons! (list e hcode))
     (display "hashcons(")
     (display "types_ht,")
     (emit-value e)
     (display ",")
     (emit-value hcode)
     (display ")")]
    [('Types-gen-index! (list))
     (display "types_unique_index_counter++")]
    [('timer-start (list)) (display timer-start)]
    [('timer-stop  (list)) (display timer-stop)]
    [('timer-report (list)) (display timer-report)]
    [((? easy-p? (app easy-p->impl (list t s a* r))) e*)
     (emit-easy-op e* t s a* r)]
    [(other wise)
     (error 'backend-c/generate-c/emit-op "unmatched value ~a" other)]))


;;(: emit-primitive-value (->  Void))
(define (emit-primitive-value p exp*) (error 'emit-primitive-value-undefined))
(define (emit-primitive-pred p e1 e2) (error 'emit-primitive-pred-undefined))

(: emit-function-call (-> D5-Value D5-Value* Void))
(define (emit-function-call val val*)
  (emit-wrap
   (if (Code-Label? val)
       (display (uid->string (Code-Label-value val)))
       (emit-cast->fn val val*))
   (sequence emit-value val* display "(" "" "," "" ")")))

;; primitives for generating syntax
(define-syntax-rule (emit-wrap a ...)
  (begin (display "(") a ... (display ")")))

(: display-seq (-> (Listof Any) Any Any Any Any Any Void))
(define (display-seq seq start before between after finish)
  (sequence display seq display start before between after finish))

(: sequence (All (A B) (-> (-> A Void) (Listof A) (-> B Void) B B B B B Void)))
(define (sequence f seq g start before between after finish)
  (: loop (All (A) (-> (-> A Void) (Listof A) Void)))
  (define (loop f seq)
    (if (null? seq)
        (g finish)
        (let ([seq^ (cdr seq)])
          (if (null? seq^)
              (begin (g before)
                     (f (car seq))
                     (g after)
                     (g finish))
              (begin (g before)
                     (f (car seq))
                     (g after)
                     (g between)
                     (loop f seq^))))))
  (g start)
  (loop f seq))

;; common c syntax constructs
(define-syntax-rule (emit-begin s* emit-res)
  (begin
    (for ([s : D5-Effect s*])
      (emit-effect s)
      (display ";\n"))
    emit-res))

(define-syntax-rule (emit-ternary a b c)
  (emit-wrap a (display " ? ") b (display " : ") c))


;; C Style casts
(define-syntax-rule (emit-cast->string exp)
  (emit-wrap (display "(const char*) ") exp))

(define-syntax-rule (emit-imdt-cast) (error 'emit-imdt-cast))

(: emit-cast->fn (-> D5-Value D5-Value* Void))
(define (emit-cast->fn exp args)
  (emit-wrap
   (emit-wrap
    (display IMDT-C-TYPE)
    (display " (*)")
    (let ([a* : (Listof String) (map (lambda (a) IMDT-C-TYPE) args)])
      (display-seq a* "(" "" "," "" ")")))
   (emit-value exp)))
