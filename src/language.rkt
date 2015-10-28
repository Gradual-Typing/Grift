#lang typed/racket/base
#|------------------------------------------------------------------------------+
|File: src/language.rkt                                                         |
+-------------------------------------------------------------------------------+
|Authors:                                                                       |
| Andre Kuhlenshmidt (akuhlens@indiana.edu)                                     |
|                                                                               |
+-------------------------------------------------------------------------------+
|Description: This file has been devided in order to support less recompilation
|by typed racket. It remains as a compatability layer until all files requiring
|this file have specified specifically what they require.
+-------------------------------------------------------------------------------|#


(define-syntax-rule (require/provide r ...)
  (begin (require r ...)
         (provide (all-from-out r ...))))

(require/provide 
 "./language/forms.rkt"
 "./language/primitives.rkt"
 "./language/types.rkt"
 "./language/schml0.rkt"
 "./language/schml1.rkt"
 "./language/cast0.rkt"
 "./language/coercion.rkt"
 "./language/cast1.rkt"
 "./language/cast2.rkt"
 "./language/cast3.rkt"
 "./language/cast4.rkt"
 "./language/cast5.rkt"
 "./language/cast6.rkt"
 "./language/data-representation.rkt"
 "./language/data0.rkt"
 "./language/data1.rkt"
 "./language/data2.rkt"
 "./language/data3.rkt"
 "./language/data4.rkt"
 "./language/data5.rkt"
 )




