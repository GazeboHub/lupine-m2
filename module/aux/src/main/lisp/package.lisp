
(in-package #:cl-user)

(defpackage #:lupine/aux
  (:use #:cl)
  (:export
   ;; type.lisp
   #:class-designator
   #:compute-class
   #:type-designator
   #:member-typep
   #:simplify-string))
