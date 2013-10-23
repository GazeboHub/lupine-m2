;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10 -*-
;;; Ystok-Library - ASDlite/ASDF system definition
;;; Copyright (c) 2003-2012 Dr. Dmitriy Ivanov. All rights reserved.

(in-package :cl-user)

(asdf:defsystem ystok-library
  :version "1.2.022"
  :description "Portable Common Lisp library of general purpose macros, functions, and utilities"
  :maintainer "Dmitriy Ivanov"
  :licence "LLGPL"
  #+asdlite #+asdlite
  :output-pathname (make-pathname :name nil :type nil :version nil
                                  :defaults (merge-pathnames "bin/" *load-truename*))
  :components
  ((:file "yl-package")
   (:file "macros" :depends-on ("yl-package"))
   (:file "debug" :depends-on ("macros"))
   (:file "functions" :depends-on ("macros"))
   #+(and win32 lispworks) (:file "win32" :depends-on ("yl-package"))
   #-lispworks (:file "lw-compat" :depends-on ("yl-package"))
   (:file "locale" :depends-on ("macros"
                                #+(and win32 lispworks) "win32"))
   (:module "lang" :depends-on ("locale")
    :components
    (#+Russian (:file "ru")
     #+German  (:file "de")
     #+French  (:file "fr")))
   (:file "meta-parse" :depends-on ("yl-package"))
   (:file "meta-parse-number" :depends-on ("lang" "meta-parse"))
   (:file "file-utils" :depends-on ("functions" "locale"
                                    #+(and win32 lispworks) "win32"))
   #+ys-product (:file "ys-product" :depends-on ("debug" "locale" "functions"))
   #-ys-product (:file "ys-stubs" :depends-on ("debug" "functions"))
   #+ys-product (:file "systema" :depends-on ("ys-product"))))

;(asdf:operate 'asdf:load-op 'ystok-library)
