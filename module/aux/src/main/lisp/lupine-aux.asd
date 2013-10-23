;; -*- lisp -*-

(in-package #:cl-user)

#-asdf
(require #:asdf)

(defpackage #:lupine-aux-system
  (:use #:asdf #:cl))

(in-package #:lupine-aux-system)

;; refer to http://www.alu.org/mop/index.html

(defsystem #:lupine-aux
  :components
  ((:file "package")
   (:file "type"
    :depends-on ("package"))
   ))
