;; -*- lisp -*-

(in-package #:asdf)

(eval-when (#:compile-toplevel #:load-toplevel #:execute)
(defpackage #:lupine/system
    (:use #:asdf #:cl)))

(in-package #:lupine/system)

(defsystem #:lupine-aux
    :components 
    ((:file "package")
     (:file "refs" :depends-on ("package"))
     (:file "strings" :depends-on ("package"))
     (:file "files" :depends-on ("package"))
     ))
