;; -*- lisp -*-

(in-package #:asdf)

(eval-when (#:compile-toplevel #:load-toplevel #:execute)
    ;; ensure that the main sysdef is loaded, initially
    (find-system #:lupine-aux))

(in-package #:lupine/system)

(defsystem #:lupine-xmi
    :defsystem-depends-on (#:lupine-aux)
    :components 
    ((:file "package")
     (:file "validation" :depends-on ("package"))
     ))
