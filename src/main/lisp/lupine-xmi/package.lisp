
(im-package #:cl-user)

(defpackage #:lupine/xmi
    (:use #:lupine/aux #:cl)
    (:export
        ;; validation.lisp
        #:version-namespace
        #:compute-xmi-namespace
    ))
