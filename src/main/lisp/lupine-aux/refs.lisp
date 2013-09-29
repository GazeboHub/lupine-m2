(in-package #:lupine/aux)

(defmacro defconstant* (name value &optional docs)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
        (unless (boundp (quote ,name))
            (defconstant ,name ,value
                ,@(when docs (list docs))))))
