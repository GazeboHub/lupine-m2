
(in-package #:cl-user)

(defpackage #:lupine/aux
    (use #:cl)
    (export
        ;; refs.lisp
        #:defconstant*
        
        ;; strings.lisp
        #:simplify-string
        #:simplify-string*
        
        ;; files.lisp
        #:%file-buffer-size%
        #:%file-buffer-element-type%
        #:file=
        
    ))
