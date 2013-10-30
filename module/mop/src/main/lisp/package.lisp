
(in-package #:cl-user)

(defpackage #:lupine/mop
  (:use #:c2mop #:lupine/aux #:cl)
  #+(or CMU SBCL)
  (:shadowing-import-from
   #+sbcl #:sb-pcl
   #+cmu #:pcl
   #:validate-superclass)
  (:shadowing-import-from
   #:c2mop
   #:defmethod
   #:defgeneric
   #:standard-generic-function
   )
   (:export
    #:class-designator
    #:compute-class
    #:compute-direct-slot-definitions
    #:slot-definition-not-found
    #:direct-slot-definition-not-found
    #:find-slot-definition
    #:find-direct-slot-definition
    )
   )

(let ((p (find-package '#:lupine/mop)))
  (do-external-symbols (s '#:c2mop)
    (export s p)))
