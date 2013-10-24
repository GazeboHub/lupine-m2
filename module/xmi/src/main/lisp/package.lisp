#|

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#


(in-package #:cl-user)

(defpackage #:lupine/xmi
    (:use #:cxml #:lupine/mop #:puri #:lupine/aux #:cl)
    #+(or CMU SBCL)
    (:shadowing-import-from
     #+sbcl #:sb-pcl
     #+cmu #:pcl
     #:validate-superclass)
    (:shadowing-import-from
     #:c2mop
     #:defmethod
     #:defgeneric
     #:standard-generic-function)
    (:export

     ))
