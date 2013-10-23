#|                                                      -*- lisp -*-

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#

(in-package #:lupine/xmi)

(defgeneric class-transform-type (class))

(defclass transform-class (standard-class)
  ((transform-type
    :initarg :transform-type
    :type string
    :accessor class-transform-type)))

(defclass UML-tranform-class (transform-class)
   ((uml-name :initarg :uml-name :type string)
    (uml-packge :initarg :uml-package)))

(defclass element-tranform-slot-definition (slot-definition)
  ((element :initarg :element)))

(deftransform-namespace "uml" "http://www.omg.org/spec/UML/20110701")

(defclass UML-class (UML-tranform-class)
  ((owned-attributes
    :element "uml:ownedAttribute"
    :initarg :owned-attributes
    :type list
    :accessor class-direct-owned-attributes)
   (owned-rules
    :element "uml:ownedRule"
    #|...initargs...|# )
   (uml-generalizations
    :element "uml:generalization"
    #|...initargs...|# )
   (documentation-stub
    #|...initargs...|# )
   (owned-comments
    :element "uml:ownedComment"
    #|...initargs...|# ))

  (:metaclass UML-tranform-class)
  (:uml-name "Class")
  (:transform-type "uml:Class"))

(let ((c (find-class 'uml-class)))
  (change-class c c))