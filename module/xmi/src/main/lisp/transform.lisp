#|                                                      -*- lisp -*-

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#

(in-package #:lupine/xmi)

;;; * Metaclasses

(defgeneric class-transform-type (class))

(defclass transform-class (standard-class)
  ((transform-type
    :initarg :transform-type
    :type string
    :accessor class-transform-type)))

(defclass uml-transform-class (transform-class)
   ((uml-name :initarg :uml-name :type string)
    (uml-packge :initarg :uml-package)))

;;; * Element transformation proto.

(defclass transformation-model ()
  ())

(declaim (type transformation-model *xmi-unmarshalling-model*))

(defvar *xmi-unmarshalling-model*)

(defgeneric find-type-transform (element type tmodel))

(defgeneric apply-type-transform (transform source))

(defclass type-transform ()
  ((containing-element
    ;; containing element for a typed model quality serialized in XMI
    ;; e.g "uml:ownedAttribute"; "uml:generalization"; "uml:packagedElement"
    :initarg :element
    :type string ;; ?? FIXME: namespace qualified strings ??
    )
   (type
    ;; type of a typed model quaity serialized in XMI
    ;; e.g. "uml:Property", "uml:Comment", "uml:Operation", "uml:Constraint", "uml:Package"
    :initag :type
    :type string ;; ?? FIXME: namespace qualified strings ??
    )
   ;; ...
   ))


;;; * Slot Definitions


(defclass element-tranform-slot-definition
    (type-transform slot-definition)
  ())

(defclass direct-element-tranform-slot-definition
    (type-transform-slot-definition standard-direct-slot-definition)
  ())

(defclass effective-element-tranform-slot-definition
    (type-transform-slot-definition standard-effective-slot-definition)
  ())


;;; * UML-Class

(def-uml-package "UML") ;; ?


(defclass uml-class (uml-transform-class)
  ;; NOTE: This class represents the main initial use-case for the
  ;; transformation algorithm proposed in Lupine XMI
  ((owned-attributes
    :element "uml:ownedAttribute"
    :initarg :owned-attributes
    :type list
    :accessor class-direct-owned-attributes)
   (owned-rules
    :element "uml:ownedRule"
    #|...initargs...|#
    )
   (uml-generalizations
    :element "uml:generalization"
    #|...initargs...|#
    )
   (documentation-stub
    #|...initargs...|#
    )
   (owned-comments
    :element "uml:ownedComment"
    #|...initargs...|#
    ))

  (:metaclass uml-tranform-class)

  (:tmodel *xmi-unmarshalling-model*)

  (:namespaces
   ("uml" "http://www.omg.org/spec/UML/20110701"))

  (:uml-name . "UML::Class")

  (:transform-type . "uml:Class"))

(let ((c (find-class 'uml-class)))
  (change-class c c))
