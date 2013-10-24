#|

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#

(in-package #:lupine/xmi)


#| Here-Documentation (Markdown Format)

refer to ./transform.md

|#

;;; * Metaclasses

(defgeneric class-transform-type (class))

(defclass transform-class (standard-class)
  ((transform-type ;; ?
    :initarg :transform-type
    :type string
    :accessor class-transform-type)))

(defclass uml-transform-class (transform-class)
   ((uml-name :initarg :uml-name :type string) ;; must map to XML local name
    (uml-package :initarg :uml-package)
    ;; ^ FIXME: define package structure and ensure referencing to
    ;; this slot

    )))

;;; * Element transformation proto.


(defclass transformation-model ()
  ;; FIXME: differentiate this class w.r.t `MODEL' (see "here documentation", previous)

  ;; effectively a container for XMI -> Common Lisp transformation descriptors
  ((namespaces
    :initarg :namespaces
    :accessor transformation-model-namespaces
    )
   (ns-registry
    :type qname-meta-registry
    :accessor tranformation-model-ns-registry)
   ))

(defmethod shared-initialize :after ((instance transformation-model)
				     slots &rest initargs
				     &key &allow-other-keys)
  (when (and (or (member 'ns-registry slots :test #'eq)
		 (not (slot-boundp instance 'ns-registry))))
    (cond
      ((slot-boundp instance 'namespaces)
       (let ((namespaces (slot-value instance  'namespaces)))
	 (when namespaces
	   (let ((mregistry (make-meta-registry)))
	     (dolist (ns namespaces)
	       (destructuring-bind (prefix . uri) ns
		 ;; FIXME: integrate further with qname utils, when
		 ;; adding elements to the transformation model, etc - avoid duplcation of qname strings, esp.
		 (handler-case
		     (ensure-prefix prefix uri mregistry)
		   (namespace-prefix-bind ()) ;; no-op
		   )))
	     (setf (tranformation-model-ns-registry instance)
		   mregistry)))))
      (t
       (simple-style-warning
	"Instance 'namespaces' slot not bound, unable to initialize ns-registry: ~s"
	instance)))))



(defgeneric add-transformation (transformation model))

(defgeneric find-transformation (namespace element type model))
;; ^ ? too broad ?

(defgeneric apply-transform (transformation source))

(defclass transformation-model-component ()
  ((model
    :initarg :model
    :type transformation-model
    :accessor component-transformation-model)

   ))

;;; * ...

(declaim (type transformation-model *uml-stub-metamodel*))

(defvar *uml-stub-metamodel*)

(defclass type-transform (transformation-model-component)
  ((source-local-name
    :initarg :source-local-name
    ;; FIXME: validate local-name as an XML name
    :type string ;; FIXME: namespace qualified strings? - see ensure-qname (?)
    :reader type-transform-source-local-name)
   (source-element-p
    :initarg :source-element-p
    :type boolean
    :reader type-transform-source-local-element-p)
   (source-attribute-p
    :initarg :source-element-p
    :type boolean
    :reader type-transform-source-attribute-p)
   (type
    ;; type of a typed model quality serialized in XMI (see notes, in
    ;; the previous)
    ;;
    ;; e.g. "uml:Property", "uml:Comment", "uml:Operation", "uml:Constraint", "uml:Package"
    :initag :type
    :type string ;; ?? FIXME: namespace qualified strings ??
    )
   ;; ...
   ))


;;; * Slot Definitions


(defclass property-transform-slot-definition
    (type-transform slot-definition)
  ())

(defclass direct-property-transform-slot-definition
    (type-transform-slot-definition standard-direct-slot-definition)
  ())

(defclass effective-property-transform-slot-definition
    (type-transform-slot-definition standard-effective-slot-definition)
  ())


;;; * UML-Class

(def-uml-package "UML") ;; ?



(defclass uml-class (classifier uml-transform-class)
  ;; NOTE: This class represents the main initial use-case for the
  ;; transformation algorithm proposed in Lupine XMI
  ((owned-attributes
    :source-element-p t
    :source-local-name "ownedAttribute" ;; qname (?)
    :initarg :owned-attributes
    ;; NB: access to values of slot containing property-table types
    ;; may be faciliated with a special slot definition extension
    :type property-table
    :accessor class-direct-owned-attributes-table)
   (documentation-stub
    :initarg :documentation-stub
    :type simple-string
    :accessor class-documentation-stub
    )
   (is-abstract
    :source-attribute-p t
    :source-local-name "isAbstract"
    :initarg :is-abstract
    :type boolean)
   )

  (:metaclass uml-transform-class)

  ;; FIXME: add :MODEL to other class definitions (?)
  (:model *uml-stub-metamodel*)

  (:namespace
   ;; namespaces for qnames
   ;; i.e. use these namespace prefixes when resolving qnames denoted
   ;; in the slot definitions and in class options
   ("uml" "http://www.omg.org/spec/UML/20110701" ))

  ;; UML composite name
  ;;
  ;; This needs a "UML" package defined for appropriate resolution,
  ;; however.
  ;;
  ;; This class option would effectively denote a packagedElement
  ;; definition for the defining class.
  (:uml-name . "UML::Class")

  ;; "uml" in the following item denotes the namespace URI assigned
  ;; tot he prefix "uml"
  (:transform-type . "uml:Class")  ;; cf. @xmi:type="uml:Class"
  )

(let ((c (find-class 'uml-class)))
  (change-class c c))


;;; * Post Hoc


(defclass element ()
  ((owned-comments
    :source-element-p t
    :source-local-name "ownedComment"
    :initarg :owned-comments
    :type property-table
    :accessor class-direct-owned-comments-table))
  (:metaclass uml-class)
  (:model *uml-stub-metamodel*)
  (:namespace
   ("uml" "http://www.omg.org/spec/UML/20110701" ))
  (:uml-name "UML::Element")
  (:is-absract . t))


(defclass namespace ()
  ((owned-rules
    :source-element-p t
    :source-local-name "ownedRule"
    :initarg :owned-rules
    :type property-table
    :accessor class-direct-owned-rules-table
    ))
  (:metaclass uml-class)
  (:model *uml-stub-metamodel*)
  (:namespace
   ("uml" "http://www.omg.org/spec/UML/20110701" ))
  (:uml-name "UML::Namespace")
  (:is-absract . t))


(defclass classifier (namespace type)
  ((generalizations
    :source-element-p t
    :source-local-name "generalization"
    :initarg :generalizations
    :type property-table
    :accessor class-direct-generalizations-table
    ))
  (:metaclass uml-class)
  (:model *uml-stub-metamodel*)
  (:namespace
   ("uml" "http://www.omg.org/spec/UML/20110701" ))
  (:uml-name "UML::Classifier")
  (:is-absract . t))
