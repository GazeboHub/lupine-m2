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


;;; * Element transformation proto.


(defclass transformation-model ()
  ;; FIXME: differentiate this class w.r.t `MODEL' (see "here documentation", previous)

  ;; effectively a container for XMI -> Common Lisp transformation descriptors
  ((namespaces
    :initarg :namespaces
    :accessor transformation-model-namespaces
    )
   (ns-registry
    :type namespace-registry
    :accessor tranformation-model-ns-registry)
   ))

(defmethod shared-initialize :after ((instance transformation-model)
				     slots &rest initargs
				     &key &allow-other-keys)
  (when (or (not (slot-boundp instance 'ns-registry))
	    (and (listp slots)
		 (member 'ns-registry (the list slots) :test #'eq)))
    (cond
      ((slot-boundp instance 'namespaces)
       (let ((namespaces (slot-value instance  'namespaces)))
	 (when namespaces
	   (let ((nsreg (make-namespace-registry)))
	     (dolist (ns namespaces)
	       (destructuring-bind (prefix . uri) ns
		 ;; FIXME: integrate further with qname utils, when
		 ;; adding elements to the transformation model, etc -
		 ;; avoid duplcation of qname strings, esp.
		 (ensure-prefix prefix uri nsreg)))
	     (setf (tranformation-model-ns-registry instance)
		   nsreg)))))
      (t
       (simple-style-warning
	"Instance 'namespaces' slot not bound, unable to initialize ~
ns-registry slot, in ~s"
	instance)))))


(defgeneric add-transformation (transformation model))

(defgeneric find-transformation (namespace element type model))
;; ^ ? too broad ?

(defgeneric apply-transform (transformation source))

(defclass transformation-model-component ()
  ;; used in METAMODEL-TRANSFORM and in TRANSFORM-CLASS
  ((model
    :initarg :model
    :type transformation-model
    :accessor component-transformation-model)
   ))

;;; * ...

(declaim (type transformation-model *uml-stub-model*))

(defvar *uml-stub-model* ;; name ??
  (make-instance 'transformation-model
		 :namespaces
		 '(("uml" . "http://www.omg.org/spec/UML/20110701" ))))


(defclass metamodel-transform (transformation-model-component)
  ((local-name ;; how differs from TYPE ?
    ;; ^ local name for type of metamodel serialiation?
    :initarg :local-name
    ;; FIXME: validate local-name as an XML name
    :type string ;; FIXME: namespace qualified strings? - see ensure-qname (?)
    :accessor transform-local-name)
   (namespace
    :initarg namespace
    :type namespace
    :accessor transform-namespace)
   (element-p
    :initarg :element-p
    :type boolean
    :accessor transform-element-p)
   (attribute-p
    :initarg :attribute-p
    :type boolean
    :accessor transform-attribute-p)
   (type
    ;; cf. xmi:type attribute value in UML
    ;; (FIXME: Decouple that from the metamodel?)
    :initarg :type
    :type string ;; ?? FIXME: namespace qualified strings ??
    )
   ;; ...
   ))


;;; * Slot Definitions


(defclass property-transform-slot-definition
    (metamodel-transform slot-definition)
  ())

(defclass direct-property-transform-slot-definition
    (metamodel-transform-slot-definition standard-direct-slot-definition)
  ())

(defclass effective-property-transform-slot-definition
    (metamodel-transform-slot-definition standard-effective-slot-definition)
  ())

;; * Trasform-Class

(defclass transform-class (transformation-model-component standard-class)
  ((transform-type
    :initarg :transform-type
    :type qname)))

#+NIL ;; FIXME: register TRANSFORM-TYPE in the appropriate namespace in MODEL
(defmethod shared-initialize :around ((instance transform-class) slots
				      &rest initargs
				      &key (transform-type nil tp)
				      &allow-other-keys)
  (cond
    (tp
     )
    (t (call-next-method))))


(defclass uml-transform-class (transform-class)
  ((uml-source-package
    )
   (uml-element-name
    )))


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
  (:model *uml-stub-model*)

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
  (:transform-type  "uml:Class")  ;; cf. @xmi:type="uml:Class"
  )

(let ((c (find-class 'uml-class)))
  (change-class c c))


;;; * Post Hoc


;; FIXME: Can't use UML-CLASS as a :METACLASS util it's finalized,
;; and it won't be finalized until after DEFCLASS CLASSIFIER

(defclass element ()
  ((owned-comments
    :source-element-p t
    :source-local-name "ownedComment"
    :initarg :owned-comments
    :type property-table
    :accessor class-direct-owned-comments-table))
  (:metaclass uml-transform-class)
  (:model *uml-stub-model*)
  (:uml-name "UML::Element")
  (:transform-type  "uml:Class")
  (:is-abstract t))


(defclass named-element (element)
  ((name
    :source-attribute-p t
    :source-local-name "name"
    :initarg :name
    :type simple-string
    :accessor named-element-name))
  (:metaclass uml-transform-class)
  (:model *uml-stub-model*)
  (:uml-name "UML::NamedElement")
  (:transform-type  "uml:Class")
  (:is-abstract t))


(defclass namespace (named-element)
  ((owned-rules
    :source-element-p t
    :source-local-name "ownedRule"
    :initarg :owned-rules
    :type property-table
    :accessor class-direct-owned-rules-table
    ))
  (:metaclass uml-transform-class)
  (:model *uml-stub-model*)
  (:uml-name "UML::Namespace")
  (:transform-type  "uml:Class")
  (:is-abstract t))


(defclass classifier (namespace type)
  ((generalizations
    :source-element-p t
    :source-local-name "generalization"
    :initarg :generalizations
    :type property-table
    :accessor class-direct-generalizations-table
    ))
  (:metaclass uml-transform-class)
  (:model *uml-stub-model*)
  (:transform-type "uml:Class")
  (:uml-name "UML::Classifier")
  (:is-abstract t))
