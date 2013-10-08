#|

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#

(defgeneric element-generalizations (element))

(defgeneric (setf element-generalizations) (new-set element))

(defclass uml-element ()
  ())

(defclass uml-element-class (uml-element standard-class)
  ())

#+NIL ;; should this be derived on the class or its metaclass?
(defmethod element-generalizations ((element uml-element-class))
  ;; NB: This captures only those generalizations mapping onto a
  ;; class' direct superclasses
  (let (bucket)
    (dolist (elt (class-direct-superclasses element) bucket)
      (when (typep elt 'uml-element)
	(setq bucket (nconc bucket (list elt)))))))

(def-uml-package "InfrastructureLibrary")
(in-uml-package  "InfrastructureLibrary")

(def-uml-package "Core")
(in-uml-package "Core")

(def-uml-package "Constructs")

;; stubs for PrimitiveType generaizations

(defclass classifier (uml-element-class)
  ()
  (:metaclss uml-element-class)
  (:name "Classifier")
  (:package "Core::Constructs")
  ;; FIXME: Define classes for the ends of these generalizations:
  (:generalizations "Core::Constructs::Type"
		    "Core::Constructs::Namespace")
  (:documentation
   "cf. UML 2.4.1 Infrastructure, subclause 11.6.1"))


(defclass data-type (uml-element-class)
  ()
  (:metaclss uml-element-class)
  (:name "DataType")
  (:package "Core::Constructs")
  (:generalizations "Core::Constructs::Classifier")
  (:documentation
   "cf. UML 2.4.1 Infrastructure, subclause 11.6.1"))


;;


(defclass primitive-type (uml-element-class)
  ()
  (:metaclss uml-element-class)
  (:name "PrimitiveType")
  (:package "Core::Constructs")
  (:generalizations "Core::Constructs::DataType")
  (:documentation
   "cf. UML 2.4.1 Infrastructure, subclause 11.6.5"))


(def-uml-package "PrimitiveTypes"
    :cl-name #:primitive-types)

(in-uml-package "PrimitiveTypes")

;; FIXME: Define how to marshal/unmarshal values of these types, when
;; interpreting an XMI encoded UML model

;; FIXME: Try defining something more sophisticated than this trivial
;; implementation, such as with a macro, DEFINE-TYPE-TRANSFORM,
;; or other procedure that would produce a UML-ELEMENT for each
;; respective UML primitive type

(defclass Primitive-Types:boolean ()
  ()
  (:documentation
   "cf. UML 2.4.1 Infrastructure, subclause 13.1.1")
  (:name "Boolean")
  #+NIL (:package "PrimitiveTypes")
  (:tag "org.omg.xmi.schemaType"
	"http://www.w3.org/2001/XMLSchema#boolean")
  (:metaclass primitive-type))

(defclass Primitive-Types:integer ()
  ()
  (:documentation
   "cf. UML 2.4.1 Infrastructure, subclause 13.1.2")
  (:name "Integer")
  #+NIL (:package "PrimitiveTypes")
  (:tag "org.omg.xmi.schemaType"
	"http://www.w3.org/2001/XMLSchema#integer")
  (:metaclass primitive-type))

(defclass Primitive-Types:real ()
  ()
  (:documentation
   "cf. UML 2.4.1 Infrastructure, subclause 13.1.3")
  (:name "Real")
  #+NIL (:package "PrimitiveTypes")
  (:tag "org.omg.xmi.schemaType"
	"http://www.w3.org/2001/XMLSchema#real")
  (:metaclass primitive-type))

(defclass Primitive-Types:string ()
  ()
  (:documentation
Primitive-Types   "cf. UML 2.4.1 Infrastructure, subclause 13.1.4")
  (:name "String")
  #+NIL (:package "PrimitiveTypes")
  (:tag "org.omg.xmi.schemaType"
	"http://www.w3.org/2001/XMLSchema#real")
  (:metaclass primitive-type))

(defclass Primitive-Types:unlimited-natural ()
  ()
  (:documentation
   "cf. UML 2.4.1 Infrastructure, subclause 13.1.5")
  (:name "UnlimitedNatural")
  #+NIL (:package "PrimitiveTypes")
  (:tag "org.omg.xmi.schemaType"
	"http://www.w3.org/2001/XMLSchema#real")
  (:metaclass primitive-type))
