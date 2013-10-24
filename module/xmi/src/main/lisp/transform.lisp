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
  ((transform-type ;; ?
    :initarg :transform-type
    :type string
    :accessor class-transform-type)))

(defclass uml-transform-class (transform-class)
   ((uml-name :initarg :uml-name :type string)
    (uml-package :initarg :uml-package)))

;;; * Element transformation proto.

(defclass transformation-model ()
  ;; effectively a container for XMI -> Common Lisp transformation descriptors
  ())

(defgeneric add-transformation (transformation model))

(defgeneric find-transformation (namespace element type model))
;; ^ ? too broad ?

(defgeneric apply-transform (transformation source))

#| NB: Types and Refinment within XMI Serialized UML Element Definitions

cf. results of shell command

 grep "xmi:type" UML.xmi |
  sed 's|<||g' | awk '{print $1 " " $2}' |
  sort | uniq

There are exactly three types of packagedElement defined
  in UML.xmi:

  packagedElement[@xmi:type="uml:Association"]
  packagedElement[@xmi:type="uml:Class"]
  packagedElement[@xmi:type="uml:Enumeration"]

## Metamodels and Types, in Pratice

The set of possible types of definitions that may be validly specified
in the `packagedElement` relation is constrained in UML itself. Note
the `PackageableElement` feature in the following - from UML.xmi, in
the definition of the type `uml:Package`

   <ownedAttribute xmi:type="uml:Property"
     xmi:id="Package-packagedElement"
     name="packagedElement"
     visibility="public"
     type="PackageableElement"
     aggregation="composite"
     subsettedProperty="Namespace-ownedMember"
     association="A_packagedElement_owningPackage">  ... </ownedAttribute>

The `xmi:type` attribute describes the type of the definition
represented by  the `ownedAttribute` declaration itself. As such,
the xmi:type attribute essentially describes the type of the
definition as a metamodel feature.

The `type` value within that `ownedAttribute` declaration - the
`type` attribute having the same XML namesapace as the
`ownedAttribute` declaration - that attribute may be understood as
describing the type of the value that may be contained in such a
relation as defined in the `ownedAttribute` property. Essentially, it
denotes the type of the model feature described by the metmodel
feature in its XMI encoding.


## Discussion of Types and Element Names in the XMI Specification

In reference to [XMI 2.4.1], subclause 7.6.3:

  "The type attribute is used to specify the type of object being
  serialized, when the type is not known from the model"

Regarding element names, from subclause 7.8.1:

  "The name for XML tags corresponding to model Properties is the
  short name of the property. The name of XML attributes corresponding
  to model properties (DataType-typed or Class-typed) is the short
  name of the property, since each tag in XML has its own naming context"

Albeit, there may not appear be a lot of guidance in that, for
integration with components implementing other semantically
complimentary OMG specifications. As far as "Proof of concept,"
however...

## Core Model Features

In order for the UML metamodel defined in UML.xmi to be interpreted, a
corresponding UML implementation may be developed manually, such as to
implement the program and semantics necessary for unmarshaling that
UML metamodel. (Once the UML metamodel defined in UML.xmi can be
interpeted competely, it may serve to add features to the initial
"Boostrap" or "Core" model defined for its intepretation)

## Pakaged Elements

A `packagedElement` relation represents a relation between a UML
Package definition and a definition of a UML element of type
PackageableElement.

The `packagedElement` and `PackageableElement` both may represent
types of UML "core" model elements, such that should be manually
supported in the UML interpreter, if simply for the sake of being able
to load the UML model itself, in its normative XMI encoding (cf. UML.xmi).

## Core Model Metamodel Types

There are exactly 21 unique element/type relations visible in
UML.xmi - sorted in alphabetical order:

  defaultValue xmi:type="uml:InstanceValue"
  defaultValue xmi:type="uml:LiteralBoolean"
  defaultValue xmi:type="uml:LiteralInteger"
  defaultValue xmi:type="uml:LiteralUnlimitedNatural"

  generalization xmi:type="uml:Generalization"

  lowerValue xmi:type="uml:LiteralInteger"

  mofext:Tag xmi:type="mofext:Tag"

  ownedAttribute xmi:type="uml:Property"

  ownedComment xmi:type="uml:Comment"

  ownedEnd xmi:type="uml:Property"

  ownedLiteral xmi:type="uml:EnumerationLiteral"

  ownedOperation xmi:type="uml:Operation"

  ownedParameter xmi:type="uml:Parameter"

  ownedRule xmi:type="uml:Constraint"

  packagedElement xmi:type="uml:Association"
  packagedElement xmi:type="uml:Class"
  packagedElement xmi:type="uml:Enumeration"

  packageImport xmi:type="uml:PackageImport"

  specification xmi:type="uml:OpaqueExpression"

  uml:Package xmi:type="uml:Package"

  upperValue xmi:type="uml:LiteralUnlimitedNatural"


## Prototype

The UML-CLASS class represents the first working prototype of a "stub"
class for unmarshalling the UML metadmodel described in UML.xmi

|#

(defclass transformation-model-component ()
  ((model
    :initarg :model
    :type transformation-model
    :accessor component-transformation-model)

   ))


(declaim (type transformation-model *xmi-unmarshalling-model*))

(defvar *xmi-unmarshalling-model*)

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

  (:metaclass uml-tranform-class)

  ;; FIXME: add :MODEL to other class definitions (?)
  (:model *xmi-unmarshalling-model*)

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
  (:namespace
   ("uml" "http://www.omg.org/spec/UML/20110701" ))
  (:uml-name "UML::Classifier")
  (:is-absract . t))
