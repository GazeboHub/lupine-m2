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

The set of possible types of definitions that may be validly specified
in the 'packagedElement' relation is constrained in UML itself. Note
the 'PackageableElement' feature in the following - from UML.xmi, in
the definition of the type 'uml:Package'

 <ownedAttribute xmi:type="uml:Property"
   xmi:id="Package-packagedElement"
   name="packagedElement"
   visibility="public"
   type="PackageableElement"
   aggregation="composite"
   subsettedProperty="Namespace-ownedMember"
   association="A_packagedElement_owningPackage">  ... </ownedAttribute>

The 'xmi:type' attribute describes the type of the definition in the
'ownedAttribute' declaration itself. The 'type' value within that
'ownedAttribute' declaration - that 'type' attribute having the same
XML namesapace as the 'ownedAttribute' declaration - that attribute
may be understood as constraining the type of the value that may be
denoted in the 'ownedAttribute' property.

So, in essence, the 'packageableElement' itself is defined with a
fixed range on the possible types that can occur in its xmi:type
element. The value of the xmi:type element would specify a refinement
on that type, as well as identifying the exact (CL) metaclass or (UML)
type of the element defined in the element definition.


It may be understood that a packagedElement relation is itself
a relation between a UML Package definition and UML PackageableElement
definition.

It may be understood, likewise, that packagedElement and
PackageableElement both may represent types of UML "core" model
elements that should be manually supported in Project Lupine, if
simply for the sake of being able to load the UML model itself, in its
normative XMI encoding (UML.xmi).

There are exactly 21 unique such element/type relations visible in
UML.xmi:

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


|#

(defclass transformation-model-component ()
  ((model
    :initarg :model
    :type transformation-model
    :accessor component-transformation-model)

   ))


(declaim (type transformation-model *xmi-unmarshalling-model*))

(defvar *xmi-unmarshalling-model*)

(defgeneric find-type-transform (namespace element type tmodel))

(defgeneric apply-type-transform (transform source))

(defclass type-transform (transformation-model-componen)
  ((source-element-local-name
    ;; containing element for a typed model quality serialized in XMI
    ;; e.g "uml:ownedAttribute"; "uml:generalization"; "uml:packagedElement"
    :initarg :soruce-element-lname
    :type string ;; ?? FIXME: namespace qualified strings ??
    )
   (source-element-namespace
    :initarg :source-element-ns
    :type string ;; ?? FIXME: represent a namespace as an interned URI ??
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
