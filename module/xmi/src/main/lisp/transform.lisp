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

#+NIL
(defclass lupine-standard-class (standard-class)
  ;; FIXME: Improve and utilize lupine-mop "read-only standard
  ;; slots" protocol in this metaclass' instances
  ())

(defclass model ()
  ((source-serialization-model
    :type serialization-model
    :initarg :source-serialization-model
    :reader model-source-serialization-model
    ;; :read-only t
    )
   (source
    :type uri
    :initarg :source
    :reader model-source)
   (source-encoding
    ;; should be a symbol interned in #:XML/ENCODING
    ;; (FIXME: Denote that package and its usage, in the codebase
    ;; documentation)
    :type symbol
    :initarg :source-encoding
    :accessor model-source-encoding
    ;; FIXME: Provide recode functionaliy on slot-value change.
    ;; (low-priority; complex procedures for nested model string
    ;; element iteration and recoding)
    )
   (id-table
    ;; FIXME: Implement this slot
    ;; Note also in documentation: Persistent element IDs
    )
   (ns-registry
    ;; NB: This slot consumes the :namespaces instance initarg
    :type namespace-registry
    :accessor model-namespace-registry)
   )
  #+NIL (:metaclass lupine-standard-class) ;; cf :READ-ONLY slots
  )

(defmethod shared-initialize :after ((instance model)
				     slots &rest initargs
				     &key namespaces &allow-other-keys)
  (declare (ignore slots initargs))
  (when namespaces
    (flet ((ensure-nsreg ()
	     (cond
	       ((slot-boundp instance 'ns-registry)
		(model-namespace-registry instance))
	       (t
		(let ((nsreg (make-namespace-registry)))
		  (setf (model-namespace-registry instance)
			nsreg))))))
      (let ((nsreg (ensure-nsreg)))
	(ensure-standard-namespaces nsreg)
	(dolist (ns namespaces)
	  (destructuring-bind (prefix . uri) ns
	    (bind-prefix prefix uri nsreg t)))))))
#| TEST

(defparameter *m*
  (make-instance 'model
		 :namespaces
		 '(("uml" . "http://www.omg.org/spec/UML/20110701" )
		   ("xmi" . "http://www.omg.org/spec/XMI/20110701" )
		   ("mofext" . "http://www.omg.org/spec/MOF/20110701")
		   )))

 (mapcar #'(lambda (n)
	    (compute-qname-symbol  n (model-namespace-registry *m*)))
	'("xmi:XMI" "uml:Package" "mofext:Tag"))



|#


(defclass serialization-model (model)
  ((model-class
    ;; cf. ALLOCATE-MODEL, BOOTSTRAP-METAMODEL
    :type class-designstor
    :initarg :model-class
    :accessor serialization-model-model-class)
   (qname-override-p
    ;; FIXME: Use this slot's value during qname resolution in
    ;; TRANSFORM-ELEMENT cf. Namespaces.md (Lupine-XMI Documentation)
    :type boolean
    :initarg :qname-override
    :accessor serialization-model-qname-override-p
    )
   ))


;;; * Element transformation protocol

(defgeneric allocate-model (encoding source serialzation-model)
  (:method ((encoding symbol) (source t)
	    (serialization-model bootstrap-metamodel))
    ;; the bootstrap-metamodel is used effectively to bootstrap itself
    (setf (model-source serialization-model) source)
    (setf (model-source-encoding serialization-model) encoding)
    (values serialization-model))
  (:method ((encoding symbol) (source t)
	    (serialzation-model serialization-model))
    (let ((m (allocate-instance
	      (find-class
	       (serialization-model-model-class serialzation-model)))))
      (setf (model-source m) (compute-source-uri source))
      (setf (model-source-encoding m) encoding))))

(defgeneric compute-element-class (qname namespace name
			   model serialzation-model)
  (:method ((qname symbol) (ns simple-string) (name simple-string)
	    (model bootstrap-metamodel) (source t)
	    (serialization-model serialzation-model))
    (declare (ignore ns name model source serialization-model))
    ;; FIXME (Documentation) : Denote this usage of
    ;;  (namespace, package, symbol, class name)
    ;; with regards to BOOTSTRAP-METAMODEL
    (find-class qname nil)))

;; To Do: Determine the relation between "Property type" and
;; "metaclass contained by property"
;;
;;  e.g. with regards to the definition of the packagedElement
;;  package property in the following excerpt from UML.xmi
;;
;;  <xmi:XMI ...
;;    <uml:Package xmi:type="uml:Package" name="UML"
;;       URI="http://www.omg.org/spec/UML/20110701">
;;       <packagedElement name="Package"
;;                        xmi:type="uml:Class" ...
;;         <ownedAttribute name="packagedElement"
;;                         xmi:type="uml:Poperty"
;;                         type="PackagableElement" ...
;; Some observations:
;;
;; * packagedEleent is the name of a property defined to the
;;   UML::Package metaclass. packagedElement is not in itself a
;;   UML metaclass.
;;
;; * The xmi:type defined for pacakgedElement as an
;;   ownedAttribute belongs effectively to the packagedElement
;;   element itself - the element defined in the given
;;   specification - it denoting that element's type within
;;   the element's (in this case, property) relation to its
;;   containing metaclass, namely UML::Package
;;
;; * The non-namespace-qualified 'type' attribute denotes a
;;   base type for elements defined in that relation. As
;;   demonstrated in the example itself, a packagedElement
;;   declaration may serve to define a type extending that
;;   base type for the packagedElement property - as noting that
;;   uml:Class is a subtype of PackagableElement
;;
;; So, in extensional conclusions:
;;  1) Insofar as the type of the property specified in the
;;     element is known (as would be known in a fully defined
;;     metamodel), a call to change-class may not be
;;     needed. Note that ownedAttribute defines a relation of
;;     a property to a metaclass (as of the packagedElement
;;     relation in its relation to the Package metaclass).
;;     Inasmuch, the specification of @xmi:type="uml:Property"
;;     would seem redundant
;;
;;  2) In NS/XMI:|type| the class of the element being defined
;;     to its containing element or model is published - as
;;     with the uml:Property type of the packagedElement
;;     relation thusly fedined
;;
;;  3) Insofar a uml:Property defines a relation between
;;     subject and object - as with a tuple (s,p,o) in
;;     defining p' as the property - the
;;     non-namespace-qualified 'type' attribute denotes the
;;     type of the object in that relation, not of the
;;     relation in itself

;; Note also: once the instance is created (whether as an
;; instance of a forward-referenced class or otherwise), it
;; must be bound to its container e.g. (FIXME TO DO)
;;
;;   (BIND-ELEMENT ELEMENT PROPERTY CONTAINER)
;;
;; in which case the PROPERTY (if a named element) takes up
;; its qualified, compound name (as derived from its name and
;; the name of the direct and effective containers)
;;
;; see also: IMPORT-ELEMENT (TO DO) cf package import
;; relations in UML


(defgeneric allocate-element (qname namespace name
			      model source serialzation-model)
  (:method ((qname symbol) (ns simple-string) (name simple-string)
	    (model bootstrap-metamodel) (source t)
	    (serialization-model serialzation-model))
    ;; FIXME: Later, audit this method for application not only w.r.t
    ;; BOOTSTRAP-METAMODEL, BOOTSTRAP-METAMODEL-ELEMENT (broaden)

    ;; See also: COMPUTE-QNAME-SYMBOL; UML-CLASS

    ;; To a note about how the XMI evaluator may operate:
    ;;
    ;; Functionally, the xmi:XMI element serves as a contaier for an
    ;; XML-encoded UML metamodel, such that in UML.xmi[1] serves as a
    ;; serialization of the UML metamodel itself - where, in effect,
    ;; the XMI UML encoding is serving as a sort of functional
    ;; meta-metamodel (introducing, perhaps, some conceputal ambiguity
    ;; with regards to MOF) a sort of functional meta-metamodel in
    ;; which the UML metamodel itself is encoded
    ;;
    ;; Functionally, Lupine-XMI will regard each xmi:XMI element as
    ;; it introducing a new model - whether that model represents a
    ;; meta-model or simply a user model.
    ;;
    ;; Specifically in regards to UML.xmi, xmi:XMI contains two
    ;; elements - once, a 'uml:Package' element (UML 2.4.1
    ;; namespace[2]) defining the package named "UML", in its entire
    ;; conjoined infrastructure/superstructure model[3]. The second
    ;; element represents an XML namespace prefix for that package,
    ;; explicitly encoded in addition to the 'URI' property contained
    ;; on the previous UML package element
    ;;
    ;; Concerning what the processing model must consume, on processing
    ;; the contents of uml:Pacakge: Every element defined within that
    ;; 'uml:Package' XML element is defined within a 'packagedElement'
    ;; XML element.
    ;;
    ;; [1] <http://www.omg.org/spec/UML/20110701/UML.xmi>
    ;; [2] <http://www.omg.org/spec/UML/20110701>
    ;; [3] Effectiely, UML.xmi serves to present the
    ;;     UML superstructure after all package merge operations. Although
    ;;     the package structure presented in UML.xmi does not match
    ;;     the package structure presented in either Infrastructure.xmi
    ;;     or Supersturcture.xmi, but at least in regards to that the
    ;;     "UML" package in UML.xmi may represent the only normative
    ;;     definition of the "UML" package together with its namespace
    ;;     URI[2], perhaps it may be implied that UML.xmi represents
    ;;     the exclusive normative encoding of UML 2.4.1 such as may be applied for purpose for XMI format model
    ;;     serializations
    (let ((c (compute-element-class qname ns name
				    model serialization-model)))
      ;; usage:
      ;; 1. Inputs:
      ;;      qname = UML::|Package|
      ;;      ns="http://www.omg.org/spec/UML/20110701"
      ;;      name="Package"
      ;;      model, serilization-model ...
      ;;
      ;;    Intended behavior: find class UML::|Package|,
      ;;                       allocate-instance of that class
      ;; 2 Inputs:
      ;;      qname = UML::|packageImport|
      ;;      ns="http://www.omg.org/spec/UML/20110701"
      ;;      name="packageImport"
      ;;      model, serilization-model ...
      ;;
      ;;    Intended behavior: allocate an instance of a class for
      ;;    represeting the property defined in the packageImport
      ;;    specification.
      ;;
      ;;    (find-class (quote UML::packageImport)) may suffice
      ;;


      (allocate-instance c))
    ))



(defgeneric apply-atrribute (value qname namespace name element
			     model source serialzation-model)
  ;; FIXME: move defmethods to after bootstrap-metamodel-element class definition
  (:method ((value simple-string)
	    (qname (eql ns/xmi:|type|))
	    (ns simple-string) (name simple-string)
	    (element bootstrap-metamodel-element)
	    (model bootstrap-metamodel)
	    (source t) (serialization-model serialzation-model))
    (let ((type (normalize-string value)))
      (multiple-value-bind (prefix name)
	  (split-string-1 #\: type)
	;; FIXME: If COMPUTE-ELEMENT-CLASS does not return a class, define a
	;; FORWARD-REFERENCED-CLASS with TYPE denoting the cname
	;; of that class - deriving the metaclass of that class from
	;; MODEL or SERIALIZATION-MODEL
	;;
	;; ^  wrap in an ENSURE-METACLASS call
	(let ((c (compute-element-class
		  ;; FIXME: ^ what function to call, there?
		  (compute-qname-symbol
		   type
		   (model-namespace-registry
		    ;; FIXME:
		    ;; use model or serialiation-model here?
		    serialization-model)))))

      (change-class element c)))

  (:method ((value simple-string)
	    (qname symbol)
	    (ns simple-string) (name simple-string)
	    (element bootstrap-metamodel-element)
	    (model bootstrap-metamodel)
	    (source t) (serialization-model serialzation-model))
    ;; FIXME: Later, audit this method for application not only w.r.t
    ;; BOOTSTRAP-METAMODEL, BOOTSTRAP-METAMODEL-ELEMENT (broaden)

    ;; FIXME: "REAL WORK HERE"
    ;; See first: allocate-element, UML class definition, and
    ;; UML-CLASS "Property table" slot definitions (special cases:
    ;; generalizations, owned attributes, owned operations, owned
    ;; rules cf. OCL, and documentation-stub handling. also address
    ;; UML associations and UML enumeration metaclasses)

    ;; NB also: (eql ns/xmi:type) and CHANGE-CLASS (w.r.t class
    ;; selection in ALLOCATE-ELEMENT, and the specifications by which
    ;; that behavior will be defined in the Lupine-XMI UML
    ;; stub/metamodel unmarshaling protocol)

    ))


(defgeneric add-element (element container
			 model serialization-model))
;; ^ FIXME: In conceptual regards, clarify whether that function is
;; for adding an XML element or a model element. Contrast with
;; ADD-TYPED-RELATION

;; ^ FIXME (TO DO): Note that behaviors of ADD-ELEMENT may
;; differ per the type of the ELEMENT. For instance, a
;; packageImport element when added to a Package must result in the
;; imported package's contents becoming visible in the containing
;; package - such that must be handled in add-allocated-element,
;; however exactly "packaged element->package" visibility may
;; ultimately be handled in the Project Lupine UML API -
;; wheres a packagedElement being added to a package simply represents
;; the binding of a definition by way of a packagedElement property,
;; within the containing package.


#|

# UML Primitive Types

### UML PrimitiveTypes Package

* Package URI: <http://www.omg.org/spec/PrimitiveTypes/20110701>

### Primtiive Types - Mapping to Common Lisp (Trivial)

* PrimitiveTypes::Boolean -> CL:BOOLEAN
* PrimitiveTypes::Integer -> CL:INTEGER
* PrimitiveTypes::Real -> CL:REAL
* PrimitiveTypes::String -> CL:STRING
* PrimittiveTypes::UnlimitedNatural -> (or unsigned-byte (eql :*))


# Model Elements

## Model Element Types (Bootstrap model)

### Abstract Element Types (Bootstrap model)

* UML::Element
* UML::NamedElement
* UML::Namespace
* UML::Classifier

### Metaclass Types

* UML::Class

### Other Types
* UML::Association
* UML::Enumeration
* UML::Package
* UML::Property

### UML Packages

### UML Classes

#### Class Generalizations (from Classifier)

#### Class Attributes

#### Class Operations (from ???)
(Note: It's a matter of modeling the operations, not _per se_ of|
implementing the opeations)

#### Owned Rules (from Namespace)

#### Comments (from Element)

### UML Profiles

(extending UML::Package)
(metaclass,profle relations - by way of UML packages)
(stereotypes - extending metaclasses)



|#

#+NIL
(defgeneric add-typed-relation (relation container model serialization-model))
;; ^ no essential distinction with regards to ADD-ELEMENT, simply for
;; adding an element that repersents a typed relation

(defgeneric final-initialize (element container
			      model serialization-model)
  (:method ((element model-element) (container model-element)
	    (model model)
	    (serialization-model serialization-model))
    (initialize-instance element)
    (add-element element container model serialization-model)
    (values element)))


(defgeneric add-cdata (encoding cdata model-element model))
;; ^ cf. READ-XMI
;; ^ FIXME: Implement ADD-CDATA for those bootstrap metamodel elements
;; accepting cdata contents (cf. ownedComment, and the
;; non-OCL-interpreting ownedRule impl)


;;; * Bootstrap Metamodel

(defconstant +package-buffer-extent+ 1) ;; FIXME: usage ?

(defclass bootstrap-metamodel (serialization-model)
  ;; effectively a container for XMI -> Common Lisp transformation
  ;; descriptors, so far as to unmarshal the full UML metamodel
  ;; serialized in UML.xmi
  ;;
  ;; Regarding disjunction of this class with regards to other
  ;; SERIALIZATION-MODEL classes, refer to generic functions having
  ;; methods specializced this class and on SERIALIZATION-MODEL,
  ;; e.g. ALLOCATE-MODEL
  ())



(declaim (type bootstrap-metamodel *bootstrap-metamodel*)) ;; FIXME class?
(defvar *bootstrap-metamodel* ;; name ??
  (make-instance
   'bootstrap-metamodel
   :model-class (quote bootstrap-metamodel)
   :namespaces
   ;; FIXME: BACKWADS COMPATABILITY (PRE 2.4.1 SERIES UML/XMI/MOF SPECS)
   '(("uml" . "http://www.omg.org/spec/UML/20110701" )
     ("xmi" . "http://www.omg.org/spec/XMI/20110701" )
     ("mofext" . "http://www.omg.org/spec/MOF/20110701")
     )))


(defclass bootstrap-metamodel-element ()
  ;; used in METAMODEL-TRANSFORM and in BOOTSTRAP-METAMODEL-METACLASS
  ((model
    ;; model containing this component
    :initarg :model
    :type serialization-metamodel
    :initform *bootstrap-metamodel*
    :accessor component-model)

   ;; naming elements
   (namespace ;; FIXME: Discard this slot (?)
    ;; namespace corresponding to the package containing the named
    ;; element reified by this component
    :type namespace
    :initarg :namespace
    :accessor component-namespace)

   (local-name ;; FIXME: Discard this slot, or inherit by NAMED-ELEMENT
    ;; ^ local name for type of metamodel serialization.
    :initarg :local-name
    :type string ;; FIXME: namespace qualified strings - see ensure-qname
    :accessor component-local-name)
   ))

#+NIL ;; needs more baseline code
(defgeneric (setf resolve-composite-name) (new-value name model))
;; ^ return local-name

#+NIL ;; needs more baseline code
(defgeneric resolve-composite-name (name model &optional errorp))

#+NIL ;; needs cleanups
(defgeneric resolve-qname (name model &optional errorp)
  ;; some axioms:
  ;;
  ;; * before this function is called, MODEL must contain a set
  ;; of (URL, prefix+) namespace bindings
  ;;
  ;; * every element used in a serialized metamodel (i.e every
  ;; metamodel element) represents a model element or a feature of a
  ;; model element
  ;;
  ;; * every metamodel element supported in the stub metamodel - that
  ;; is, the bootsrap-metamodel - may be represented with a metaclass
  ;; or a slot definition contained by such a metaclass, such that may
  ;; be registered to the bootstrap-metamodel when or after the
  ;; metaclass is defined
  ;;
  ;; * once every such metaclass is defined in Lisp and furthermore is
  ;; registered to the bootstrap-metamodel, the bootstrap-metamodel
  ;; may then be used for unmarshallimg the entire UML metamodel
  ;; serialized in UML.xmi. (That unmarshaling procedure may result in
  ;; discrete modifications onto the same metaclasses used in
  ;; unmarshalling that metamodel, or alternately it would result in
  ;; the production of an objectively distinct model, however whose
  ;; metaclasses may inherit from metaclasses defined in the
  ;; bootstrap-metamodel)
  ;;
   ;; * once the UML metamodel is completely defined, then it may be
  ;; presented for graphical operations via CLIM presemtation methods
  ;;
  ;;
  ;; That outline basically describes the design intention underlying
  ;; the implementation of the Lupine-XMI components.
  ;;
  ;; In the implementation of the Lupine-XMI metamodel unmarshaling
  ;; model, as denoted in the previous outline, a design issue has
  ;; been noticed with regards to namespace defaulting. That design
  ;; issue and a workaround for that design issue have been denoted in
  ;; Namespaces.md in the Lupine XMI module's documentation

  (:method ((name string) model &optional (errorp t))
    (resolve-qname (simplify-string string) model errorp))

  (:method ((name simple-string) (model bootstrap-metamodel)
	    &optional (errorp t))
    (let ((ns-reg (model-namespace-registry model)))
      (multiple-value-bind (pfx lname)
	  ;; FIXME: allow for "foo" as well as "bar:foo"
	  ;; i.e. "Null namespace" (but note: program must make
	  ;; account for containing namespaces during element
	  ;; processing) (see Namespaces.md)
	  (split-string-1 #\: name)

	;; FIXME: use symbol bindings instead!
	(flet ((resolve-in-namespace (ns)
		 (gethash lname (namespace-local-names-table ns))))
	  (cond
	    (pfx
	     (let ((ns (resolve-prefix-namespace pfx)))
	       (cond
		 (ns
		  (let ((ln-p (resolve-in-namespace ns)))
		    (cond
		      (ln-p (values ln-p))
		      (errorp
		       (error "Local name ~s not registered in ~
namespace ~s" ns))
		      (t (values nil ns)))))
		 (errorp
		  (error "No namepace registered for prefix ~s in ~s ~
ns-registry ~s"
			 pfx model nsreg))
		 (t (values nil nil)))))
	    (t  ;; element name was not qualified with a prefix
	     ;; FIXME: this assumes no namespace has been defaulted (?)
	     (let ((ns (registry-null-namespace ns-reg)))
	       (resolve-in-namespace ns)))))))))


(defmethod shared-initialize ((instance bootstrap-metamodel-element)
			      slots &rest initargs
			      &key qname &allow-other-keys)
  ;; This method consumes the :QNAME initarg
  (macrolet ((uncadr (name)
	       (with-gensyms (it)
		 (let ((,it (getf initargs ,name)))
		   (cond
		     ((and ,it (consp ,it))
		      (setf (getf initargs ,name)
			    (cadr ,it)))
		     (t (values ,it)))))))
    (let ((model (uncadr :model))
	  (name (uncadr :qname)))
      (when name
	;; FIXME:
	;; 1) split qname into name, prefix
	;; 2) resolve namespace from prefix,
	;;    (setf getf) resolved namespace as :namespace initarg value
	;; 3) (setf getf) local name as name value
	(let ((reg (model-namespace-registry model)))
	  (multiple-value-bind (s ns)
	      (compute-qname-symbol name reg t)
	    (setf (getf initargs :local-name)
		  (symbol-name s))
	    (setf (getf initargs :namespace)
		  ns)
	    (delf :qname initargs)))

      (prog1 (apply #'call-next-method instance slots initargs)
	(when (and name model)
	  (let ((local-name (setf (resolve-composite-name name model)
				  instance)))
	    (setf (component-local-name instance)
		  local-name)))))))

;;; * ...


(defclass metamodel-transform (bootstrap-metamodel-element)
  ;; this may be subject to some revision - note the irrelevance of
  ;; the 'type' slot and the corredponding @xmi:type attribute, with
  ;; regards to "simple" metamodel unmarshaling

  ((element-p
    ;; whether the metamodel element is known to be serialized as an
    ;; XML element
    :initarg :element-p
    :type boolean
    :accessor transform-element-p)
   (attribute-p
    ;; whether the metamodel element is known to be serialized as an
    ;; XML attribute
    :initarg :attribute-p
    :type boolean
    :accessor transform-attribute-p)

   ;; Note that xmi:type does not need to be implemented directly in
   ;; the metamodel unmarshalling framework.
   ;;
   ;; The input xmi:type value would rather denote the metaclass
   ;; of the element being umarshaled
   ))


;;; * Property Tables
;;
;; Precursor to PROPERTY-TRANSFORM-SLOT-DEFINITION
;;
;; Precedent: Unique trie of (value, name, table)
;;
;; Usage in UML:
;;
;;      name => persistent XML ID, as a symbol interned in the model's
;;              ID symbol namespace (cf. INTERN-ID, to do)
;;      value => UML object representing <name>


(defclass property-table ()
  ;; usage: e.g. to store the set of packageElement properties
  ;; definied within a UML Package
  ())

(defclass named-property-table (named-element)
  ;; usage: e.g. to store the set of packageElement properties
  ;; definied within a UML Package
  ())


(defgeneric get-property (name table))
(defgeneric ensure-property (value name table))
(defgeneric map-properties (function table))



(defclass simple-eq-property-table ()
  ((storage
    :type hash-table
    :initform (make-hash-table :test #'eq))))

(defmethod get-property ((name symbol) (table simple-eq-property-table))
  (getf name table))

(defmethod ensure-property ((value t) (name symbol)
			   (table simple-eq-property-table))
  (setf (getf name table) value))

(defmethod map-properties ((function function)
			   (table simple-eq-property-table))
  (maphash function table))


;;; * Property-Oriented Slot Definitions

#|

Motivation:

A UML metaclass (type UML::Class) is defined with a set of properties,
in the following class attribute groups:

* Owned Attributes
* Owned Operations
* Owned Rules (Constraints, inherited from UML::Namespace)
* Owned Comments (inherited from UML::Element)

The following slot definition protocol represents a generic framework
for:

  1) Define a single slot definition for each such attribute goup,
     within a slot vulue of the metacalss

  2) Add/Set/Delete/List/Map properties within each such attribute
     group, within that slot's value on the metaclass

FIXME: As it representing a thin and perhaps ill fitting layer on an
ordinary instance access protocol, this slot definition framework
should be deleted

|#


(defclass property-transform-slot-definition
    ;; FIXME: THIS THING NEEDS WORK TOO
    (metamodel-transform slot-definition)
  ())

(defclass direct-property-transform-slot-definition
    (metamodel-transform-slot-definition standard-direct-slot-definition)
  ())

(defclass effective-property-transform-slot-definition
    (metamodel-transform-slot-definition standard-effective-slot-definition)
  ())

;; * Trasform-Class

(defclass bootstrap-metamodel-metaclass
    (bootstrap-metamodel-element standard-class)
  ())


(defmethod direct-slot-definition-class ((class bootstrap-metamodel-metaclass)
					 &rest initargs)
  ;; towards a pattern: type-specialized slot definitions

  (destructuring-bind (&key type &allow-other-keys)
      initargs
    (cond
      ((subtypep type 'property-table)
       (find-class 'direct-property-transform-slot-definition))
      (t (call-next-method))))
  #+NIL
  (destructuring-bind (&key local-name &allow-other-keys)
      initargs
    (cond
      (local-name
       (find-class 'direct-property-transform-slot-definition))
      (t (call-next-method)))))

(defmethod effective-slot-definition-class ((class bootstrap-metamodel-metaclass)
					    &rest initargs)
  (destructuring-bind (&key name &allow-other-keys)
      initargs
    (let ((dslots (compute-direct-slot-definitions name class)))
      (delare (type cons dslots))
      (cond
	((some #'(lambda (sl)
		   (typep sl 'direct-property-transform-slot-definition))
	       dslots)
	 (find-class 'effective-property-transform-slot-definition))
	(t (call-next-method))))))


(defmethod get-property ((name symbol) (table bootstrap-metamodel-metaclass))
  (find-slot-definition name table))


;;; * UML-Class

(def-uml-package "UML") ;; ?


(defclass uml-class (classifier bootstrap-metamodel-metaclass)
  ;; NOTE: This class represents the main initial use-case for the
  ;; transformation algorithm proposed in Lupine XMI
  ((owned-attributes
    :element-p t
    :local-name "ownedAttribute" ;; qname (?)
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
    :attribute-p t
    :local-name "isAbstract"
    :initarg :is-abstract
    :type boolean)
   )

  (:metaclass bootstrap-metamodel-metaclass)

  ;; FIXME: add :MODEL to other class definitions (?)
  (:model *bootstrap-metamodel*)

  ;; UML composite name
  ;;
  ;; This needs a "UML" package defined for appropriate resolution,
  ;; however.
  ;;
  ;; This class option would effectively denote a packagedElement
  ;; definition for the defining class.
  (:qname "UML:Class")

  ;; "uml" in the following item denotes the namespace URI assigned
  ;; to the prefix "uml"
  (:model-metaclass  "uml:Class")  ;; cf. @xmi:type="uml:Class"
  )

#+Nil ;; FIXME: do this CHANGE-CLASS for all elements in the
;; *bootstrap-metamodel* after loading
(let ((c (find-class 'uml-class)))
  (change-class c c))


;;; * Post Hoc


;; N.B: Can't use UML-CLASS as a :METACLASS util it's finalized,
;; and it won't be finalized until after DEFCLASS CLASSIFIER

(defclass element ()
  ((owned-comments
    :element-p t
    :local-name "ownedComment"
    :initarg :owned-comments
    :type property-table
    :accessor class-direct-owned-comments-table))
  (:metaclass bootstrap-metamodel-metaclass)
  (:model *bootstrap-metamodel*)
  (:model-metaclass  "uml:Class")
  (:qname "UML:Element")
  (:is-abstract t))


(defclass named-element (element)
  ((name
    :attribute-p t
    :local-name "name"
    :initarg :name
    :type simple-string
    ;; fixme: :type simple-ncname ;; ?
    ;; (NB: NCName type for 'name' is not specified in UML, but may be
    ;; implied on account of the XMI serialization for UML named
    ;; elements, in which a named element's name is used as an XML
    ;; element name, optionally qualified within a XML namespace
    ;; having the URI of the UML package conaining the element)
    :accessor named-element-name)
   (namespace
    ;; not directly encoded in XMI, rather derived from when a
    ;; named-element is contained in a packagedElement relation (as
    ;; that relation entailing a subset of Namespace.ownedMember)
    ;; cf UML.xmi#A_ownedMember_namespace
    :intiarg :namespace
    :type namespace
    :accessor named-element-namespace
    )
   )
  (:metaclass bootstrap-metamodel-metaclass)
  (:model *bootstrap-metamodel*)
  (:model-metaclass  "uml:Class")
  (:qname "UML:NamedElement")
  (:is-abstract t))


(defclass namespace (named-element)
  ((owned-rules
    :element-p t
    :local-name "ownedRule"
    :initarg :owned-rules
    :type property-table
    :accessor class-direct-owned-rules-table
    ))
  (:metaclass bootstrap-metamodel-metaclass)
  (:model *bootstrap-metamodel*)
  (:model-metaclass  "uml:Class")
  (:qname "UML:Namespace")
  (:is-abstract t))



(defun compute-composite-name (named)
  ;; FIXME: note that the PrimitiveTypes package is the singleton
  ;; denoted with "::" as a prefix part. The InfrastructureLibrary
  ;; package, for instance, i not denoted with the "::" prefix in the
  ;; UML 2.4.1 specification, though it may be understood as
  ;; representing a "top level" package
  ;;
  ;; This implementation will skip the prefix "::" altogether
  (declare (type named-element named)
	   (values simple-string &optional))
  (let ((ns (when (slot-boundp named 'namespace)
	      (named-element-namespace named)))
	(name (named-element-name named)))
    (cond
      (ns
       (concatenate 'simple-string name
		    #.(simplify-string "::")
		    (compute-coposite-name ns)))
      (t (values  name)))))
#+NIL
(defmethod (setf resolve-composite-name) ((new-value bootstrap-metamodel-element)
					  (name string)
					  (model bootstrap-metamodel)
					  )
  ;; FIXME: complete this method definition
  )

#+NIL
(defmethod resolve-composite-name ((name string)
				   (model bootstrap-metamodel)
				   &optional (errorp t))
  ;; FIXME: complete this method definition
  )


(defclass classifier (namespace #+NIL type)
  ((generalizations
    :element-p t
    :local-name "generalization"
    :initarg :generalizations
    :type property-table
    :accessor class-direct-generalizations-table
    ))
  (:metaclass bootstrap-metamodel-metaclass)
  (:model *bootstrap-metamodel*)
  (:model-metaclass "uml:Class")
  (:qname "UML:Classifier")
  (:is-abstract t))


(defclass uml-package (namespace)
  ((uri ;; FIXME: intern as namespace in containing model
    :attribute-p t
    :local-name "URI"
    :initarg :uri
    :type string
    :accessor uml-package-uri)
   (packaged-elements
    :element-p t
    :local-name "packagedElement"
    :type property-table
    :accessor uml-package-packaged-elements))
  (:metaclass bootstrap-metamodel-metaclass)
  (:model *bootstrap-metamodel*)
  (:model-metaclass "uml:Class")
  (:qname "UML:Package"))


;;; * UML Primitive Types


(defclass data-type (classifier)
  ((lisp-type
    :initarg :lisp-type
    :accessor data-type-lisp-type))
  (:metaclass bootstrap-metamodel-metaclass)
  (:model *bootstrap-metamodel*)
  (:model-metaclass "uml:Class")
  (:qname "UML:Package")
  (:is-abstract t))



(defclass primitive-type (data-type standard-class)
  ()
  (:metaclass bootstrap-metamodel-metaclass)
  (:model *bootstrap-metamodel*)
  (:model-metaclass "uml:Class")
  (:qname "UML:PrimitiveType"))


(defclass type-proxy ()
  ;; FIXME: use an AROUND method on (SETF TYPE-PROXY-VALUE) to ensure
  ;; that the NEW-VALUE is of the appopriate LISP-TYPE
  ((value
    :initarg :value
    :accessor type-proxy-value)
   ))

(defclass uml-boolean (type-proxy)
  ((value
    :type boolean))
  (:metaclass primitive-type)
  (:lisp-type boolean)
  (:model *bootstrap-metamodel*)
  (:model-metaclass "uml:PrimitiveType")
  (:qname "PrimitiveTypes:Boolean"))


(defclass uml-integer (type-proxy)
  ((value
    :type integer))
  (:metaclass primitive-type)
  (:lisp-type integer)
  (:model *bootstrap-metamodel*)
  (:model-metaclass "uml:PrimitiveType")
  (:qname "PrimitiveTypes:Integer"))

(defclass uml-string (type-proxy)
  ((value
    :type string))
  (:metaclass primitive-type)
  (:lisp-type string)
  (:model *bootstrap-metamodel*)
  (:model-metaclass "uml:String")
  (:qname "PrimitiveTypes:Integer"))

(deftype unlimited-natural ()
  '(or unsigned-byte (eql :*)))

(defclass uml-unlimited-natural (type-proxy)
  ((value
    :type unlimited-natural))
  (:metaclass primitive-type)
  (:lisp-type unlimited-natural)
  (:model *bootstrap-metamodel*)
  (:model-metaclass "uml:String")
  (:qname "PrimitiveTypes:Integer"))
