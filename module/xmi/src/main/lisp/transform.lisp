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

(defgeneric class-model-metaclass (class))

(decclass lupine-standard-class (standard-class)
	;; FIXME: Improve and utilize lupine-mop "read-only standard slots" protocol in this metsclass' instances
	())

(defclass model ()
  ((source-serialization-model
    :type serialization-model
    :initarg :source-serialization-model
    :reader model-source-serialization-model
    :read-only t)
   (source
    :type uri
    :initarg :source
    :reader model-source)
   (source-encoding
    ;; should be a symbol interned in #:XML/ENCODING (FIXME: Denote that in the codebase documentation)
    :type symbol
    :initarg :source-encoding
    :accessor model-source-encoding
    ;; FIXME: Provide recode functionaliy on slot-vslue change?
    ;; (low-priority; complex procedures for nested model element iteration)
    ))
  (:metaclass lupine-standard-class))

(defclass serialization-model (model)
  ((instance-class
    ;; cf. ALLOCATE-MODEL, BOOTSTRAP-METAMODEL
    :type class-designstor
    :initarg :instance-class
    :accessor :serialization-model-instance-class)
   (qname-override-p
    ;; FIXME: Use this slot's value during qname resolution in TRANSFORM-ELEMENT
    ;; cf. Namespaces.md (Lupine-XMI Documentation)
    :type boolean
    :initarg :qname-override
    :accessor serialization-model-qname-override-p
    )))

;;; * Element transformation protocol


(defgeneric compute-source-uri (source))
;; FIXME: Move this defn into utils-cxml.lisp and define its methods
;; there, cf. allocate-model

(defgeneric allocate-model (encoding source serialzation-model)
  (:method ((encoding symbol) (source t)
	    (serialzation-model serialization-model))
    (let ((m (allocate-instance
	      (find-class
	       (serialization-model-instance-class serialzation-model)))))
      (setf (model-source m) (compute-source-uri source))
      (setf (model-source-encoding m) encoding))))

(defgeneric allocate-element (qname-symbol namespace name
			      model source serialzation-model)
  (:method ((qs symbol) (ns simple-string) (name simple-string)
	    (model stub-metamodel)
	    (source t)
	    (serialization-model serialzation-model))
    ;; FIXME: Later, audit this method for application not only w.r.t
    ;; STUB-METAMODEL, STUB-METAMODEL-ELEMENT (broaden)

    ;; FIXME: "REAL WORK HERE"
    ;; See first: intern-qname-symbol, then UML-CLASS definition

    ))


(defgeneric apply-atrribute (qname-symbol namespace name element
			     model source serialzation-model)
  ;; FIXME: move defmethod to after stub-metamodel-element class definition
  (:method ((qs symbol) (ns simple-string) (name simple-string)
	    (element stub-metamodel-element)
	    (model stub-metamodel)
	    (source t)
	    (serialization-model serialzation-model))
    ;; FIXME: Later, audit this method for application not only w.r.t
    ;; STUB-METAMODEL, STUB-METAMODEL-ELEMENT (broaden)

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


(defgeneric final-initialize (model context serialization-model)
  (:method ((model model) (context t)
	    (serialization-model serialization-model))
    (initialize-instance model)))

(defgeneric add-cdata (encoding cdata model-element model))
;; ^ cf. READ-XMI
;; ^ FIXME: Implement ADD-CDATA for those bootstrap metamodel elements
;; accepting cdata contents (cf. ownedComment, and the
;; non-OCL-interpreting ownedRule impl)


;;; * Bootstrap Metamodel

(defconstant +package-buffer-extent+ 1)

(defclass bootstrap-metamodel (serialization-model)
  ;; effectively a container for XMI -> Common Lisp transformation
  ;; descriptors, so far as to unmarshal the full UML metamodel
  ;; serialized in UML.xmi
  ((ns-registry
    ;; NB: This slot consumes the :namespaces instance initarg
    :type namespace-registry
    :accessor bootstrap-metamodel-ns-registry)
   (root-packages
    ;; Notice that UML-PACKAGE will not yet have been defined,
    ;; when this class' definition is initially evaluted
    :type (vector uml-package)
    :initform (make-array #.+package-buffer-extent+
			  :element-type 'uml-package
			  :fill-pointer 0
			  :adjustable t)
    ))
  (:default-initargs :instance-class bootstrap-metamodel))


(defmethod shared-initialize :after ((instance bootstrap-metamodel)
				     slots &rest initargs
				     &key namespaces &allow-other-keys)
  (declare (ignore slots initargs))
  (when namespaces
    (flet ((ensure-nsreg ()
	     (cond
	       ((slot-boundp instance 'ns-registry)
		(bootstrap-metamodel-ns-registry instance))
	       (t
		(let ((nsreg (make-namespace-registry)))
		  (setf (bootstrap-metamodel-ns-registry instance)
			nsreg))))))
      (let ((nsreg (ensure-nsreg)))
	(dolist (ns namespaces)
	  (destructuring-bind (prefix . uri) ns
	    (ensure-prefix prefix uri nsreg)))))))



(declaim (type bootstrap-metamodel *boostrap-model*)) ;; FIXME class?
(defvar *boostrap-model* ;; name ??
  (make-instance
   'bootstrap-metamodel
   :namespaces
   '(("uml" . "http://www.omg.org/spec/UML/20110701" ))))


(defclass stub-metamodel-element ()
  ;; used in METAMODEL-TRANSFORM and in METAMODEL-STUB-CLASS
  ((model
    ;; model containing this component
    :initarg :model
    :type bootstrap-metamodel
    :initform *bootstrap-metamodel*
    :accessor component-model)

   ;; naming elements
   (namespace
    ;; namespace corresponding to the package containing the named
    ;; element reified by this component
    :type namespace
    :initarg :namespace
    :accessor component-namespace)
   (local-name
    ;; ^ local name for type of metamodel serialization. (Note that
    ;; that name must identify a single named element in the
    ;; serialization metamodel )
    :initarg :local-name
    :type string ;; FIXME: namespace qualified strings - see ensure-qname
    :accessor component-local-name)
   ))

(defgeneric (setf resolve-composite-name) (new-value name model))
;; ^ return local-name

(defgeneric resolve-composite-name (name model &optional errorp))

(defgeneric resolve-qname (name model &optional errorp)
  ;; some axioms:
  ;;
  ;; * before this function is called, MODEL must contain a set
  ;; of (URL, predix+) namespace bindings
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
  ;; * once every such metsclass is defined in Lisp and furthermore is
  ;; registered to the bootstrap-metamodel, the bootstrap-metamodel
  ;; may then be used for unmarshallimg the entire UML metamodel
  ;; serialized in UML.xmi. (That unmarshaling procedure may result in
  ;; discrete modifications onto the same metaclasses used in
  ;; unmarshalling that metamodel, or alternately it would result in
  ;; the production of an objectively distinct model, however whose
  ;; metaclasses may inherit from metsclasses defined in the
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
    (let ((ns-reg (bootstrap-metamodel-ns-registry model)))
      (multiple-value-bind (pfx lname)
	  ;; FIXME: allow for "foo" as well as "bar:foo"
	  ;; i.e. "Null namespace" (but note: program must make
	  ;; account for containing namespaces during element
	  ;; processing) (see Namespaces.md)
	  (split-string-1 #\: name)
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

(defmethod shared-initialize ((instance stub-metamodel-element)
			      slots &rest initargs
			      &key &allow-other-keys)
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
	;; FIXME: split qname into name, prefix. resolve.
	(setf (getf initargs :qname)
	      (simplify-string name)))
      (prog1 (apply #'call-next-method instance slots initargs)
	(when (and name model)
	  (let ((local-name
		  (setf (resolve-composite-name name model)
			instance)))
	    (setf (component-local-name instance)
		  local-name)))))))


;;; * ...


(defclass metamodel-transform (stub-metamodel-element)
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

(defclass metamodel-stub-class (stub-metamodel-element standard-class)
  ((model-metaclass
    :initarg :model-metaclass
    :types simple-string
    :accessor class-model-metaclass
    )
   ))


(defmethod shared-initialize ((instance metamodel-stub-class)
			      slots &rest initargs
			      &key &allow-other-keys)
  (macrolet ((uncadr (name)
	       (with-gensyms (it)
		 (let ((,it (getf initargs ,name)))
		   (when (and ,it (consp ,it))
		     (setf (getf initargs ,name)
			   (cadr ,it)))))))
    ;; FIXME: resolve MODEL-METACLASS (as a QName) onto model
    ;; namespace registry (prefix/ns bindings) in MODEL
    (uncadr :model-metaclass)
    ;; FIXME: resolve COMPOSITE-NAME ...
    (apply #'call-next-method instance slots initargs)
    ))


(defmethod direct-slot-definition-class ((class metamodel-stub-class)
					 &rest initargs)
  (destructuring-bind (&key source-local-name &allow-other-keys)
      initargs
    (cond
      (source-local-name
       (find-class 'direct-property-transform-slot-definition))
      (t (call-next-method)))))

(defmethod effective-slot-definition-class ((class metamodel-stub-class)
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


;;; * UML-Class

(def-uml-package "UML") ;; ?


(defclass uml-class (classifier metamodel-stub-class)
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

  (:metaclass metamodel-stub-class)

  ;; FIXME: add :MODEL to other class definitions (?)
  (:model *boostrap-model*)

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
;; *boostrap-model* after loading
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
  (:metaclass metamodel-stub-class)
  (:model *boostrap-model*)
  (:model-metaclass  "uml:Class")
  (:qname "UML:Element")
  (:is-abstract t))


(defclass named-element (element)
  ((name
    :attribute-p t
    :local-name "name"
    :initarg :name
    :type simple-string ;; fixme: NQname
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
  (:metaclass metamodel-stub-class)
  (:model *boostrap-model*)
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
  (:metaclass metamodel-stub-class)
  (:model *boostrap-model*)
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
(defmethod (setf resolve-composite-name) ((new-value stub-metamodel-element)
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
  (:metaclass metamodel-stub-class)
  (:model *boostrap-model*)
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
  (:metaclass metamodel-stub-class)
  (:model *boostrap-model*)
  (:model-metaclass "uml:Class")
  (:qname "UML:Package"))
