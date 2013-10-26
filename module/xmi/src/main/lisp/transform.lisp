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


;;; * Element transformation proto.

(defconstant +package-buffer-extent+ 1)

(defclass bootstrap-metamodel ()
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
   )))


(defgeneric add-element (element model))

(defgeneric find-element (name model))

(defgeneric apply-element-transform (element source)) ;; ??


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





(declaim (type bootstrap-metamodel *boostrap-model*))
(defvar *boostrap-model* ;; name ??
  (make-instance 'bootstrap-metamodel
		 :namespaces
		 '(("uml" . "http://www.omg.org/spec/UML/20110701" ))))


(defclass bootstrap-metamodel-component ()
  ;; used in METAMODEL-TRANSFORM and in metamodel-stub-class
  ((model
    :initarg :model
    :type bootstrap-metamodel
    :initform *bootstrap-metamodel*
    :accessor component-model)
   (namespace
   :type namespace
   :initarg :namespace
   :accessor component-namespace
    )
   (local-name
    ;; ^ local name for type of metamodel serialiation. (Note that that
    ;; name will identify a single metamodel element type)
    :initarg :local-name
    :type string ;; FIXME: namespace qualified strings - see ensure-qname
    :accessor component-local-name)))

(defgeneric (setf resolve-composite-name) (new-value name model))
;; ^ return local-name

(defgeneric resolve-composite-name (name model &optional errorp))

(defgeneric resolve-qname (name model &optional errorp)
	;; some axioms:
	;; * before this function is called, MODEL must contain a set of (URL, predix+) namespace bindings
	;; * every element used in a serialized metamodel (i.e every metamodel element) represents a model element or a feature of a model element
	;; * every metamodel element supported in the stub metamodel - that is, the bootsrap-metamodel - may be represented with a metaclass or a slot definition contained by such a metaclass, such that may be registered to the bootstrap-metamodel when or after the metaclass is defined
	;; * once every such metsclass is defined in Lisp and furthermore is registered to the bootstrap-metamodel, the bootstrap-metamodel may then be used for unmarshallimg the entire UML metamodel serialized in UML.xmi. (That unmarshaling procedure may result in discrete modifications onto the same metaclasses used in unmarshalling that metamodel, or alternately it would result in the production of an objectively distinct model, however whose metaclasses may inherit from metsclasses defined in the bootstrap-metamodel)
	;; * once the UML metamodel is completely defined, then it may be presented for graphical operations via CLIM presemtation methods
	;;
	;; That outline basically describes the design intention underlying the implementation of the Lupine-XMI components.
	;;
	;; In the implementation of the Lupine-XMI metamodel unmarshaling model, as denoted in the previous outline, a design issue has been noticed, as would be denoted in Namespaces.md

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
	(cond
	  (pfx
	   (let ((ns (resolve-prefix-namespace pfx )))
	     (cond
	       (ns
		(let ((ln-p (gethash lname (namespace-local-names-table ns))))
		  (cond
		    (ln-p (values ln-p))
		    (errorp
		     (error "Local name ~s not registered in namespace ~s" ns))
		    (t (values nil ns)))))
	       (errorp
		(error "No namepace registered for prefix ~s in ~s ns-registry ~s"
		       pfx model nsreg))
	       (t (values nil nil)))))
	  (t  ;; element name was not qualified with a prefix
	   (error "???") ;; What to do, here?
	   ;; Option 1): Search for a "default namespace" in the
	   ;; registry, and "Use it" (even if that namespace has no
	   ;; URI assigned to it)
	   ;;
	   ;; Option 2): If there's a "containing element" context,
	   ;; use its namespace - whether that  namespace is assigned
	   ;; by namespace defaulting or by explicit qualified name
	   ;; declaration
	   )))

(defmethod shared-initialize ((instance bootstrap-metamodel-component)
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


(defclass metamodel-transform (bootstrap-metamodel-component)

  ;; this may be subject to some revision - note the irrelevance of
  ;; the 'type' slot and the corredponding @xmi:type attribute, with
  ;; regards to "simple" metamodel unmarshaling

  (
   (namespace ;; ??

    ;; XML namespace URI for the element in the metamodel
    ;; serialization, likewise may the URI of a UML package containing
    ;; the element
    :initarg namespace
    :type namespace
    :accessor transform-namespace)
   (element-p
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

   #+NIL ;; not needed in the metamodel
   (type ;; type of the model element object to be unmarshaled
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

(defclass metamodel-stub-class (bootstrap-metamodel-component standard-class)
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

#+Nil ;; FIXME: do this CHANGE-CLASS for all elements in the *boostrap-model* after loading
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
(defmethod (setf resolve-composite-name) ((new-value bootstrap-metamodel-component)
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
