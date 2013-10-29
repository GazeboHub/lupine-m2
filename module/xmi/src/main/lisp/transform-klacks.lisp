#|

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#

(in-package #:lupine/xmi)

(defun make-event-handler (source &key sax-handler)
  ;; FIXME: Modify this form producing a handler implementing the
  ;; QNAME-OVERRIDE behavior denoted in Namespaces.md
  "Generate an appropriate `KLACKS:SOURCE' handler for parsing SOURCE.

SAX-HANDLER, if non-nil, must be a CXML SAX handler

See also:
* `SAX:SAX-PROXY'
* `SAX:BROADCAST-HANDLER'
* `SAX:DEFAULT-HANDLER'
* `SAX:CONTENT-HANDLER'"
  (declare (type parser-input-source source)
	   (type (or null sax:abstract-handler) sax-handler)
	   (values klacks:source &optional))
  (if sax-handler
      (klacks:make-tapping-source source sax-handler)
      (cxml:make-source source)))

#+NIL ;; use *boostrap-model*
(defun make-standard-model (&optional source)
  (make-instance 'standard-model :source source))

(define-condition event-condition ()
  ((event :initarg :event :reader event-condition-event)
   (event-data :initarg :event-data :reader event-condition-event-data)))

(define-condition unsupported-event-type (event-condition warning)
  ()
  (:report (lambda (c s)
	     (format s "Unsupported event type: ~s - event data: ~s"
		     (event-condition-event c)
		     (event-condition-event-data c)))))

(defgeneric intern-qname-symbol (uri lname metamodel)
  ;; FIXME: This represents a point of integration between the XML and
  ;; Common Lisp namespaces, allowing for some peculiar instances, e.g
  ;; if URI is "CL". The side-effects of that may be negligible
  ;; insofar as the interned name is not used as a function or
  ;; variable name. However, it should be denoted in the
  ;; documentation, for developer interest.
  (:method ((uri simple-string) (lname simple-string)
	    (metamodel stub-metamodel))
    ;; FIXME: Later, audit this method for application not
    ;; only w.r.t STUB-METAMODEL (broaden)
    (let ((reg (model-namespace-registry metamodel))
	  ;; FIXME: Define MODEL-NAMESPACE-REGISTRY accessor
	  ;; and remove redefinitions of that feature
	  (ns (compute-namespace uri reg)))
      (cond
	(ns (let ((p (namespace-package ns)))
	      (values (intern lname (the package p))
		      p)))
	(*INTERN-IN-NULL-NAMESPACE*
	 ;; FIXME: Define this runtime feature - optionally as
	 ;; a METAMODEL feature deriving somehow from
	 ;; :QNAME-OVERRIDE

	 ;; FIXME: Define REGISTY-NULL-NAMESPACE accessor, and
	 ;; document this special handling of "null nameapace
	 ;; in metamodel", specifically with regards to
	 ;; :QNAME-OVERRIDE
	 (let ((p (registry-null-namespace reg)))
	   (values (intern lname (the package p))
		   p))))
      (t (error "cannot intern local name ~s - No namespace ~
for ~s found in metamodel-namespace-registry ~s of ~s and not ~
*INTERN-IN-NULL-NAMESPACE*"
		lname uri reg metsmodel))))))

(defun read-xmi (source &key
			  (event-handler (make-event-handler source))
			  (serialization-model *boostrap-model*))
  (declare (type parser-input-source source)
	   (type serialization-model serialization-model)
	   (values model boolean &optional))
  ;; cf. `cxml-rng:parse-schema' for klacks usage reference
  ;; also <http://common-lisp.net/project/cxml/klacks.html#sources>
  (let (model-element element-container
	model
	xml-version
	xml-document-encoding
	xml-standalone-p)
    (klacks:with-open-source (s event-handler)
      (handler-case
	  (block klacks-parse
	    (loop
	(destructuring-bind  (event-type &rest event-data)
	    ;; NB: The "Variable number of return values" behavior
	    ;; used in KLACKS:CONSUME and similar functions may seem
	    ;; to essentially require some consing, to capture the
	    ;; variable number of return values after the first return
	    ;; value, for every Klacks event type
	    (multiple-value-list (klacks:consume s))
	  (macrolet ((warn-unsupported ()
		       (warn 'unsupported-event-type
			     :event event-type
			     :event-data event-data)))
	    (ecase event-type
	      ((nil)
	       ;; return model as created so far.
	       ;; second value indicates "did not complete document"
	       ;; note that this does not call INITIALIZE-MODEL
	       (return-from klacks-parse (values model nil)))
	      (:start-document
	       ;; version, encoding, standalonep
	       (destructuring-bind (v enc s)
		   ;; FIXME: Parse string values for numeric biding
		   (setq xml-version v
			 xml-document-encoding
			 (intern enc '#:xml/encoding)
			 xml-standalone-p s)))
	      (:dtd ;; name, public-id, system-id
	       (warn-unsupported))
	      (:start-element ;; uri, lname, qname
		  	;;; 1) Detect root XMI element, intialize MODEL
		  	;;; 2) Else, ALLOCATE-ELEMENT and map ATTRS "here"
	       (destructuring-bind (uri lname qname) event-data
		 (declare (ignore qname) (type simple-string uri lname))
		 (let ((qname-symbol (intern-qname-symbol
				      uri lname serialization-model)))
		   (cond
		     ((eq qname-symbol (quote ns/xmi:XMI))
		      (setq model (allocate-model
				   xml-document-encoding
				   source serialization-model)))
		     (t
		      (setq
		       element-container model-element
		       model-element  (allocate-element
				       qname-symbol
				       uri lname model source
				       serialization-model)))))
		 (klacks:map-attributes
		  (lambda (uri lname qname val defaulted-p)
		    (declare (simple-string uri lname val)
			     (ignore qname defaulted-p))
		    (let ((qs (intern-qname-symbol
			       uri lname serializatino-model)))
		      (apply-atrribute value qs uri lname
				       model-element model
				       source
				       serialization-model)))
		  source)))
	      (:end-element ;; uri, lname, qname
	       ;; "close" model-element
	       (final-initialize model-element
				 element-container
				 model
				 serialization-model))
	      (:characters ;; data
	       ;; FIXME: Normalize string - optional or
	       ;; element-dependent - handle in ADD-CDATA
	       ;; methods
	       (add-cdata xml-document-encoding data
			  model-element model))
	      (:processing-instruction ;; pi-target, data
	       (warn-unsupported))
	      (:comment ;; data
	       (warn-unsupported))
	      (:end-document
	       ;; 1. "close" and shared-initialize model
	       (final-initialize model source serialization-model)
	       ;; 2. then return - second value indicates
	       ;; "Completed document"
	       (return-from klacks-parse (values m t)))
	      )))) ;; block klacks-parse
	    ;; handle errors that the klacks parser might produce...
	    )))))
