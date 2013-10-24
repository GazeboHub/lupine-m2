#|

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#

(in-package #:lupine/xmi)

(defun make-xmi-source (source &key
				 sax-handler
				 (tmodel *xmi-unmarshalling-model*))
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

(defun make-standard-model (&optional source)
  (make-instance 'standard-model :source source))

(defun read-xmi (source &key sax-handler
			  (transform-model (make-standard-model source)))
  ;; FIXME: should this just
  ;; 'MAKE-MODEL SOURCE &REST OTHERSTUFF' ?
  (declare (type parser-input-source source)
	   (values model boolean &optional))
  ;; cf. `cxml-rng:parse-schema' for some klacks usage reference
  ;; also <http://common-lisp.net/project/cxml/klacks.html#sources>
  (let (*container*)
    (klacks:with-open-source (s (make-xmi-source source sax-handler))
      (handler-case
	  (block klacks-parse
	    (loop
	      (multiple-value-bind (event-type &rest event-data)
		  (klacks:consume s)
		(ecase event-type
		  ((nil)
		   ;; return model as created so far.
		   ;; second value indicates "did not complete document"
		   (return-from klacks-parse (values m nil)))
		  (:start-document
		   )
		  (:dtd
		   )
		  (:start-element
		   (destructuring-bind (namespace lname qname) event-data
		     (declare (ignore qname))
		     (setq *container* (allocate-instance
					;; FIXME: should this use namespace,
					;; qname in the element class query?
					;;
					;; FIXME: should this also
					;; use *container* ?
					(model-default-element-class transform-model)))

		     ;; dispatch on attributes
		     ;; cf. klacks:map-attributes

		     ;; @xmi:type
		     (let ((trans (find-type-transform
				   namespace lname type transform-model)))
		       ;; FIXME: should do more than "Change class," here?
		       (change-class *container* trans))

		     ;; @id = needs to be stored, for reference, in
		     ;; the result model (TO DO)

		     ;; availablity of other attributes, in a
		     ;; conforming model, will depend on {NAMESPACE, LNAME}
		     ;; e.g @name = may or may not be available

		     ;; 2. "dispatch" onto element contents
		  (:end-element
		   ;; "close" *container*
		   (initialize-instance *container*) ;; (?)
		   )
		  (:characters ;; contents for *container*
		   )
		  (:processing-instruction ;; use special PI handling
		   ;; (non-normative)
		   )
		  (:comment ;; use comment handling
		   ;; (non-normative)
		   )
		  (:end-document
		   ;; 1. finalize model

		   ;; (?)

		   ;; 2. then return - second value indicates
		   ;; "Completed document"
		   (return-from klacks-parse (values m t)))
		  ))))
	;; handle errors that the klacks parser might produce...
	))))
