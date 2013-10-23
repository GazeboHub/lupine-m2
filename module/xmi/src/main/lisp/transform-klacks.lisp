#|                                                      -*- lisp -*-

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

(defun read-xmi (source &key sax-handler (model-class 'standard-model))
  (declare (type parser-input-source source)
	   (values model boolean &optional))
  ;; cf. `cxml-rng:parse-schema' for some klacks usage reference
  ;; also <http://common-lisp.net/project/cxml/klacks.html#sources>
  (let ((m (make-instance model-class
			  ;; FIXME: should this just
			  ;; 'MAKE-MODEL SOURCE &REST OTHERSTUFF' ?
			  :source (compute-uri source))))
    (klacks:with-open-source (s (make-xmi-source source sax-handler))
      (handler-case
	  (block klacks-parse
	    (loop
	      (multiple-value-bind (event-type &rest event-data)
		  (klacks:consume s)
		(ecase event-type
		  (null (return-from klacks-parse (values m nil)))
		  (:start-document
		   )
		  (:dtd
		   )
		  (:start-element
		   ;; call klacks:map-attributes, looking for xmi:type.
		   ;; one finding the xmi:type attribute,
		   ;; (let ((trans (find-type-transform type transform-model)))
		   )
		  (:end-element
		   )
		  (:characters
		   )
		  (:processing-instruction
		   )
		  (:comment
		   )
		  (:end-document
		   (return-from klacks-parse (values m t)))
		  ))))
	;; handle errors that the klacks parser might produce...
	))))
