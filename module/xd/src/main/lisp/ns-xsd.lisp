#|

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#


(in-package #:lupine/xd/xsd)

(defgeneric namespace-uri (namespace))

(defgeneric (setf namespace-uri) (uri namespace))

(defgeneric element-namespace (object))

(defgeneric (setf element-namespace) (namespace object))

(defgeneric prefix-namespace (prefix container))

(defgeneric (setf prefix-namespace) (namespace prefix container))

(defgenric namespace-prefixes (namespace container))

(defclass namespace-container ()
	;; TO DO: Implement the procedures of the previous,
	;; onto this genrric NAMESPACE-CONTAINER class,
	;; extending this class for:
	;;	* CXML elements
	;;  * RNG schema
	;;  * XSD schema
	;;  * MOF models
	;;  * OWL ontologies
	())


(defclass xml-namespace (namespace)
	;; Design Decision (to do)
	;; Namely, should an XML Namespace be represented with a unique 
	;; object type, or just be implicitly be representd with a URI?
	((ns-uri 
		:initarg :ns-uri
		:type string
		:initform +null-namespace+)
	(ns-prefixes
		:initarg ns-prefixes
		:type sequence ;; not good...
		)))
