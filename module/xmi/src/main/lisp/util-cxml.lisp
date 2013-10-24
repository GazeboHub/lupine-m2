#|                                                      -*- lisp -*-

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#

(in-package #:lupine/xmi)

;;; * Utils for Parse-Schema

(deftype parser-input-source ()
  '(or pathname stream string runes:xstream))

;;; * Namespace Qualified Names (QNames)

;; This implementation endeavors to minimize the number of string
;; vaules created in the Lisp image.

(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant +qname-buffer-extent+ 32))

(defstruct (qname-registry
	    (:conc-name #:registry-)
	    (:constructor %make-qname-registry (namespace namespace-uri)))
  (namespace #.(simplify-string "")
   :type simple-string)
  (namespace-uri (allocate-instance (find-class 'puri:uri))
   :type puri:uri)
  (local-names-table
   (make-hash-table :test #'equal :size #.+qname-buffer-extent+)
   :type hash-table))


(declaim (type hash-table *qname-ns-registry*))

(defvar *qname-ns-registry* (puri:make-uri-space ))

(defun make-qname-registry (ns-uri)
  (declare (type string ns-uri))
  (let ((s (simplify-string ns-uri)))
    (%make-qname-registry
     ns-uri
     (puri:intern-uri ns-uri *qname-ns-registry*))))

(defun ensure-qname-string (qname registry)
  (declare (type string qname)
	   (type qname-registry registry)
	   (values (or simple-string simple-base-string) &optional))
  (let ((qname-s (simplify-string qname))
	(table (registry-local-names-table registry)))
    (or (gethash qname-s table)
	(setf (gethash qname-s table) qname-s))))

(defstruct (qname
	    (:constructor %make-qname (namespace name)))
  ;; FIXME: back-reference to a containing qname registry?
  (namespace "" :type simple-string)
  (name "" :type simple-string))

(defgeneric make-qname (name namespace)
  #+NIL
  (:method ((name string) (namespace string))
    ;; FIXME: need a table of qname-registry instances
    )
  (:method ((name string) (namespace qname-registry))
    (%make-qname (registry-namespace registry)
		 (ensure-qname-string qname registry))))


#|

(let ((reg (make-qname-registry "http://foo.example.com/"))
      (q "FOO"))
  (eq (ensure-qname-string q reg)
      (ensure-qname-string q reg)))

;; =expect=> T


|#