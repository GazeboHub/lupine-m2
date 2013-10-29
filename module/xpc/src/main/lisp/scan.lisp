#|

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#

(in-package #:lupine/ucx)

(define-condition frob (program-error) ;; temporary name in defn
  ())

(defstruct (qname
	    (:constructor make-qname (namespace local-name)))
  ;; FIXME: Integrate with the namespace resolver framework
  #+NIL (namespace (error 'frob) :type namespace)
  (namespace (error 'frob) :type simple-string)
  (local-name (error 'frob) :type simple-ncname))


(defun qname= (q1 q2)
  (declare (type qname q1 q2)
	   (values boolean &optional))
  (and (string= (qname-namespace q1)
		(qname-namespace q2))
       (string= (qname-local-name q1)
		(qname-local-name q2))))

(defun scan-element-types (source)
  (declare (type parser-input-source source)
	   (values list boolean &optional))
  (let ((event-handler (cxml:make-source source))
	tree
	node
	containing-node)
    (declare (type list tree node containing-node))
    (klacks:with-open-source (s event-handler)
      (loop
	;; FIXME: &REST not allowed in MV-BIND
	;; see also: read-xmi
	(multiple-value-bind (event-type &rest event-data)
	    (klacks:consume s)
	  (ecase event-type
	    ((nil)
	     (return (values tree nil)))
	    (:start-document
	     (setq node (make-list 1)
		   tree node))
	    (:dtd)
	    (:start-element ;; uri, lname, qname
	     ;; to do: map-attributes
	     (destructuring-bind (uri lname qname)
		 event-data
	       (declare (type simple-string uri lname)
			(ignore qname))
	       (unless (find-if (lambda (q)
				  (and (string= (qname-namespace q) uri)
				       (string= (qname-local-name q) lname))))

		 (let ((qn (make-qname  uri lname)))
		   (setf containing-node (push qn containing-node))
		   (setf node (list qn))))))
	    (:end-element
	     (setq node containing-node
		   containing-node nil))
	    (:characters)
	    (:processing-instruction)
	    (:comment)
	    (:end-document
	     (return (values tree t)))
	    ))))))

#|

(scan-element-types (merge-pathnames
		     "Dropbox/gc_sandbox/projects/GazeboHub/lupine-m2/module/xmi/src/main/xmi/UML.xmi"
		     (user-homedir-pathname)))

|#