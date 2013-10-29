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
  ;; FIXME: use :QNAME-OVERRIDE style namespace binding onto QNAME objects
  (namespace (error 'frob) :type (or simple-string null))
  (local-name (error 'frob) :type simple-ncname))


(defun qname-frob (qname)
  (declare (type qname qname)
	   (values simple-string))
  (let ((ns (qname-namespace qname))
	(n (qname-local-name qname)))
    (cond
      (ns (format nil "{~a}~a" ns n))
      (t (values n)))))

;; (qname-frob (make-qname "http://a.example.com/" "b"))


(defun qname= (q1 q2)
  (declare (type qname q1 q2)
	   (values boolean &optional))
  (let ((ns-1 (qname-namespace q1))
	(ns-2 (qname-namespace q2)))
    (and (or (when (and ns-1 ns-2)
	       (string= (the simple-string ns-1)
			(the simple-string ns-2)))
	     (and (null ns-1) (null ns-2)))
	 (string= (qname-local-name q1)
		  (qname-local-name q2)))))


(defun qname=* (ns lname q)
  (declare (type (or null simple-string) ns)
	   (type simple-string lname)
	   (type qname q)
	   (values boolean &optional))
  (let ((ns-q (qname-namespace q)))
    (and (or (when (and ns ns-q)
	       (string= (the simple-string ns)
			(the simple-string ns-q)))
	     (and (null ns) (null ns-q)))
	 (string= lname (qname-local-name q)))))

#|

;;; Test QNAME=*, QNAME=

 (let ((data '((t
	       ("http://foo.example.com/" . "FOO")
	       ("http://foo.example.com/" . "FOO"))
	      (t
	       (nil . "FOO")
	       (nil . "FOO"))
	      (nil
	       (nil . "FOO")
	       ("http://foo.example.com/" . "FOO"))
	      (nil
	       ("http://foo.example.com/" . "FOO")
	       (nil . "FOO"))
	      (nil
	       ("http://a.foo.example.com/" . "FOO")
	       ("http://b.foo.example.com/" . "FOO")))))

  (dolist (spec data t)
    (destructuring-bind (result (ns-1 . lname-1) (ns-2 . lname-2))
	spec
      (unless
	  (eq result
	      (qname= (make-qname ns-1 lname-1)
		      (make-qname ns-2 lname-2)))
	(error "QNAME= test failed, input ~s" spec))
      (unless
	  (eq result
	      (qname=* ns-1 lname-1
		       (make-qname ns-2 lname-2)))
	(error "QNAME=* test failed, input ~s" spec)))))

;; => T

|#


(defstruct (simple-container
	    (:constructor make-simple-container ()))
  (contents nil :type list))

(defmethod print-object ((object simple-container) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~d)" (length (simple-container-contents  object)))))

(defstruct (simple-node
	    (:include simple-container)
	    (:constructor make-simple-node (element &optional parent)))
  (element (error 'frob) :type qname)
  (parent nil :type (or simple-container null)))

(defmethod print-object ((object simple-node) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a (~d)"
	    (qname-frob (simple-node-element object))
	    (length (simple-container-contents object)))))

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defun scan-unique-element-types (source)
  ;; Prototype for a basic XML paser, using Klacks
  (declare (type parser-input-source source)
	   #+NIL (values list boolean &optional))
  (let ((event-handler (cxml:make-source source))
	(tree nil)
	(node nil))
    (klacks:with-open-source (s event-handler)
      (loop
	;; FIXME: &REST not allowed in MV-BIND
	;; see also: read-xmi
	(destructuring-bind  (event-type &rest event-data)
	    ;; NB: The "Variable number of return values" behavior
	    ;; used in KLACKS:CONSUME and similar functions may seem
	    ;; to essentially require some consing, to capture the
	    ;; variable number of return values after the first return
	    ;; value, for every Klacks event type
	    (multiple-value-list (klacks:consume s))

	  ;; FIXME: It's stuck in a loop, after end packagedElement
	  (ecase event-type
	    ((nil)
	     (return (values tree nil)))
	    (:start-document
	     (setq tree (make-simple-container)
		   node tree))
	    (:dtd)
	    (:start-element ;; uri, lname, qname
	     ;; NB: This functions scans element names,
	     ;;     does not map-attributes
	     (destructuring-bind (uri lname qname)
		 event-data
	       (declare (type (or null simple-string) uri)
			(type simple-string lname)
			(ignore qname))
	       (flet ((add-node ()
			(let ((n (make-simple-node (make-qname uri lname)
						   node)))
			  #+NIL (warn "START: ~S (~s)" n node)
			  (setf (simple-container-contents node)
				(push n (simple-container-contents node)))
			  (setq node n))))
		 (add-node))))
	    (:end-element
	     #-SKIP-NODE
	     (flet ((finalize-node ()
		      #+NIL (warn "END ~s" node)
		      (setf (simple-container-contents  node)
			    (nreverse (simple-container-contents node)))
		      (setq node (simple-node-parent node))))
	       #-SKIP-NODE
	       (finalize-node)))
	    (:characters)
	    (:processing-instruction)
	    (:comment)
	    (:end-document
	     (return (values tree nil))
	     )))))))


(defgeneric frob-tree (container)
  (:method ((container simple-container))
    (mapcar #'frob-tree (simple-container-contents container)))
  (:method ((container simple-node))
    (let ((name (qname-frob (simple-node-element container))))
      ;; (warn "FROB-TREE ~S" name)
      (cons name
	    (call-next-method)))))

#|

(trace simple-node-parent)

(defparameter *tree*
(scan-unique-element-types
 (merge-pathnames
  "../../../../../module/xmi/src/main/xmi/UML.xmi"))
)

;; note that PUSH results in the elements occurring in reverse order
;; in the result set, compared to their order in the input set.
;; (Thus, the corresponding NREVERSE call)
(simple-container-contents (car (simple-container-contents *tree*)))

;; FIXME: SCAN... is not nesting the result set correctly to the
;; nesting of the input set. It seems that many elements are being
;; skipped, entirely.

(princ (frob-tree *Tree*) t)
|#
