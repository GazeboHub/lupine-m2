#|

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

;;; * Utils for Name Validation

;; This needs the cxml-ncname-patch.diff onto CXML
;; for (fboundp #'cxml::valid-ncname-p) => T

(deftype ncname ()
  ;; FIXME: Compatibility with Closure RUNES - needs to be further
  ;; integrated into this system, for supporting non-UTF lisp
  ;; implementations
  '(and runes:rod (satisfies cxml::valid-ncname-p)))

(deftype simple-ncname ()
  '(and (simple-array runes:rune) ncname))


;;; * Namespace Qualified Names (QNames)

;; FIXME: The followimg code, excepting the call to ncname-p, does
;; not depend exclusively on CXML. Move the code into a seperate
;; file.

;; This implementation endeavors to minimize the number of string
;; values created in the Lisp image, for persistent representstion of
;; QName (Namespace URI and NCName) values

(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant +qname-buffer-extent+ 32))

(defstruct (namespace
	    (:include finalizable-instance)
	    (:conc-name #:namespace-)
	    (:constructor %make-namespace (string uri)))
  (string #.(simplify-string "")
   :type simple-string)
  (uri (allocate-instance (find-class 'puri:uri))
   ;; FIXME: Use or discard the URI support
   :type puri:uri)
  (prefix-table (make-array #.+qname-buffer-extent+
			    :element-type 'simple-string
			    :adjustable t
			    :fill-pointer 0)
   :type (or (vector simple-string) simple-vector))
  (local-names-table (make-hash-table
		      :test #'equal
		      :size #.+qname-buffer-extent+)
   :type hash-table))

(defmethod print-object ((instance namespace) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (princ (namespace-prefix-table instance) stream)
    (write-char #\Space stream)
    (princ (namespace-string instance) stream)
    (write-char #\Space stream)
    (princ (instance-finalized-p instance) stream)))

(defmethod finalize ((instance namespace))
  ;; FIXME: call FINALIZE sometime after model initialization
  ;;
  ;; FIXME: how to make the instance's slot values read-only ?
  ;;        ^ (needs CLOS, SLOT-VALUE-USING-CLASS, etc

  ;; FIXME: also finalize LOCAL-NAMES-TABLE ? (implementation-specific
  ;; finalization procedures, specifically w.r.t hash-tables' internal
  ;; vectors, where previously adjustsble - needs a 'finalized-p' flag
  ;; on the hash-table, then)
  (setf (namespace-prefix-table instance)
	(coerce (namespace-prefix-table instance)
		'simple-vector)))

(declaim (type hash-table *qname-ns-registry*))

(defvar *qname-ns-registry* (puri:make-uri-space )) ;; ??

(defun make-namespace (ns-string)
  (declare (type string ns-string))
  (let ((s (simplify-string ns-string)))
    (%make-namespace
     ns-string
     (puri:intern-uri ns-string *qname-ns-registry*))))

(defun ensure-qname-string (ncname ns)
  (declare (type string ncname)
	   (type namespace ns)
	   (values simple-ncname &optional))
  (locally (declare (inline cxml::valid-ncname-p))
    (let ((ncname-s (simplify-string ncname))
	  (table (namespace-local-names-table ns)))
      (or (gethash ncname-s table)
	  (progn
	    (unless (cxml::valid-ncname-p ncname)
	      (error 'type-error :expected-type 'ncname :datum ncname))
	    (setf (gethash ncname-s table) ncname-s))))))

#|

 (let ((reg (make-namespace "http://foo.example.com/"))
      (q "FOO"))
  (eq (ensure-qname-string q reg)
      (ensure-qname-string q reg)))

;; =expect=> T


|#

(defun find-prefix (prefix namespace)
  (declare (type simple-string prefix)
	   (type namespace namespace)
	   (values (or null simple-string) &optional))
  (find prefix (namespace-prefix-table namespace)
	:test #'string=))

#+QNAMES
(defstruct (qname
	    (:constructor %make-qname (namespace name)))
  ;; FIXME: back-reference to a containing qname registry?
  (namespace "" :type simple-string)
  (name "" :type simple-string))

#+QNAMES
(defgeneric ensure-qname (name namespace)
  (:method ((name string) (namespace namespace))
    (values (%make-qname (namespace-string registry)
			 (ensure-qname-string qname registry))
	    namespace)))



;;; ** Meta Registry (Multiple Namespaces)

(defstruct (namespace-registry
	    (:include finalizable-instance)
	    (:constructor make-namespace-registry ())
	    (:conc-name #:namespace-registry-))
  ;; the namespace-table slot uses a vector for its table, in not
  ;; expecting a lot of variety/breadth in the table's contents
  (namespace-table (make-array #.+qname-buffer-extent+
			       :element-type 'namespace
			       :adjustable t
			       :fill-pointer 0)
   :type (or (vector namespace) simple-vector)))

(defmethod print-object ((instance namespace-registry) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (princ (length (namespace-registry-namespace-table instance))
	   stream)
    (write-char #\Space stream)
    (princ (instance-finalized-p instance) stream)))


(defmethod finalize ((instance namespace-registry))
  ;; FIXME: call FINALIZE sometime after model initialization
  ;;
  ;; FIXME: how to make the instance's slot values read-only ?
  ;;        ^ (needs CLOS, SLOT-VALUE-USING-CLASS, etc

  ; FIXME: Thread safety
  (let ((table (coerce (namespace-registry-namespace-table instance)
		       'simple-vector)))
    (setf (namespace-registry-namespace-table instance)
	  table)
    (do-vector (ns (the simple-vector table) instance)
      (finalize ns)
      )))

#+NIL
(defmethod unfinalize ((instance namespace-registry))
  (setf (namespace-registry-namespace-table instance)
	...))

#+QNAMES
(declaim (type namespace-registry *namespace-registry*))
#+QNAMES ;; used in ENSURE-QNAME (STRING STRING)
(defvar *namespace-registry*)

(defun compute-namespace (namespace registry &optional ensure-p)
;; FIXME: Propagate ENSURE-P behavior to other ensure=>compute functions in this file
  "Ensure that a namespace object exists for the string URI within the
specified NAMESPACE-REGISTRY. Returns the namespace object and a
second, boolean value indicating whether the namespace object was
created newly in this evaluation."
  (declare (type string namespace)
	   (type namespace-registry registry)
	   (values namespace boolean &optional))
  (let ((the-string (simplify-string namespace))
	(table (namespace-registry-namespace-table registry)))
    (declare (type simple-string the-string)
	     (type vector table))
    (flet ((find-registered ()
	     (do-vector (ns table)
	       (declare (type namespace ns))
	       (when (string= the-string (namespace-string ns))
		 (return (values ns nil))))))
      (or (find-registered)
	  (cond
	  	((and ensure-p (instance-finalized-p registry)
	     (error "Unable to create instance for namespace ~s in ~
finalized registry ~s"
		    namespace registry))
	    (ensure-p
	     (let ((reg (make-namespace the-string)))
	       (vector-push-extend reg table)
	       (values reg t))))
	     (t (error "Namespace ~s not found in registry ~s when :ENSURE-P NIL"
	     		namespace registry))
	       ))))


#+QNAMES
(defmethod ensure-qname ((name string) (namespace string))
  "Ensure that the NAME is registered to a namespace denoted by
STRING, within `*NAMESPACE-REGISTRY*'. Returns the simplified NAME
and its contsining NAMESPACE object"
  (let ((ns (compute-namespace namespace *namespace-registry*)))
    (values (ensure-qname name ns)
	    ns)))


(define-condition namespace-condition ()
  ((namespace
    :initarg :namespace
    :reader namespace-condition-namespace)))

(define-condition namespace-prefix-condition (namespace-condition)
  ((prefix
    :initarg :prefix
    :reader prefix-condition-prefix)))

(defmethod print-object ((instance namespace-prefix-condition) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (write-string (prefix-condition-prefix instance) stream)
    (write-char #\Space stream)
    (let ((ns (namespace-condition-namespace instance)))
      (typecase ns
	;; fixme: frobbed dispatching
	(namespace (write-string (namespace-string ns) stream))
	(t (princ ns stream))))))

(define-condition namespace-prefix-bind (namespace-prefix-condition)
  ()
  (:report
   (lambda (c s)
     (let ((ns (namespace-condition-namespace c)))
       (format s "prefix ~s bound to ~a"
	       (prefix-condition-prefix c)
	       (typecase ns
		 ;; fixme: frobbed dispatching
		 (namespace (namespace-string ns))
		 (t ns)))))))

(define-condition namespace-prefix-unbind (namespace-prefix-condition)
  ()
  (:report
   (lambda (c s)
     (let ((ns (namespace-condition-namespace c)))
       (format s "prefix ~s unbound from ~a"
	       (prefix-condition-prefix c)
	       (typecase ns
		 ;; fixme: frobbed dispatching
		 (namespace (namespace-string ns))
		 (t ns)))))))

(defun resolve-prefix-namespace (prefix registry &optional (errorp t))
  (declare (type string prefix)
	   (type namespace-registry regisry)
	   (values (or namespace null)
		   (or string null)
		   &optional))
  (let ((namespaces (namespace-registry-namespace-table registry))
	ns-p pf-p)
    (do-vector (ns namespaces (cond
				(errorp
				 (error "Prefix ~s not found in ~s"
					prefix registry))
				(t (values nil nil))))
      (declare (type namespace ns))
      (let ((p (find-prefix prefix ns)))
	(when p
	  (return (values ns p)))))))


(defun ensure-prefix (prefix uri registry)
  "Ensure that the namespace prefix PREFIX is registered uniquely to the
namespace URI in namespace-registry REGISTRY.

If an existing prefix/namespace binding is superseded by this bindimg,
a condition of type NAMESPACE-PREFIX-UNBIND is signaled.

If a new prefix/namespace binding is created - whether newly or in
superseding an existing binding - a condition of type
NAMESPACE-PREFIX-BIND is signaled.

The namespace object is returned as the first value. The simplified
prefix string is returned as the second value.

An error is signaled when a new binding must be created in either the
REGISTRY or the respective namespace, though the object would be
FINALIZED-P"
  (declare (type string prefix uri)
	   (type namespace-registry registry)
	   (values namespace simple-string &optional))
  (let ((pfx (simplify-string prefix))
	(urix (simplify-string uri)))
    (declare (type simple-string pfx urix))
    (block local-binding
      (block ns-prefix
	;; 0) check for prefix in registry (locating namespace
	;;    if available)
	;; 0.A) If so: Check for string equivalence to URI
	;; 0.A.A) If so, return
	;; 0.A.B) If not: remove prefix, emit signal PREFIX-UNBIND
	(multiple-value-bind (ns-p pf-p)
	    (resolve-prefix-namespace pfx registry nil)
	  (cond
	    ((and ns-p (string= (namespace-string ns-p) urix))
	     (return-from local-binding
	       (values ns-p pf-p)))
	    ((and ns-p (instance-finalized-p ns-p))
	     (error "Unable to add prefix ~s to ~
finalized namespace ~s"
		    pf-p ns-p))
	    (ns-p
	     ;; FIXME: Thread safety....?
	     (delete pf-p (namespace-prefix-table ns-p)
		     :test #'eq)
	     (restart-case
		 (signal 'namespace-prefix-unbind
			 :namespace ns-p
			 :prefix pf-p)
	       (continue)))))) ;; block ns-prefix
      ;; FIXME: try to wrap the following in a restart (?)
      ;; cf.  non-loal exit when the NAMESPACE-PREFIX-UNBIND signal is caught
      (block ns-uri
	;; 1) check for URI in Registry
	;; 1.A) if not, create namespace, initial prefix binding,
	;;      and add to registry, emit signal PREFIX-BIND (?)
	;; 1.B) if so, determine whether prefix exists for namespace
	;; 1.B.A) if so, return
	;; 1.B.B) if not, add prefix to namespace, emit signal
	;;        PREFIX-BIND
	(multiple-value-bind (ns new-p)
	    (compute-namespace uri registry)
	  (flet ((add-prefix ()
		   (cond
		     ((instance-finalized-p ns)
		      (error "Unable to add prefix ~s to ~
finalized namespace ~s"
			     pfx ns))
		     (t
		      ;; FIXME: Thread safety....?
		      (vector-push-extend pfx (namespace-prefix-table ns)
					  #.+qname-buffer-extent+)
		      (restart-case
			  (signal 'namespace-prefix-bind
				  :namespace ns
				  :prefix pfx)
			(continue))
		      (values ns pfx)))))
	    (cond
	      (new-p (add-prefix))
	      (t
	       (let ((pf-p (find-prefix pfx ns)))
		 (cond
		   ;; scan for prefix
		   (pf-p
		    (return-from local-binding
		      (values ns pf-p)))
		   (t (add-prefix))))
	       )))) ;; block ns-uri
	)  ;; block local-binding
      ))) ;; defun

#|

 (defparameter *r* (make-namespace-registry))

 (defparameter  *foo* (simplify-string "foo"))

 (defparameter  *foo.ex* (simplify-string "http://foo.example.com/"))

 (eq (compute-namespace *foo.ex* *r*)
     (compute-namespace *foo.ex* *r*))
 ;; => T


 (multiple-value-bind (ns pfx)
     (ensure-prefix *foo* *foo.ex* *r*)
   (values (eq pfx *foo*) ns )
   )
  ;; => T, #<structure-object ...>

;; (resolve-prefix-namespace "foo" *r*) ;; OK
;; (resolve-prefix-namespace "wtf" *r*) ;; expect error
;; (resolve-prefix-namespace "wtf" *r* nil) ;; expect NIL, NIL

;; (aref (namespace-registry-namespace-table *r*) 0)


 (ensure-prefix "bar" *foo.ex* *r*)
 ;; ^ call multiple times, should not be duplicating prefixes

 (handler-case
     (ensure-prefix "bar" "http://bar.example.com/" *r*)
   (namespace-prefix-unbind (c)
     (warn "Caught unbind signal: ~s" c)
     (continue c)))
 ;; expect, always - noting that the signal should happen only once
 ;;   (length (namespace-registry-namespace-table *r*))
 ;;    => 2

 (handler-case
     (ensure-prefix (string (gensym "NS-")) "http://bar.example.com/" *r*)
   (namespace-prefix-bind (c)
     (warn "Caught bind signal: ~s" c)
     (continue c)))


|#


(defun ensure-qname (cname registry)
  (declare (type string cname) (type namespace-registry registry)
	   (values simple-string simple-string &optional))
  (multiple-value-bind (prefix name)
      (split-string-1 #\: cname)
    (unless (cxml::valid-ncname-p prefix)
      (type-error :expected-type 'ncname :datum prefix))
    (unless (cxml::valid-ncname-p name)
      (type-error :expected-type 'ncname :datum name))
    ;; FIXME: define RESOLVE-PREFIX-NAMESPACE
    (let ((ns (resolve-prefix-namespace prefix registry)))
      (unless ns
	(error "Prefix ~s is not registered in ~s" prefix registry))
      (values (ensure-qname-string name ns)
	      ns))))