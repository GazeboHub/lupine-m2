#|

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#

(in-package #:lupine/ns)

;;; * Utils for Name Validation

;; This needs the cxml-ncname-patch.diff onto CXML
;; for (fboundp #'cxml::valid-ncname-p) => T

(deftype ncname ()
  ;; FIXME: Compatibility with Closure RUNES - needs to be further
  ;; integrated into this system, for supporting non-UTF lisp
  ;; implementations
  '(and (or runes::rod string)
    (satisfies cxml::valid-ncname-p)))

(deftype simple-ncname ()
  #+NIL
  '(and (simple-array runes:rune (*)) ncname)
  '(and (or runes::simple-rod simple-string)
    ncname))


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
	    (:constructor nil)
	    (:conc-name #:%namespace-))
  (local-names-table (make-hash-table
		      :test #'equal
		      :size #.+qname-buffer-extent+)
   :type hash-table))


(defstruct (simple-namespace
	    (:include namespace)
	    (:constructor make-simple-namespace (package)))
  (string #.(simplify-string "")
   :type simple-string)
  #+NAMESPACE-URI ;; FIXME: Use or discard the URI support
  (uri (allocate-instance (find-class 'puri:uri))
   :type puri:uri)

  ;; FIXME: PACKAGE essentially duplicates LOCAL-NAMES-TABLE.
  ;; Consider replacing local-names-table search with FIND-SYMBOL calls
  (package (copy-structure (defpackage "SIMPLE-NAMESPACE-CLONE"))
   :type package
   ))


(defstruct (prefix-qualified-namespace
	    (:include simple-namespace)
	    (:constructor
		%make-namespace
		(string #+NAMESPACE-URI uri
			&optional
			  (package (find-package* string)))))
  (prefix-table (make-array #.+qname-buffer-extent+
			    :element-type 'simple-string
			    :adjustable t
			    :fill-pointer 0)
   :type (or (vector simple-string) simple-vector)))


;;; * Accessor Protocol

(defgeneric namespace-string (namespace)
  (:method ((namespace simple-namespace))
    (simple-namespace-string namespace)))

#+NAMESPACE-URI
(defgeneric namespace-uri (namespace)
  (:method ((namespace simple-namespace))
    (simple-namespace-uri namespace)))

(defgeneric namespace-package (namespace)
  (:method ((namespace simple-namespace))
    (simple-namespace-package namespace)))

(defgeneric namespace-local-names-table (namespace)
  (:method ((namespace namespace))
    (%namespace-local-names-table namespace)))


(defgeneric namespace-prefix-table (namespace)
  (:method ((namespace prefix-qualified-namespace))
    (prefix-qualified-namespace-prefix-table namespace)))



(defgeneric (setf namespace-string) (new-value namespace)
  (:method ((new-value string) (namespace simple-namespace))
    (setf (simple-namespace-string namespace)
	  new-value)))

#+NAMESPACE-URI
(defgeneric (setf namespace-uri) (new-value namespace)
  (:method ((new-value uri) (namespace simple-namespace))
    (setf (simple-namespace-uri namespace)
	  new-value)))

(defgeneric (setf namespace-package) (new-value namespace)
  (:method ((new-value package) (namespace simple-namespace))
    (setf (simple-namespace-package namespace)
	  new-value)))

(defgeneric (setf namespace-local-names-table) (new-value namespace)
  (:method ((new-value hash-table) (namespace namespace))
    (setf (%namespace-local-names-table namespace)
	  new-value)))


(defgeneric (setf namespace-prefix-table) (new-value namespace)
  (:method ((new-value vector)
	    (namespace prefix-qualified-namespace))
    (setf (prefix-qualified-namespace-prefix-table namespace)
	  new-value)))



#|
 (let ((ns (simplify-string "CL")))
  (defparameter *ns*
    (%make-namespace ns (puri:intern-uri ns))))

 (eq (namespace-package *ns*) (find-package "COMMON-LISP"))
 =EXPECT=> T
|#


(defmethod print-object ((instance prefix-qualified-namespace) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~A ~A ~A ~A"
	    (namespace-prefix-table instance)
	    (namespace-string instance)
	    (instance-finalized-p instance)
	    (package-shortest-name (namespace-package instance)))))

(defmethod finalize ((instance prefix-qualified-namespace))
  ;; FIXME: call FINALIZE sometime after model initialization
  ;; FIXME: Thread safety
  (setf (namespace-prefix-table instance)
	(coerce (namespace-prefix-table instance)
		'simple-vector)))

(defmethod unfinalize ((instance prefix-qualified-namespace))
  ;; Assumption: INSTANCE is finalized
  ;; FIXME: Regression testing
  ;; FIXME: Thread safety
  (let* ((table (namespace-prefix-table instance))
	 (len (length (the simple-vector table))))
    (setf (namespace-prefix-table instance)
	  (make-array len
		      :element-type 'simple-string
		      :fill-pointer len
		      :adjustable t
		      :initial-contents table))))


(declaim (type hash-table *qname-ns-registry*))

(defvar *qname-ns-registry* (puri:make-uri-space )) ;; ??

(defun make-namespace (ns-string
		       &optional
			 (package (find-package* ns-string)))
  (declare (type string ns-string)
	   (type package package)
	   ;; FIXME: Always return a prefix-qualified-namespace ?
	   (values prefix-qualified-namespace &optional))
  (let ((s (simplify-string ns-string)))
    (%make-namespace s
		     #+NAMESPACE-URI
		     (puri:intern-uri s *qname-ns-registry*)
		     package)))

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
	    (setf (gethash ncname-s table) ncname-s)
	    )))))

#|

 (let ((reg (make-namespace "http://foo.example.com/"))
      (q "FOO"))
  (eq (ensure-qname-string q reg)
      (ensure-qname-string q reg)))

;; =expect=> T


|#

(defun find-prefix (prefix namespace)
  ;; FIXME: ADD ERRORP
  (declare (type simple-string prefix)
	   (type prefix-qualified-namespace namespace)
	   (values (or null simple-string) &optional))
  (find prefix (prefix-qualified-namespace-prefix-table namespace)
	:test #'string=))


;;; ** Meta Registry (Multiple Namespaces)

;; FIXME: When moving this code into a new Lupine/NS module,
;; also move the finalizable instance proocol into its own module,
;; namely Lupine/Final




(defstruct (namespace-registry
	    (:include finalizable-instance)
	    (:constructor make-namespace-registry
		(&optional null-namespace))
	    (:conc-name #:namespace-registry-))
  ;; the namespace-table slot uses a vector for its table, in not
  ;; expecting a lot of variety/breadth in the table's contents
  (namespace-table (make-array #.+qname-buffer-extent+
			       :element-type 'prefix-qualified-namespace
			       :adjustable t
			       :fill-pointer 0)
   :type (or (vector namespace) simple-vector))
  (null-namespace
   ;; FIXME: There may be some ambiguous semantics with regards to a
   ;; "Null namespace" within a namespace-registry decoupled from an
   ;; XML document object model - to a question, what would be the
   ;; namespace URI of the null namespace within a namespace registry?
   (make-simple-namespace
    (copy-structure (defpackage "NULL-NAMESPACE-CLONE")))
   :type simple-namespace))

;; (namespace-registry-null-namespace (make-namespace-registry))

(defmethod print-object ((instance namespace-registry) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~D ~A"
	    (length (namespace-registry-namespace-table instance))
	    (instance-finalized-p instance))))


(defmethod finalize ((instance namespace-registry))
  ;; FIXME: call FINALIZE sometime after model initialization
  ;; FIXME: Thread safety
  (let ((table (coerce (namespace-registry-namespace-table instance)
		       'simple-vector)))
    (setf (namespace-registry-namespace-table instance)
	  table)
    (finalize (namespace-registry-null-namespace instance))
    (do-vector (ns (the simple-vector table) instance)
      (finalize ns))))


(defmethod unfinalize ((instance namespace-registry))
  ;; Assumption: INSTANCE is finalized
  ;; FIXME: Thread Safety
  ;; FIXME: Regression testing
  (let* ((table (namespace-registry-namespace-table instance))
	 (len (length (the simple-vector table))))
    (setf (namespace-registry-namespace-table instance)
	  (make-array len
		      :element-type 'namespace
		      :fill-pointer len
		      :adjustable t
		      :initial-contents table))
    (unfinalize (namespace-registry-null-namespace instance))
    (do-vector (ns table instance)
      (unfinalize ns))))


;;; * Condition Type NAMESPACE-CONDITION


(define-condition namespace-condition ()
  ((namespace
    :initarg :namespace
    :reader namespace-condition-namespace)))

;;; * Condition Type Protocol: NAME-NOT-FOUND

(define-condition name-not-found (name-condition namespace-condition)
  ()
  (:report
   (lambda (c s)
     (format s "Name ~s not found in ~s"
	     (name-condition-name c)
	     (namespace-condition-namespace c)))))

(define-condition name-not-found-error (error name-not-found)
  ())

(define-condition simple-name-not-found-error (simple-condition
					       name-not-found-error)
  ()
  (:report
   (lambda (c s)
     (format s "Name ~s not found in ~s ~?"
	     (name-condition-name c)
	     (namespace-condition-namespace c)
	     (simple-condition-format-control c)
	     (simple-condition-format-arguments  c)))))


;;; * Condition Type Protocol: NAMESPACE-NOT-FOUND

(define-condition namespace-not-found (name-not-found)
  ()
  (:report
   (lambda (c s)
     (format s "Namespace ~s not found in ~s"
	     (name-condition-name c)
	     (namespace-condition-namespace c)))))


(define-condition namespace-not-found-error (error
					     namespace-not-found)
  ())


;;;  * Namespace Resolvers


(defun compute-namespace (uri registry &optional ensure-p)
  ;; FIXME: Propagate ENSURE-P behavior to other ensure=>compute
  ;; functions in this file [DO NEXT]

  "Ensure that a namespace object exists for the string URI within the
specified NAMESPACE-REGISTRY. Returns the namespace object and a
boolean value indicating whether the namespace object was created
newly in this evaluation."
  (declare (type string uri)
	   (type namespace-registry registry)
	   (values simple-namespace boolean &optional))
  (let ((%uri (simplify-string uri))
	(table (namespace-registry-namespace-table registry)))
    (declare (type simple-string %uri)
	     (type vector table))
    (flet ((find-registered ()
	     (do-vector (ns table)
	       (declare (type namespace ns))
	       (when (string= %uri (namespace-string ns))
		 (return ns)))))
      (let ((ns (find-registered)))
	(cond
	  (ns (values ns nil))
	  (ensure-p
	   (assert-not-finalized registry
				 "Unable to add instance for uri ~s"
				 %uri)
	   (let ((ns-new (make-namespace %uri)))
	     (vector-push-extend ns-new table)
	     (values ns-new t)))
	  (t (error 'namespace-not-found-error
		    :name %uri
		    :namespace registry)))))))

;;; * Condition Type Protocol: Namespace Binding/Unbinding

(define-condition namespace-prefix-condition (name-condition
					      namespace-condition)
  ())

(defmethod print-object ((instance namespace-prefix-condition) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~A ~A"
	    (name-condition-name instance)
	    (namespace-condition-namespace instance))))

(define-condition namespace-prefix-bind (namespace-prefix-condition)
  ()
  (:report
   (lambda (c s)
       (format s "prefix ~s bound to ~a"
	       (name-condition-name c)
	       (namespace-condition-namespace c)))))


(define-condition namespace-prefix-unbind (namespace-prefix-condition)
  ()
  (:report
   (lambda (c s)
     (format s "prefix ~s unbound from ~a"
	     (name-condition-name c)
	     (namespace-condition-namespace c)))))


;;; * Condition Type Protocol: PREFIX-ALREADY-BOUND

(define-condition prefix-already-bound (name-condition
					namespace-condition)
  ()
  (:report
   (lambda (c s)
     (format s "Prefix ~s is already bound to ~s"
	     (name-condition-name c)
	     (namespace-condition-namespace c)))))

(define-condition prefix-already-bound-error (error
					      prefix-already-bound)
  ())

;;; * Condition Type Protocol: PREFIX-NOT-FOUND

(define-condition prefix-not-found (name-not-found)
  ()
  (:report
   (lambda (c s)
     (format s "Prefix ~s not found in ~s"
	     (name-condition-name c)
	     (namespace-condition-namespace c)))))

(define-condition prefix-not-found-error (error prefix-not-found)
  ())


;;; * Namespace Prefix Resolvers

(defun resolve-prefix-namespace (prefix registry &optional (errorp t))
  "Compute and return the namespace for PREFIX within REGISTRY,
if PREFIX is bound to any namespace within REGISTRY.

When PREFIX is found, the namespace is returned as the first
value. The exact, simplified prefix string object for PREFIX within
that namesapce is returned as the second value.

## Exceptional Situations

When PREFIX is not found for any namespace within REGISTRY and ERRORP
is \"True\", an error of type PREFIX-NOT-FOUND-ERROR is signaled"
  (declare (type string prefix)
	   (type namespace-registry registry)
	   (values (or prefix-qualified-namespace null)
		   (or simple-string null)
		   &optional))
  (let ((namespaces (namespace-registry-namespace-table registry)))
    (do-vector (ns namespaces (cond
				(errorp
				 (error 'prefix-not-found-error
					:name  prefix
					:namespace registry))
				(t (values nil nil))))
      (declare (type namespace ns))
      (let ((p (find-prefix prefix ns)))
	(when p
	  (return (values ns p)))))))


(defun bind-prefix (prefix uri registry &optional move-p)
"Ensure that the namespace prefix PREFIX is registered uniquely to
the namespace URI in namespace-registry REGISTRY.

The namespace object is returned as the first value. The exact,
simplified prefix string object for PREFIX is returned as the second
value.

## Signals

If an existing prefix/namespace binding is superseded by this binding,
a condition of type NAMESPACE-PREFIX-UNBIND is signaled.

If a new prefix/namespace binding is created - whether newly or in
superseding an existing binding - a condition of type
NAMESPACE-PREFIX-BIND is signaled.

## Exceptional Situations

An error of type PREFIX-ALREADY-BOUND-ERROR is signaled when MOVE-P
is \"False\" and PREFIX denotes a prefix bound to a namespace not
namespace-equivalent (that is, string-equivalent) to URI

An error of type SIMPLE-INSTANCE-FINALIZED-ERROR is signaled when a
new binding must be created in either the REGISTRY or the namespace,
though the respective object would be FINALIZED-P"
  (declare (type string prefix uri)
	   (type namespace-registry registry)
	   (values prefix-qualified-namespace
		   simple-string &optional))
  (let ((pfx (simplify-string prefix))
	(urix (simplify-string uri)))
    (declare (type simple-string pfx urix))
    (block local-binding
      (labels ((bind-ns-uri ()
		 ;; 1) check for URI in Registry
		 ;; 1.A) if not, create namespace, initial prefix binding,
		 ;;      and add to registry, emit signal PREFIX-BIND (?)
		 ;; 1.B) if so, determine whether prefix exists for namespace
		 ;; 1.B.A) if so, return
		 ;; 1.B.B) if not, add prefix to namespace, emit signal
		 ;;        PREFIX-BIND
		 (multiple-value-bind (ns ns-new-p)
		     (compute-namespace uri registry t)
		 (flet ((add-prefix ()
			  (assert-not-finalized ns
						"Unable to add prefix ~s"
						pfx)
			  ;; FIXME: Thread safety onto NS - lock NS
			  (vector-push-extend pfx (namespace-prefix-table ns)
					      #.+qname-buffer-extent+)
			  (restart-case
			      (signal 'namespace-prefix-bind
				      :namespace ns
				      :name pfx)
			    (continue ()
			      (bind-ns-uri)))

			  (values ns pfx)))
		   (cond
		     (ns-new-p (add-prefix))
		     (t
		      (let ((pf-p (find-prefix pfx ns)))
			(cond
			  ;; scan for prefix
			  (pf-p (return-from local-binding
				  (values ns pf-p)))
			  (t (add-prefix)))))))))
	       (bind-ns-prefix ()
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
		     ((and ns-p move-p)
		      (assert-not-finalized ns-p
					    "Unable to add prefix ~s"
					    pf-p)

		      ;; FIXME: Thread safety onto NS-P - lock NS-P
		      (setf (namespace-prefix-table ns-p)
			    (delete pf-p (namespace-prefix-table ns-p)
				    :test #'eq))

		      ;; FIXME: when the unbind signal is caught in
		      ;; a containing handler-case calling this function
		      ;; within its body, then even after 'continue' in that
		      ;; handler-case, the later binding is not made
		      (restart-case
			  (signal 'namespace-prefix-unbind
				    :name pf-p
				    :namespace ns-p)
			(continue ()
			  ;; How to ensure that BIND-NS-URI is called
			  ;; after control flow returns (?) to this function?
			  (bind-ns-uri))))
		     (ns-p
		      (error 'prefix-already-bound-error
			     :name pf-p
			     :namespace ns-p))
		     ))))
	(bind-ns-prefix)
	(bind-ns-uri))
    ))) ;; defun

;; FIXME: For XML namespace support, defun ADD-DEFAULT-XML-NAMESPACES
;; cf. "xml" and "xmlns" prefixes' standard namespace bindings

#|

  (progn ;; test harness setup
     (defparameter *r* (make-namespace-registry))

     (defparameter  *foo* (simplify-string "foo"))

     (defparameter  *foo.ex* (simplify-string "http://foo.example.com/"))

     (defpackage #.*foo.ex* (:use) (:nicknames "NS/FOO"))
  )

 (compute-namespace *foo.ex* *r* t)
;; => #<...>, t ; first call
;; => #<...>, nil ; later calls

 (eq (compute-namespace *foo.ex* *r*)
     (compute-namespace *foo.ex* *r*)
     )
 ;; => T


 (multiple-value-bind (ns pfx)
     (bind-prefix *foo* *foo.ex* *r* t)
   (values (eq pfx *foo*) ns )
   )
  ;; => T, #<structure-object ...>

;; (resolve-prefix-namespace "foo" *r*) ;; OK
;; (resolve-prefix-namespace "nope" *r*) ;; expect error
;; (resolve-prefix-namespace "2xNull" *r* nil) ;; expect NIL, NIL

;; (namespace-registry-namespace-table *r*)
;; ^ informative

  (= (length (namespace-registry-namespace-table *r*)) 1)
 ;; => T


 (bind-prefix "bar" *foo.ex* *r* t)
;; ^ when called multiple times, should not be duplicating prefixes




;; an example of appropriate handling for namespace-perfix-unbind,
;; such that the handler results in completion of the later binding
;; behavior:
 (handler-bind ((NAMESPACE-PREFIX-UNBIND
		       #'(lambda (c)
			     (declare (ignore C))
			     (invoke-restart 'CONTINUE))))
   (progn
     (defpackage "http://bar.example.com/")
     (defparameter *r* (make-namespace-registry))
     (bind-prefix *foo* *foo.ex* *r* t)
     (bind-prefix "bar" *foo.ex* *r* t)
     (bind-prefix "bar" "http://bar.example.com/" *r* t)
     (length (namespace-registry-namespace-table *r*))
     ))
 ;; => 2


;;; "last test"

 (handler-case
     (bind-prefix (string (gensym "NS-")) "http://bar.example.com/" *r*)
   (namespace-prefix-bind (c)
     (warn "Caught bind signal: ~s" c)
     (continue c)))
;; ^ always adds a new prefix. cf:
 (namespace-registry-namespace-table *r*)

|#

;;; * QName Resolvers

(defun ensure-qname (cname registry)
  (declare (type string cname)
	   (type namespace-registry registry)
	   (values simple-string simple-namespace &optional))
  (multiple-value-bind (prefix name)
      (split-string-1 #\: cname)

    (when prefix
      (unless (cxml::valid-ncname-p prefix)
	(error 'type-error :expected-type 'ncname :datum prefix)))

    (unless (cxml::valid-ncname-p name)
      (error 'type-error :expected-type 'ncname :datum name))

    (let ((ns (cond
		(prefix (resolve-prefix-namespace prefix registry))
		(t (namespace-registry-null-namespace registry)))))
      (unless ns
	(error 'prefix-not-found-error
	       :name prefix
	       :namespace registry))
      (values (ensure-qname-string name ns)
	      ns))))

;; (ensure-qname "FOO" (make-namespace-registry ))

(defun ensure-qname-symbol (cname registry)
    (declare (type string cname)
	     (type namespace-registry registry)
	     (values symbol simple-namespace &optional))
  (multiple-value-bind (ncname ns)
      (ensure-qname cname registry)
    (values (intern ncname (namespace-package ns))
	    ns)))
#|

 (progn ;; test harness setup
   (defparameter *r* (make-namespace-registry))

   (defparameter  *foo* (simplify-string "foo"))

   (defparameter  *foo.ex* (simplify-string "http://foo.example.com/"))

   (defpackage #.*foo.ex* (:use) (:nicknames "NS/FOO"))

   (defparameter *ns* (bind-prefix *foo* *foo.ex* *r*))
 )

 (multiple-value-bind (s ns)
   (ensure-qname-symbol "foo:FOO" *r*)
   (values (eq ns *ns*) s))
 ;; =EXPECT=> T, |http://foo.example.com/|::FOO

|#