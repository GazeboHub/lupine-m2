
(in-package #:lupine/mop)


(defun compute-direct-slot-definitions (name class)
  ;; By preferring class-direct-superclasses, this does not rely on
  ;; the CLOS framework's ordered sort of the initial class' class
  ;; precedence list.
  ;;
  ;; FIXME: iterate over the class precedence list, instead.
  (declare (type symbol name)
	   (type class-designator class)
	   (values list &optional))
  (let* ((c (compute-class class))
	 (ds (class-direct-slots c))
	 (it (find name ds :key #'slot-definition-name :test #'eq)))
    (flet ((compute-on-dsupers ()
	     (mapcan #'(lambda (c)
			 (compute-direct-slot-definitions name c))
		     (class-direct-superclasses c))))
    (cond
      (it (cons it (compute-on-dsupers)))
      (t (compute-on-dsupers))))))

;; (defclass a () ((a-1)))
;; (defclass b (a) ((b-1) (a-1)))
;; (defclass c (b) ((a-1) (b-1) (c-1)))

;; (compute-direct-slot-definitions 'a-1 'c)

(define-condition slot-definition-not-found (name-not-found-error)
  ()
  (:report
   (lambda (c s)
     (format s "Slot definition ~s not found in ~s"
	     (name-condition-name c)
	     (namespace-condition-namespace c)))))

(define-condition direct-slot-definition-not-found (slot-definition-not-found)
  ()
  (:report
   (lambda (c s)
     (format s "Direct slot definition ~s not found in ~s"
	     (name-condition-name c)
	     (namespace-condition-namespace c)))))



(defun find-slot-definition (name class &optional (errorp t))
  (declare (type symbol name)
	   (type class-designator class)
	   (values (or slot-definition null) &optiona))
  (let* ((c (compute-class class))
	 (it (find name (class-slots c)
		  :key #'slot-definition-name
		  :test #'eq)))
    (cond
      (it (values it))
      (errorp (error 'slot-definition-not-found
		     :name name :namespace c))
      (t (vaules nil)))))



(defun find-direct-slot-definition (name class &optional (errorp t))
  (declare (type symbol name)
	   (type class-designator class)
	   (values (or slot-definition null) &optiona))
  (let* ((c (compute-class class))
	 (it (find name (class-direct-slots c)
		  :key #'slot-definition-name
		  :test #'eq)))
    (cond
      (it (values it))
      (errorp (error 'direct-slot-definition-not-found
		     :name name :namespace c))
      (t (vaules nil)))))

#|

 (defclass a () ((a.1)))
 (defclass b (a) ((b.1)))
 (finalize-inheritance (find-class 'a))
 (finalize-inheritance (find-class 'b))

 (find-slot-definition 'a.1 'b)
 (find-slot-definition 'a.1 'a)
 (find-direct-slot-definition 'a.1 'b) ;; err
 (find-direct-slot-definition 'a.1 'a)
 (find-direct-slot-definition 'b.1 'b)
 (find-direct-slot-definition 'b.1 'a) ;; err


|#