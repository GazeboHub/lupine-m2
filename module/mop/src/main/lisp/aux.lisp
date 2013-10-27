
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
