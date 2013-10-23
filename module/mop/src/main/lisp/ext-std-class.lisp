
(in-package #:lupine/mop)


(defclass extended-standard-class (standard-class)
  ())

(defmethod validate-superclass ((class extended-standard-class)
				(superclass standard-class))
  t)

(defmethod direct-slot-definition-class
  ((class extended-standard-class) &rest initargs)
  ;; this system could endeavor to use an initarg-centric sorting
  ;; method, in defining a sort of "slot definition module"
  ;; architecture
  ;;
  ;; If each slot definition extension "module" would be assigned a
  ;; single slot definition initarg or "initarg signature", such that
  ;; would effectively signify the class of either or both of the
  ;; direct and effective slot definitions, in the containing class,
  ;; for the containing slot definition, then each slot definition
  ;; "module" should be provided with a protocol for defining its
  ;; "initarg signature" and registering the "module" to a central
  ;; "module registry"
  (cond
    ((getf initargs :null-unbound)
     (find-class 'direct-null-unbound-slot-definition))
    (t (call-next-method))))

(defmethod effective-slot-definition-class
    ((class extended-standard-class) &rest initargs)
  (let* ((name (getf initargs :name))
	 (dslotds (compute-direct-slot-definitions name class)))
    (cond
      ((member-typep 'direct-null-unbound-slot-definition dslotds)
       ;; FIXME: "This gets the job done," but it may be an inefficient
       ;; approach for multiple dispatching, to use MEMBER-TYPEP as
       ;; such. Consider developing a MAP-DIRECT-SLOTS procedure,
       ;; instead (?)
       (find-class 'effective-null-unbound-slot-definition))
      (t (call-next-method)))))


#| Tests : Null-Unbound Slot Definition Framework

(defclass foo ()
  ((a)
   (b :null-unbound t))
  (:metaclass extended-standard-class))

(progn
  (finalize-inheritance (find-class 'foo))
  (member-typep 'effective-null-unbound-slot-definition
		(class-slots (find-class 'foo))))
;; => (not nil)

(let ((it (compute-direct-slot-definitions 'b 'foo)))
  (and (car it) (null (cdr it))
       (typep (car it) 'direct-null-unbound-slot-definition)))
;; => T


(defclass bar (foo)
  ((b))
  (:metaclass extended-standard-class))

(progn
  (finalize-inheritance (find-class 'bar))
  (member-typep 'effective-null-unbound-slot-definition
		(class-slots (find-class 'bar))))
;; => (not nil)

(let ((it (class-direct-slots (find-class 'bar))))
  (member-typep 'direct-null-unbound-slot-definition it))
;; => NIL, NIL


(defclass quux (bar)
  ()
  (:metaclass extended-standard-class))

(progn
  (finalize-inheritance (find-class 'bar))
  (member-typep 'effective-null-unbound-slot-definition
		(class-slots (find-class 'bar))))
;; => (not nil)

(let ((it (class-direct-slots (find-class 'bar))))
  (member-typep 'direct-null-unbound-slot-definition it))
;; => NIL, NIL


|#
