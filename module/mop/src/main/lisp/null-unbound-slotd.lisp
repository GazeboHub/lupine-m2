
(in-package #:lupine/mop)

(defclass null-unbound-slot-definition ()
  ((null-unbound
    ;; FIXME: This slot is defined only to consume the :null-unbound
    ;; initialization argument at slot definition initialization time,
    ;; and is otherwise unused
    :initform t
    :initarg :null-unbound
    :allocation :class
    )))


(defmethod slot-value-using-class (class instance
				   (slotd null-unbound-slot-definition))
    (cond
      ((slot-boundp-using-class class instance slotd)
       (call-next-method))
      (t (values))))

(defclass effective-null-unbound-slot-definition
    (null-unbound-slot-definition standard-effective-slot-definition)
  ())

;; FIXME: When parsing the defclass form for a class that may be
;; defined with null-unbound slot definitions, parse each direct slot
;; definition for the :null-unbound initarg and its value. If the
;; :null-unbound initarg is present and has a non-null value, then
;; initialize the slot definition as a
;; direct-null-unbound-slot-definition. Else, "prefer next method"...

;; FIXME: When defining a subclass of a class whose deinition contains
;; a direct null-unbound slot definition, permit the direct and
;; effectivr slot definitions' class selection to be overridden, if
;; that slot's defition in the subclass is provided with a
;; ':null-unbound nil' initarg/value pair

(defclass direct-null-unbound-slot-definition
    (null-unbound-slot-definition standard-effective-slot-definition)
  ())

;; FIXME: Integrate this "null-unbound" slot definition protocol into
;; the class definitions in core-classes.lisp
;; ... defclass standard-class* ??
;; (refer to documentation, cf. AMOP)


#+NIL
(define-slot-definition-module  ;; ??? (this is a prototype form)
    (null-unbound-slot-definition)
  ((:direct-slotd-class  direct-null-unbound-slot-definition)
   (:effective-slotd-class effective-null-unbound-slot-definition)
   (:direct-slotd-test
    (lambda (&rest args &key (null-unbound nil nup) &allow-other-keys)
      (when nup (values t))))))


#+nil
(defgeneric module-active-p (module class &rest initargs
			     &key &allow-other-keys)
  (:method ((module slot-definition-module)
	    (class extended-standard-class)
	    &rest initargs)
    (apply (module-signature-test-function module) initargs)))
