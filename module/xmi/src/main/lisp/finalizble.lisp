#|

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#

;; Simple "Object Finalization" Protocol

;; example usage: in FINALIZE, convert adjustable vectors to simple
;; vectors

(in-package #:lupine/xmi)

;;; * Generic Functions

(defgeneric finalize (instance))


(defgeneric unfinalize (instance))


(defgeneric instance-finalized-p (instance))

(defgeneric (setf instance-finalized-p) (new-value instance))


;;; * Condition Protocol: INSTANCE-FINALIZED


(define-condition instance-finalized ()
  ((instance
    :initarg :instance
    :reader instance-finalized-instance))
  (:report
   (lambda (c s)
     (format s "Instance is finalized: ~s"
	     (instance-finalized-instance c)))))


(define-condition instance-finalized-error (error instance-finalized)
  ())

(define-condition simple-instance-finalized-error (simple-condition
						   instance-finalized-error)
  ()
  (:report
   (lambda (c s)
     (format s "Instance is finalized: ~s ~?"
	     (instance-finalized-instance c)
	     (simple-condition-format-control c)
	     (simple-condition-format-arguments c)))))


(defmacro assert-not-finalized (instance &optional format-control
				&rest format-args)
  (with-gensyms (%instance)
    `(let ((,%instance ,instance))
       (when (instance-finalized-p ,%instance)
	 ,@(cond
	     (format-control
	      `((error 'simple-instance-finalized-error
		       :instance ,%instance
		       :format-control ,format-control
		       :format-arguments (list ,@format-args))))
	     (t
	      `((error 'instance-finalized-error
		       :instance ,%instance))))))))


;;; * FINALIZABLE-INSTANCE


(defstruct (finalizable-instance
	    (:conc-name #:%instance-))
  (finalized-p nil
   :type boolean))

(defmethod instance-finalized-p ((instance finalizable-instance))
  (%instance-finalized-p instance))

(defmethod (setf instance-finalized-p) (new-value
					(instance finalizable-instance))
  (setf (%instance-finalized-p instance)
	(when new-value t)))


(defmethod finalize :around ((instance finalizable-instance))
  (unless (instance-finalized-p instance)
    (when (next-method-p)
      (call-next-method))
    (setf (instance-finalized-p instance) t)))

(defmethod unfinalize :around ((instance finalizable-instance))
  (when (instance-finalized-p instance)
    (when (next-method-p)
      (call-next-method))
    (setf (instance-finalized-p instance) nil)))


;;; * FINALIZABLE-OBJECT


(defclass finalizable-object ()
  ((finalized-p
    ;; :read-only t ;; cf. read-only standard slots
    :type boolean
    :initform nil
    :accessor instance-finalized-p
    )))

(defmethod finalize :around ((instance finalizable-object))
  (unless (instance-finalized-p instance)
    (when (next-method-p)
      (call-next-method))
    (setf (instance-finalized-p instance) t)))

(defmethod unfinalize :around ((instance finalizable-object))
  (when (instance-finalized-p instance)
    (when (next-method-p)
      (call-next-method))
    (setf (instance-finalized-p instance) nil)))
