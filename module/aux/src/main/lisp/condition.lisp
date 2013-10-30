#|

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#

(in-package #:lupine/aux)

#-(or SBCL CMUCL)
(define-condition simple-style-warning (simple-condition
					style-warning)
  ())

(defmacro simple-style-warning (ctrl &rest args)
  `(warn
    #+SBCL 'sb-int:simple-style-warning
    #+CMU 'conditions:simple-style-warning
    :format-control ,ctrl
    :format-arguments (list ,@args)))


;; (simple-style-warning "FOO is not FOO : ~s" "foo")


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

(define-condition name-not-found-error (program-error name-not-found)
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
