#|

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#

(in-package #:lupine/aux)

(deftype class-designator ()
  '(or symbol class))

(defun compute-class (c)
  (declare (type class-designator c)
	   (values class &optional))
  (etypecase c
    (symbol (find-class c))
    (class c)))

(deftype type-designator ()
  '(or class-designator (cons symbol t)))

(defun member-typep (type list)
  (declare
   (type type-designator type)
   (type list list)
   (values t (or unsigned-byte null) &optional))
  (do* ((restv list (cdr restv))
	(eltv (car restv) (car restv))
	(n 0 (incf n)))
       ((null restv) (values nil nil))
    (when (typep eltv type)
      (return (values eltv n)))))

;; (member-typep 'string '(1 2 "Three" #:4))
;; => "Three", 2
;; (member-typep 'string '(1 2 "Three"))
;; => "Three", 2
;; (member-typep 'string '("Three" 2 1))
;; => "Three", 0
;; (member-typep 'bignum '("Three" 2 1))
;; => NIL, NIL
