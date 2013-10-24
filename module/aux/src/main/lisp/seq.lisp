#|

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#

(in-package #:lupine/aux)

(deftype array-dimension-index ()
  `(mod #.array-dimension-limit))

(deftype array-length ()
  `(integer 0 #.array-dimension-limit))


(defmacro do-vector ((var vector &optional retv) &body body)
  (with-gensyms (%vector %len %n)
    `(let* ((,%vector ,vector)
	    (,%len (length ,%vector))
	    (,%n 0))
       (declare (type vector ,%vector)
		(type array-length ,%len)
		(type array-dimension-index ,%n))
       (dotimes (,%n ,%len ,retv)
	 (let ((,var (aref ,%vector ,%n)))
	   ,@body)))))

;; (do-vector (c "FOO" 412) (format t "Char ~s~%" c))
