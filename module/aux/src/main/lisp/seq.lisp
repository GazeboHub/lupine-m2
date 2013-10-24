#|

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#

(in-package #:lupine/aux)

(defun simplify-string (str)
  (declare (type string str)
	   (values (or simple-string simple-base-string)
		   &optional))
  (handler-case
      (coerce str 'simple-base-string)
    (type-error ()
      (coerce str 'simple-string))))


#|
 (type-of (simplify-string "FOO"))

 (type-of
 (simplify-string (make-array 31 :element-type 'character :adjustable t
		       :initial-element (code-char #x3C0))))
|#
