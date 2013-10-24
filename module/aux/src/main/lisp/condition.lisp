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