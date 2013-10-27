#|

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#

(in-package #:cl-user)

(defpackage #:lupine/aux
  (:use #:cl)
  (:export
   ;; package.lisp
   #:package-designator
   #:package-shortest-name
   ;; type.lisp
   #:array-index
   #:class-designator
   #:compute-class
   #:type-designator
   #:member-typep
   #:simplify-string
   ;; conditions.lisp
   #:simple-style-warning
   ;; macro.lisp
   #:with-gensyms
   #:defconst
   ;; seq.lisp
   #:array-index
   #:array-dim
   #:do-vector
   #:split-string-1
   #:readtable-case-designator
   #:char-readtable-case
   #:dash-transform-camel-case
   )
  )

(in-package #:lupine/aux)

(deftype package-designator ()
  '(or string symbol package character))

(defun package-shortest-name (package)
  (declare (type package-designator package)
	   (values simple-string &optional))
  (let ((p (or (find-package package)
	       (error "Package not found: ~s" package))))
    (car (sort (cons (package-name p)
		     (copy-list (package-nicknames p)))
	       #'< :key #'length))))

#|

 (package-shortest-name '#:cl)
 =EXPECT=> "CL"

 (defpackage "http://foo.example.com"
     (:nicknames #:FOO))

 (package-shortest-name  "http://foo.example.com")

 =EXPECT=> "FOO"

|#
