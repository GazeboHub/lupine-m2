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
