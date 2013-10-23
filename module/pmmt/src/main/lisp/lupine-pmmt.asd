#|                                                      -*- lisp -*-

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#

#-:asdf
(require #:asdf)

(in-package #:asdf-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defpackage #:lupine/system
  (:use #:asdf #:cl)
))

(in-package #:lupine/system)

(defsystem #:lupine-pmmt
  ;; :class 'lupine-system/system
  ;; :project #:lupine ;; FIXME: DEFPROJECT #:LUPINE
  :depends-on (#:puri #:lupine-aux)
  :components
  ((:file "package")
   (:file "project" :depends-on ("package"))
   ))
