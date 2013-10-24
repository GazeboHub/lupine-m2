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

#-:lupine-asdf ;; defpackage
(asdf:find-system '#:lupine-pmmt)

(in-package #:lupine/system)

(defsystem lupine-xmi
  ;; :class 'lupine-system/system
  ;; :project #:lupine
  ;; :defsystem-depends-on #:lupine-asdf
  :depends-on (#:cxml #:lupine-mop #:puri #:lupine-aux)
  :components
  ((:file "package")
   (:file "transform" :depends-on ("package")
   (:file "util-cxml" :depends-on ("package"))
   (:file "uri" :depends-on ("package"))
   (:file "model" :depends-on ("package" "uri"))
   (:file "transform-klacks" :depends-on ("package"
					  "transform"
					  "model"
					  "util-cxml"))
   ))
