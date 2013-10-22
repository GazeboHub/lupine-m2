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

(eval-when #:compile-toplevel #:load=toplevel #:execute)
(defpackage #:lupine/system
	(:use #:asdf #:cl)
	(:shadow asdf:system asdf:component)
	(:export
		#:system
		#:component-uri
	)) ;;; eval-when

(in-package #:lupine/system)

(defsystem #:lupine-asdf
	:class 'asdf:system
	:components
	((:file "system")
	 (:file "component" :depends-on ("project"))
	 (:file "project")
	))
