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
(operate 'load-op #:lupine-asdf)

(in-package #:lupine/system)

(defsystem lupine-pmmt
	;; FIXME: This system will need the namespace support provided by lupine-xd, and will be used by lupine-dm, such that thismsytem definition would ultimately use. There is a concern with regards to circular dependencies. It needs a system definition bootstrap procedure, such as:
	;; *) define and load lupine-pmmt system
	;; *) define and load lupine project definition
	;; *) define and load lupine-dm
	;; *) index lupine-pmmt system as a module within lupine projct definition (cannot be done in lupine-pmmt system definition)
	:class 'lupine-system/system
	:project #:lupine ;; FIXME: DEFPROJECT #:LUPINE; DEFSYSTEM LUPINE-PMP
	:defsystem-depends-on #:lupine-asdf
	)
