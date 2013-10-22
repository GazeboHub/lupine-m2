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

(defsystem lupine-xmi
	:class 'lupine-system/system
	:project #:lupine ;; FIXME: DEFPROJECT #:LUPINE; DEFSYSTEM LUPINE-PMP
	:defsystem-depends-on #:lupine-asdf
	:depends-on (#:cxml)
	:components
	((:file "package")
	)
