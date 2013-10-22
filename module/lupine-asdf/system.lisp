#|                                                      -*- lisp -*-

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#

(in-package #:lupine/system)

(defgeneric component-uri (system))

(defclass system (asdf:system)
	((uri
		:initarg :uri
		:type uri
		:accessor component-uri)))
