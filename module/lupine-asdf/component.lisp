#|                                                      -*- lisp -*-

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#

(in-package #:lupine/system)

(defclass component (asdf:component)
	(project
		:initarg :project
		:accessor component-project))

(defmethod component-project :around ((component component))
	(cond
 		((slot-boundp component 'project)
 		 (call-next-method))
 		(t
 		 (component-project (component-parent component))))
 