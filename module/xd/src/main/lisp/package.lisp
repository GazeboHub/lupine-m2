#|

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#


(in-package #:cl-user)

(defpackage #:lupine/xd
    (:use #:c2mop #:cl)
    (:export
     #:namespace-type
     #:namespace
     #:put-instance
     #:get-instance
     ))
