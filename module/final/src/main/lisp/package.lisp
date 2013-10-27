#|

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#


(in-package #:cl-user)

(defpackage #:lupine/final
  (:use #:lupine/aux #:cl)
  (:export
   #:finalize
   #:unfinalize
   #:instance-finalized-p

   #:instance-finalized
   #:instance-finalized-instance
   #:instance-finalized-error
   #:simple-instance-finalized-error
   #:assert-not-finalized

   #:finalizable-instance
   #:finalizable-object
   ))