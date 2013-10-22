#|

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#


(in-package #:lupine/xd)

(defclass namespace-type (standard-class)
    ())

(defclass namespace ()
    ())

(defgeneric put-instance (name object namespace)
    ;; the generic xD protocol will not define an exact storage model
    #+:nil
    (:method (name (object namespace) (namespace namespace-type))
    
    ))

(defgeneric get-instance (name namespace)
    #+:nil
    (:method (name (namespace namespace-type))
    
    ))
