#|

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#

(in-package #:lupine/xmi)


(defclass visibility-kind (?)
  (?))

(defclass named-element (element)
  ((name
    :attribute "name"
    :initarg :name
    :type string
    :accessor named-element-name
    ))
  (:metaclass uml-class)
  (:namespace
   ("uml" "http://www.omg.org/spec/UML/20110701" ))
  (:uml-name "UML::NamedElement")
  (:is-absract . t))


(defclass packageable-element (named-element)
  ()
  (:metaclass uml-class)
  (:namespace
   ("uml" "http://www.omg.org/spec/UML/20110701" ))
  (:uml-name "UML::PackageableElement")
  (:is-absract . t))


(defclass uml-package