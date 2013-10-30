#|

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#


(in-package #:cl-user)

(defpackage #:lupine/xmi
  (:use #:lupine/ns  #:lupine/final
	#:cxml  #:lupine/mop #:puri
	#:lupine/aux #:cl)
  #+(or CMU SBCL)
  (:shadowing-import-from
   #+sbcl #:sb-pcl
   #+cmu #:pcl
   #:validate-superclass)
  (:shadowing-import-from
   #:c2mop
   #:defmethod
   #:defgeneric
   #:standard-generic-function)
  (:export
   #:+xmi-2.4.1-namespace-uri+
   #:+uml-2.4.1-namespace-uri+
   #:+mofext-2.4.1-namespace-uri+
   #:read-xmi
   ))

;; NB: See also package-late.lisp

;; for package-late.lisp

(in-package #:lupine/xmi)

(defconst +xmi-2.4.1-namespace-uri+
    (simplify-string "http://www.omg.org/spec/XMI/20110701")
    "Namespace URI for XMI 2.4.1

See also:
* <http://www.omg.org/spec/XMI/20110701/XMI.xsd>
* Package NS/XMI/2.4.1")

(defconst +uml-2.4.1-namespace-uri+
    (simplify-string "http://www.omg.org/spec/UML/20110701")
    "Namespace URI for UML 2.4.1 serialized as XMI

See also:
* Package NS/UML/2.4.1")

(defconst +mofext-2.4.1-namespace-uri+
    (simplify-string "http://www.omg.org/spec/MOF/20110701")
    "Namespace URI for 'mofext' XMI element namespace

See also:
* Package NS/MOFEXT/2.4.1")
