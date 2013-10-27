#|

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#


(in-package #:cl-user)

(defpackage #:lupine/xmi
  (:use #:cxml #:lupine/mop #:puri #:lupine/aux #:cl)
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
   #:+xmi-2.4.1-mamespace-uri+
   #:+xmi-model-element-name+
   #:read-xmi
   ))

(defpackage #:xml/encoding
  ;; package for interning of arbitrary XML encoding names as symbols
  ;; cf. ADD-CDATA, READ-XMI
  ;; See also: <http://www.w3.org/TR/REC-xml/#NT-EncodingDecl>
  (:use))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant lupine/xmi:+xmi-2.4.1-namespace-uri+
    (lupine/aux:simplify-string
     "http://www.omg.org/spec/XMI/20110701")
    "Namespace URI for XMI 2.4.1 serialized as XML

See also:
* <http://www.omg.org/spec/XMI/20110701/XMI.xsd>"
    )

  (defconstant lupine/xmi:+xmi-model-element-name+
    (lupine/aux:simplify-string "XMI")

    "Name of the typical root XMI model serialization element

See also:
* <http://www.omg.org/spec/XMI/2.4.1/>")
  ) ;; eval-when

(defpackage #.lupine/xmi:+xmi-2.4.1-namespace-uri+
  ;; FIXME: Allow for processing multiple XMI versions
  ;; cf. xmi:XMI in lupine/xmi:read-xmi
  (:use)
  (:nicknames #:ns/xmi #:ns/xmi/2.4.1)
  (:export
   #.lupine/xmi:+xmi-model-element-name+
   ;; FIXME: Regarding UML Names in Common Lisp
   ;;
   ;; Option 1) CamelCaseName -> Lisp-Name
   ;;
   ;;    Develop an algorithm for translation of UML
   ;;    element names to/from Lisp. That UML apparently uses
   ;;    "Same old camel-case", consider putting the code in
   ;;    lupine/aux
   ;;
   ;;    then, intern the translated 'type' attribute's name
   ;;    as a symbol, in this package
   ;;
   ;; Option 2) CamelCaseName -> |LispName|
   ;;
   ;;    This would use the UML name as a Lisp name, without
   ;;    further transformation
   ;;
   ;; Option 3)
   ;;  Support both Option.1 and Option.2, defaulting to Option.2
   )
  )
