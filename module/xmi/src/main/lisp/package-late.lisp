#|

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#


(in-package #:lupine/xmi)

(defpackage #:xml/encoding
  (:nicknames #:encoding)
  ;; package for interning of arbitrary XML encoding names as symbols
  ;; cf. ADD-CDATA, READ-XMI
  ;; See also: <http://www.w3.org/TR/REC-xml/#NT-EncodingDecl>
  (:use))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconst +xmi-2.4.1-namespace-uri+
    (simplify-string "http://www.omg.org/spec/XMI/20110701")
    "Namespace URI for XMI 2.4.1 serialized as XML

See also:
* <http://www.omg.org/spec/XMI/20110701/XMI.xsd>"
    )

  (defconst +xmi-model-element-name+
    (simplify-string "XMI")
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
   #.+xmi-model-element-name+
   #.(dash-transform-camel-case +xmi-model-element-name+)
   )
  )
