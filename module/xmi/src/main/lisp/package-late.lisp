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

(defpackage #.lupine/xmi:+xmi-2.4.1-namespace-uri+
  ;; FIXME: Allow for processing multiple XMI versions
  ;; cf. xmi:XMI in lupine/xmi:read-xmi
  (:use)
  (:nicknames #:ns/xmi #:ns/xmi/2.4.1)
  (:export #:|XMI|))

(defpackage #.lupine/xmi:+uml-2.4.1-namespace-uri+
  ;; FIXME: Allow for processing multiple XMI versions
  ;; cf. xmi:XMI in lupine/xmi:read-xmi
  (:use)
  (:nicknames #:ns/uml #:ns/uml/2.4.1)
  (:export #:|Package|))


(defpackage #.lupine/xmi:+mofext-2.4.1-namespace-uri+
  ;; FIXME: Allow for processing multiple XMI versions
  ;; cf. xmi:XMI in lupine/xmi:read-xmi
  (:use)
  (:nicknames #:ns/mofext #:ns/mofext/2.4.1)
  (:export #:|Tag|))