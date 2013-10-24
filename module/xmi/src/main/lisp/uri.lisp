#|

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#

(in-package #:lupine/xmi)

(defgeneric compute-uri (source)
  (:method ((source null))
    (values nil))
  (:method ((source pathname))
    (parse-uri
     (concatenate 'simple-string "file://" ;; ?
		  (namestring source))))
  (:method ((source file-stream))
    (compute-uri (pathname source)))
  (:method ((source string))
    (values nil))
  (:method ((source runes:xstream))
    (compute-uri (runes::xstream-os-stream source))))
