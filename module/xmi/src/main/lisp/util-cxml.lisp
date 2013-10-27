#|

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#

(in-package #:lupine/xmi)

;;; * Utils for Parse-Schema

(deftype parser-input-source ()
  '(or pathname stream string runes:xstream))

;;; * Source->URI Computation


(defgeneric compute-source-uri (source)
  (:method ((source pathname))
    (let ((pbuff (make-string-output-stream))
	  (%source (merge-pathnames source)))
      (write-char #\/ pbuff)
      (dolist (p (cdr (pathname-directory %source)))
	(write-string p pbuff)
	(write-char #\/ pbuff))
      (let* ((n (pathname-name %source))
	     (tp (pathname-type %source)))
	(when n
	  (write-string n pbuff)
	  (when tp
	    (write-char #\. pbuff)
	    (write-string tp pbuff))))
      (make-instance 'puri:uri
		     :scheme :file
		     :host nil
		     :path  (get-output-stream-string pbuff))))
  (:method ((source file-stream))
    (compute-source-uri (pathname source)))
  #+TO-DO
  (:method ((source a-socket-stream-in-a-network-transport-app))
    )
  #+TO-DO
  (:method ((source runes:xstream))
    ))

#|
(compute-source-uri
 (make-pathname :directory '(:absolute "tmp" "foo")
		:name "foo"
		:type "md"))
|#