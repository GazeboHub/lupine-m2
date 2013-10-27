#|

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#

(in-package #:lupine/aux)


(deftype array-index ()
  '(mod #.array-dimension-limit))

(deftype array-dim ()
  `(integer 0 #.array-dimension-limit))


(defmacro do-vector ((var vector &optional retv) &body body)
  (with-gensyms (%vector %len %n)
    `(let* ((,%vector ,vector)
	    (,%len (length ,%vector))
	    (,%n 0))
       (declare (type vector ,%vector)
		(type array-dim ,%len)
		(type array-index ,%n))
       (dotimes (,%n ,%len ,retv)
	 (let ((,var (aref ,%vector ,%n)))
	   ,@body)))))

;; (do-vector (c "FOO" 412) (format t "Char ~s~%" c))


(defun split-string-1 (c str)
  (declare (type character c)
	   (type string str)
	   (values (or string null) string &optional))
  (let ((n (position c str :test #'char=)))
    (declare (type (or array-index null) n))
    (cond
      (n (values (subseq str 0 n)
		 (subseq str (1+ (the array-index n)))))
      (t (values nil str)))))

;; (split-string-1 #\: "FOO:BAR")

;; (split-string-1 #\: "FOOBAR")

(deftype readtable-case-designator ()
  '(member :upcase :downcase :preserve :invert))

(defun char-readtable-case (c
			    &optional
			      (char-case
			       (readtable-case *readtable*)))
  (declare (type character c)
	   (type readtable-case-designator char-case)
	   (values character &optional))
  (ecase char-case
    (:upcase (char-upcase c))
    (:downcase (char-downcase c))
    (:preserve (values c))
    (:invert ;; complex condition
     ;; frob
     (cond
       ((upper-case-p c)
	(char-downcase c))
       (t (char-upcase c))))))



(defun dash-transform-camel-case
    (name &optional
	    (convert-case (readtable-case *readtable*)))
  (declare (type string name)
	   (values simple-string &optional))
  (let* ((start-p t)
	 after-up-p
	 multi-up-p
	 (len (length name))
	 (elt-type (array-element-type name))
	 (outbuff
	   (make-array
	    len :adjustable t :fill-pointer 0 :element-type elt-type))
	 (upcase-token
	   (make-array
	    0 :adjustable t :fill-pointer 0 :element-type elt-type)))
    (declare (type array-dim len)
	     (type boolean start-p after-up-p multi-up-p))
    (do-vector (c name (simplify-string outbuff))
      (declare (type character c))
      (let ((up-p (upper-case-p c)))
	(declare (type boolean up-p))
	(setq c (char-readtable-case c convert-case))
	(cond
	  (up-p
	   (when after-up-p
	     (setq multi-up-p t))
	   (unless (or start-p after-up-p)
	     (vector-push-extend #\- outbuff))
	   (setq after-up-p t)
	   (vector-push-extend c upcase-token))
	  (after-up-p
	   (setq after-up-p nil)
	   (do-vector (uc upcase-token)
	     (vector-push-extend uc outbuff))
	   (when multi-up-p
	     (vector-push-extend #\- outbuff)
	     (setq multi-up-p nil))
	   (setf (fill-pointer upcase-token) 0)
	   (vector-push-extend c outbuff))
	  (t
	   (vector-push-extend c outbuff)))
	(when start-p
	  (setq start-p nil))))))

;; (dash-transform-camel-case "FooBar" :upcase)
;; => "FOO-BAR"

;; (dash-transform-camel-case "fooBar" :upcase)
;; => "FOO-BAR"

;; (dash-transform-camel-case "fooXYZbar" :upcase)
;; => "FOO-XYZ-BAR"

;; (dash-transform-camel-case "XYZfoo" :upcase)
;; => "XYZ-FOO"

;; (dash-transform-camel-case "XYZfoo" :invert)
;; => "xyz-FOO"

;; (dash-transform-camel-case "FooBar" :invert)
;; => "fOO-bAR" ;; oddly perhaps
