
(in-package #:lupine/aux)

(defun simplify-string (string)
 "Coerce STRING to a `SIMPLE-BASE-STRING', if possible, or else to a `SIMPLE-STRING', or else err.
 
 See also: `SIMPLIFY-STRING*'"
    (declare (type string string)
        (values simple-string &optional))
    (or (ignore-errors (coerce string 'simple-base-string))
        (coerce string 'simple-string)))

(defun simplify-string* (string)
 "Coerce STRING to a `SIMPLE-BASE-STRING', if possible, else err
 
 See also: `SIMPLIFY-STRING'"
    (declare (type string string)
        (values simple-base-string &optional))
    (coerce string 'simple-base-string))
