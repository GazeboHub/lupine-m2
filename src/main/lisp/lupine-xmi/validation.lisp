
(in-package #:xmi)

(defconstant* version-namespace (simplify-string "20110701")
  "Refer to MOF 2 /XMI Mapping Specification, v2.4.1, subclause 7.3.1

See also: `compute-xmi-namespace'")
    
(defun compute-xmi-namespace (&optional (version-ns version-namespace))
  "Refer to MOF 2 /XMI Mapping Specification, v2.4.1, subclause 7.3.1"
    (declare (type string version-ns)
        (values simple-base-string &optional))
    (concatenate 'simple-base-string
        #.(simplify-string "http://www.omg.org/spec/XMI/")
        version-ns))

