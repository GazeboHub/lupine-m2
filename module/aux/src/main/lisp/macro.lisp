
#|

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#

(in-package #:lupine/aux)


(defmacro with-gensyms ((&rest names) &body body)
  `(let (,@(mapcar #'(lambda (n)
		       `(,n (gensym
			     ,(simplify-string
			       (format nil "~a-" n)))))
		   names))
     ,@body))

;; (macroexpand-1 '(with-gensyms (a b c) (list a b c)))
;; (with-gensyms (a b c) (list a b c))


(defmacro defconst (name value &optional docs)
  (with-gensyms (%value)
  `(let ((,%value ,value))
     (defconstant ,name
       (cond
	 ((boundp (quote ,name))
	  (symbol-value (quote ,name)))
	 (t ,%value))
       ,@(when docs (list docs))))))

;; (defconst *FOO* "foo")
