#|                                                      -*- lisp -*-

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#

#-:asdf
(require #:asdf)

(in-package #:asdf-user)

#-:lupine-asdf
(asdf:find-system '#:lupine-pmmt)  ;; defpackage

(in-package #:lupine/system)

(defsystem #:lupine-mop
  :depends-on (#:closer-mop #:lupine-aux)
  :components
  ((:file "package")
   (:file "aux" :depends-on ("package"))

   #+NULL-UNBOUND-SLOTS
   ;; unused - FIXME, move into a seperate module
   (:file "null-unbound-slotd"
    :depends-on ("package"))
   #+NULL-UNBOUND-SLOTS
   (:file "ext-std-class"
    :depends-on ("null-unbound-slotd" "aux" "package"))
   ))
