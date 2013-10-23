;;; -*- Mode: LISP; Syntax: COMMON-LISP; -*-
;;; Ystok-URI (RFC3986) - LispWorks system definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 2002-2012 Dr. Dmitriy Ivanov. All rights reserved.

(in-package :cl-user)

;(pushnew :debug *features*)
;(load #P"PROJECTS:ylib;defsys")
;(compile-system 'YSTOK-LIBRARY :load t)
;(pushnew :tester *features*)
;(load #P"PROJECTS:ynet;acl-compat;defsys")
;(compile-system 'ACL-COMPAT :load t)

(defsystem YSTOK-URI (:object-pathname (lw:current-pathname
  #+(and debug (not acl-compat) (not flexi-streams))		"bin/debug/"
  #+(and debug acl-compat)					"bin/debug/acl-compat/"
  #+(and debug (not acl-compat) flexi-streams)			"bin/debug/flexi/"
  #+(and (not debug) (not Russian) (not acl-compat) (not flexi-streams))"bin/"
  #+(and (not debug) (not Russian) acl-compat)				"bin/acl-compat/"
  #+(and (not debug) (not Russian) (not acl-compat) flexi-streams)	"bin/flexi/"
  #+(and (not debug) Russian (not acl-compat) (not flexi-streams))	"bin/ru/"
  #+(and (not debug) Russian acl-compat)				"bin/ru/acl-compat/"
  #+(and (not debug) Russian (not acl-compat) flexi-streams)		"bin/ru/flexi/"))
 :members ((YSTOK-LIBRARY :type :system)
           "package"
           "uri"
           "utils")
 :rules ((:in-order-to :compile :all
          (:requires (:load :previous))
         (:in-order-to :load :all
          (:requires (:load :previous)))) ))

#+ys-product
(ys:defmodule :uri (2 0 :abbrev "uri") :depends-on (:ylib))

;(compile-system 'YSTOK-URI :load t)
