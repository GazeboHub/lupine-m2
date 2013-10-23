;;; -*- Mode: Lisp; -*-
;;; Ystok-Library - System definition for LispWorks
;;; Copyright (c) 2003-2012 Dr. Dmitriy Ivanov. All rights reserved.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

;(pushnew :ys-product *features*)
;(pushnew :debug *features*)
;(removef *features* :debug) 
;(pushnew :Russian *features*)
;(yl:debug-on :yl)

(defsystem YSTOK-LIBRARY (:object-pathname (lw:current-pathname
	#+(not (or German French Russian debug))		"bin/"
	#+(and (not German) (not French) (not Russian) debug)	"bin/debug/"
	#+(and German (not debug))				"bin/de/"
	#+(and German debug)					"bin/debug/de/"
	#+(and French (not debug))				"bin/fr/"
	#+(and French debug)					"bin/debug/fr/"
	#+(and Russian (not debug))				"bin/ru/"
	#+(and Russian debug)					"bin/debug/ru/"))
 :members
 ("yl-package" 
  "macros"
  "debug"
  "functions" 
  ("win32"	  	:features (and :lispworks :win32))
  ("lw-compat"		:features (not :lispworks))
  "locale" 
  ("lang/de"		:features :German)
  ("lang/fr"		:features :French)
  ("lang/ru"		:features :Russian)
  "meta-parse"
  "meta-parse-number"
  "file-utils"
  ("ys-product"		:features :ys-product)
  ("ys-stubs"		:features (not :ys-product))
  ("systema"		:features :ys-product))
 :rules
 ((:in-order-to :compile :all (:caused-by (:compile "yl-package"))
   (:requires (:load "yl-package")))
  (:in-order-to :load :all
   (:requires (:load "yl-package")))

  (:in-order-to :compile ("debug" "functions" "file-utils")
   (:caused-by (:compile "macros"))
   (:requires (:load "macros")))
  (:in-order-to :load ("debug" "functions" "file-utils")
   (:requires (:load "macros")))
  
  (:in-order-to :compile "locale" (:caused-by (:compile "macros" "win32" "functions"))
   (:requires (:load "macros" "win32" "functions")))
  (:in-order-to :load "locale"
   (:requires (:load "macros" "win32" "functions")))

  (:in-order-to :compile ("de" "fr" "ru")
   (:requires (:load "locale")))
  (:in-order-to :load ("de" "fr" "ru")
   (:caused-by (:load "locale"))
   (:requires (:load "locale")))

  (:in-order-to :compile "meta-parse-number"
   (:caused-by (:compile "locale" "meta-parse" "de" "fr" "ru"))
   (:requires (:load "locale" "meta-parse")))
  (:in-order-to :load "meta-parse-number"
   (:requires (:load "locale" "meta-parse" "de" "fr" "ru")))

  (:in-order-to :compile "file-utils"
   (:caused-by (:compile "functions" "win32"))
   (:requires (:load "functions" "win32" "locale")))
  (:in-order-to :load "file-utils"
   (:requires (:load "functions" "win32" "locale")))

  (:in-order-to :compile ("ys-product" "ys-stubs")
   (:caused-by (:compile "functions"))			; expand inline definitions
   (:requires (:load "debug" "locale" "functions")))
  (:in-order-to :load ("ys-product" "ys-stubs")
   (:caused-by (:load "yl-package"))			; do not loose additional exports
   (:requires (:load "debug" "locale" "functions")))

  (:in-order-to :compile "systema"
   (:caused-by (:compile "locale"))			;"ys-product"
   (:requires (:load "ys-product")))
  (:in-order-to :load "systema"
   (:caused-by (:load "locale"))			; in-bundle macro
   (:requires (:load "ys-product")))
))

;(ys:defmodule :ylib (1 0 :abbrev "lib")) ; <- set in systema.lisp
;(lw:compile-system 'YSTOK-LIBRARY :load t)