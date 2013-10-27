#|

  Copyright (c) 2013, Sean Champ. All rights reserved.

  This program and the accompanying materials are made available under
  the terms of the Eclipse Public License v1.0 which accompanies this
  distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html

|#


(in-package #:cl-user)

(defpackage #:lupine/ns
  (:use #:lupine/final #:lupine/aux #:cl)
  (:export
   #:ncname
   #:simple-ncname

   #:namespace
   #:simple-namespace
   #:prefix-qualified-namespace
   #:namespace-string
   #:namespace-package
   #:namespace-local-names-table
   #:namespace-prefix-table

   #:make-namespace
   #:ensure-qname-string
   #:find-prefix

   #:namespace-registry

   #:namespace-condition
   #:name-not-found
   #:name-not-found-error
   #:simple-name-not-found-error

   #:namespace-not-found
   #:namespace-not-found-error

   #:compute-namespace

   #:namespace-prefix-condition
   #:namespace-prefix-bind
   #:namespace-prefix-unbind
   #:prefix-already-bound
   #:prefix-already-bound-error
   #:prefix-not-found
   #:prefix-not-found-error

   #:resolve-prefix-namespace
   #:ensure-qname
   #:ensure-qname-symbol

   ))