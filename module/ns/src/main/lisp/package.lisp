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
   #:prefix-qualified-namespace
   #:namespace-string
   #:namespace-package

   #:make-namespace
   #:make-prefix-qualified-namespace

   #:compute-qname-string
   #:find-prefix

   #:namespace-registry
   #:make-namespace-registry

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
   #:bind-prefix
   #:compute-qname
   #:compute-qname-symbol

   #:ensure-standard-namespaces
   ))

(defpackage "http://www.w3.org/XML/1998/namespace"
  ;; cf. http://www.w3.org/TR/xml-names/#ns-decl
  (:nicknames #:ns/xml)
  (:export  #:|base| #:|id| #:|lang| #:|space| )
  (:use))

(defpackage "http://www.w3.org/2000/xmlns/"
  ;; cf. http://www.w3.org/TR/xml-names/#ns-decl
  ;;
  ;; NOTE: Application developers may be cautioned before binding
  ;; symbols in this package to any document-specific values. For
  ;; example, Whereas in an XML namespace, xmlns:quux may denote one
  ;; namespace in a document A and an entirely other namespace in a
  ;; document B, then if ns/xmlns::|quux| would bound to any single
  ;; value in processing the former document, the behaviors are
  ;; unspecified for how the binding of ns/xmlns::|quux| would be
  ;; handled in processing the latter document.
  ;;
  ;; For most intents and purposes, this package may simply serve in
  ;; denoting the xmlns namespace and any symbols that may be interned
  ;; in that namespace, though normal application procedures
  ;;
  ;; FIXME: Concerning "Package as persistent collector" - it's up to
  ;; the application developer to ensure that symbols will be
  ;; uninterned when no longer needed in any xmlns/foo package
  ;; namespace. Regarding the matter of packages serving as persisent
  ;; objects, implementation-specific features may be avaialble for
  ;; deleting unused packages.
  (:nicknames #:ns/xmlns)
  (:use))
