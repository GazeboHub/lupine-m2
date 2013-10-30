XML Persistence in CLOS - Project Lupine
========================================

## Conceptual Overview

This module represents a conceptual baseline for the development of an
XML persisetnce layer for CLOS.

## Functional Overview

### `scan-element-types` - `scan.lisp`

Presently, the codebase for this module contains one offhand utility
for modeling XML in Lisp, namely in the function
`scan-element-types`. This utility was made as a matter of study,
during the development of the XMI module for Project Lupine.

A simple API was defined, within this utility, for purpose of modeling
XML qualified names (qnames) and XML elements, as stand-alone
objects. The API has extensive limitations with regards to the
qualifies of XML data models - including, for instance no support for
modeling of XML attributes, processing instructions, or comments. That
simple API is included within this codebase, for archival purposes.

#### QName API

* [Structure Class] `qname` - biding of local name string
  and namespace URI
* [Accessor] `qname-namespace` - return the namespace URI of a
  `qname`, as a string, or null if no namespace URI is defined to the
  qname
* [Accessor] `qname-local-name`- return the local name of a `qname`
* [Function] `qname=` - compare two `qname` instances for equivalence
* [Function] `qname=*` - compare an input namespace URI and local name
  with a `qname` object for equivalence

#### Simple Nodes API

* [Structure Class] `simple-container`
* [Structure Class] `simple-node`
* [Accessor] `simple-container-contents`
* [Accessor] `simple-node-element`
* [Accessor] `simple-node-parent`

#### Reflection onto XML Data Models

* [Function] `scan-element-types`


## See Also

* [[UML 2.4.1](http://www.omg.org/spec/UML/2.4.1/)]
* [[JSR 222: Java(tm) Architecture for XML Binding (JAXB)](http://jcp.org/en/jsr/detail?id=222)]
