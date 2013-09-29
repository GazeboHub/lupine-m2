Common Lisp Systems Defined by Project Lupine
=============================================

_**Table of Contents**_

[TOC]

## Summary

Project Lupine uses [ASDF][asdf] in defining source code _systems_ for compilation and loading by a Common Lisp implementation.

This docuent serves as an informal description of the system definitions provided by Project Lupine. For detailed documentation about componemts and forms defined in those systems, the reader is advised to consult the documentation system of the respective component's _source language_, or else the _source code_ of the component.

## Systems

### System: lupine-aux

The _lupine-aux_ system provides a small number of utility _forms_, such that may be used at compile time and at load time, within other _systems_

### System: lupine-xmi

The _lupine-xmi_ system defines an [XMI][xmi] implementation for Project Lupine. This system is designed to use [CL-XML][cl-xml] as the _backend_ XML processing system. 

[asdf]: http://common-lisp.net/project/asdf/
[xmi]: http://www.omg.org/spec/XMI/
[cl-xml]: http://common-lisp.net/project/cl-xml/
