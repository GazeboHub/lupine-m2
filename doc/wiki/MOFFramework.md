Project Lupine M3/M2 Framework onto MOF
=======================================

_**Table of Contents**_

[TOC]


## Summary

Project Lupine is developing an MOF metamodel for Common Lisp programs, the Lupine metamodel. The Lupine metamodel will be an M2 metamodel, developed onto MOF (M3), supporting the development of models (M1) for Common Lisp programs (M0). The Lupine M2 metamodel is being designed onto the Common Lisp programming language, second edition - commonly referred to as CLtL2 or, for its standardization after the work of ANSI X3J13, ANSI Common Lisp.

### Design Considerations onto MOF

Project Lupine will rely on MOF Core 2.4.1 for purpose of standardization, in developing an MDA-centric modeling system in Common Lisp. Considering the modularity of standards within the domain of OMG MDA, and the reusability of those models in the development and application of modeling tools, Project Lupine will focus firstly on the development of an MOF core system (supporting MOF at the M3 _metalevel_), prior to extending that system with the Lupine M2 metamodel and M1 modeling tools for Common Lisp.

Commensurate with the development of the Lupine M3 core, Project Lupine should also develop a protocol for XMI I/O for serialized metamodels (M2), and later, serialized models (M1) in any supported metamodel framework. _(ToDo: Note how XMI serves as a serialization format for  MOF)_

Towards the development of the XMI infrastucture for Project Lupine, CL-XML is proposed as a platform for XML processing, considering CL-XML's existing support for XML XSD schema (onto XMI integration with XML schema) and features such as XPath processing, also available in CL-XML. (Project Lupine has begun a fork of CL-XML, integrating the CL-XML SBCL branch with the CL-XML baseline. The development ofmthe fork is presently paused, in the interest of developing and intrgrating extensions onto ASDF -  for purposes of release tooling, documentation generation, resource packing, and resource publishing, analogous to procedures presented in CL-XML's own upstream codebase. Furthemore, the development of that fork is "On pause," while concepts underlying the development of Project Lupine may be furthermore clarified, towards the project's continuing development as a resource for models of systems modeling, developed onto the Common Lisp platform.)

Project Lupine may secondly consider the development of code and tooling for the UML M2 metamodel, as well as the SoaML, SysML and BPMN M2 metamodels, and other metamodel systems developed onto OMG MOF, as well as architectures such as TOGAF - as also supported with the [Modelio][modelio] modeling platform. At such a time, Project Lupine may develop _integtation interfaces_ for data file I/O with common modeling tools also in the MDA domain, such as [Cubetto][cubetto], [Papyrus][papyrus], [ArgoUML][argo], [Modelio][modelio], [Rational Rose][rose], and [MagicDraw][md], as well as any other third party tools also addressing components of OMG MDA, and taken as topics of interest in workflows using components developed in Project Lupine.

Lastly, Project Lupine may propose to apply the Lupine core for OMG MOF onto CORBA 2, for purpose of integrating with such network authentication and data transfer protocols as may be implemented with CORBA2 and its extensions. This work may incorporate the OMG MOF to IDL mapping ([MOF2I][mof2i]).


#### MOF/UML Interdependence

(Referring to _Figure 7.1 - MOF imports from the UML Core_ at PDF page 20 (printed page 10) of _[MOF Core Specification, v2.4.1][formal-11-08-07]_)

MOF depends on some features of UML, as by way of _package import_ and _package merge_ operations. Specifically, the following packages from UML Infrastructure are used in MOF Core, and may be used within other MOF packages.

* UML Primitive Types - _::PrimitiveTypes_ (direct dependency from package _import_ by MOF::Common, also imported by UML Core Constructs)
* UML Kernel Classes - _UML::Classes::Kernel_
* UML Core Constructs - _InfrastructureLibrary::Core::Constructs_ (a transitive dependency, by way of _package merge_ from the UML Kernel Classes)

_Assorted notes follow_

_(ToDo: Continue to develop and to clarify the design, here. E.g how is the concept of a 'package' developed in MOF? and in amy way parallel to thr concept's development in UML?)_

_(ToDo: Note that not all MOF metamodels are, per se, "Object oriented", e.g CWM and its database-oriented design)_

## Overview: MOF

Some highlights about MOF (citing [formal-11-08-07][formal-11-08-07], clause 7.2) would include:

* MOF 2 enables component-aided modeling, incorporating reusable modeling packages and modules
* Designed with seperation of concerns in regards to technologies of modeling platforms, programming languages, model interchange systems, modeling system capabilities and services (reflection, versioning, etc)

### Metalevels in the MOF Architecture (M3..M0)

The _MOF Core Specification, v2.4.1_ ([formal-11-08-07][formal-11-08-07]) in clause 7.3, denotes a four layered _metalevels_ architecture to OMG _Model Driven Architecture_ (MDA). Those layers are summarized by David Frankel, in the book _[Model Driven Architecture: Applying MDA to Enterprise Computing][frankel2003]_ (Kindle edition) - near Kindle location 1108 - as well as in _UML Infrastructure Specification, v2.4.1_ ([formal-11-08-05][formal-11-08-05]) clause 7.10, and is summarized below:

* **M3**, the metamodel model (alternately, meta-metamodel) providing constructs for metamodel definition. This layer is reified by MOF, itself.
* **M2**, the metamodel, such as with [CWM][cwm], [UML][uml], or [SoaML][soaml] metamodels.
* **M1**, the model, as an extension of the metamodel's concrete syntax, illustrating features of the entity being modeled, and their relations. In a UML diagram system - in the UML metamodel (M2) -  the M1 model may furthermore incorporate a _UML profile_ providing _stereotypes_ extending on features of the UML metamodel (M2).
* **M0**, entity data and objects


[frankel2003]: http://www.goodreads.com/book/show/428880.Model_Driven_Architecture
[formal-11-08-07]: http://www.omg.org/spec/MOF/2.4.1/
[formal-11-08-05]: http://www.omg.org/spec/UML/2.4.1/
[cwm]: http://www.omg.org/spec/CWM/
[uml]: http://www.uml.org/
[soaml]: http://www.omg.org/spec/SoaML/
[ocl]: http://www.omg.org/spec/OCL/
[xmi]: http://www.omg.org/spec/XMI/
[mof2i]: http://www.omg.org/spec/MOF2I/
[odm]: http://www.omg.org/spec/ODM/
[cubetto]: http://www.semture.de/en/cubetto-toolset-en
[papyrus]: http://www.eclipse.org/papyrus/
[argo]: http://argouml.tigris.org/
[modelio]: http://www.modeliosoft.com/en/products/features.html
[rose]: http://www-03.ibm.com/software/products/us/en/ratirosefami/
[md]: http://www.nomagic.com/products/magicdraw.html
