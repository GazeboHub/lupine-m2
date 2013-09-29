Project Lupine "To Do" Items
============================

## Codebase Design Context - "To Do"

(Time frame: Unspecified)

* Define an XSD extension onto CXML
    * Consider extending the RELAX NG implementation for CXML

* Define an XMI implementation, integrating the XSD schema for XMI

* Define an implementation of the UML `PrimitiveTypes` package and UML `InfrastructureLibrary` package and its subpackages, wih consideration for bootstrapping the implementation into an MOF implementation (cf. subclause 7.4 of _Meta Object Facility (MOF) Core Specification, version 2.4.1_ and Subpart II of _UML Infrastructure Specification, version 2.4.1_) also referencing `PrimitiveTypes.xmi` and `Infrastructure.xmi` as published by the OMG, for UML 2.4.1

* Define an MOF implementation, for the MOF M3 metamodel defined in `MOF.xmi` and _Meta Object Facility (MOF) Core Specification_ as published by the OMG for MOF 2.4.1

* Define CLIM presentations for all MOF M3 elements 

* Define an editing platform for editing wnd applying MOF elements defined within concrete MOF metamodels and within models extending those metamodels, utilizing those CLIM presentations (concept: Metamodeling/Modeling IDE)

* Define an OCL implementation integrating with the MOF implementation, as must be capable of loading the files `CMOFConstraints.ocl` and `EMOFConstraints.ocl` - as published by the OMG for MOF 2.4.1 - in applying those constraints onto the MOF M3 model

* Define a corresponding UML implementation, with support for UML profiles as published by the OMG amd other organizations

* Define a corresponding implementation of the _Software & Services Process Engineering Metamodel_ (SPEM) 2.0

## Documentation Context - "To Do"

*  Determine whether the project's _wiki_ will be hosted at SourceForge, at GitHub, or using any other third-party wiki service. Key requirement: Capability to manage wiki pages under SCCM versioning, supportimg synchronization with a local source code repository (namely, Git)
    * A GitHub repository's wiki can possibly be synchronized via Git (To do: look into that)
    * SourceForge wiki can be synchronized with local repository using (possibly new) tooling, such that would integrate with SourceForge Allura (This alternative would need development time for developmig such tooling, as well as a commitments for documentation and codebase maintenance)
    * An alternate wiki hosting service may not provide SCCM functionality, directly, but may provide network synchronization support. Additional tooling could provide SCCM-driven management for the wiki pages, albeit offline. (That would also serve to denote a feature for the "Synch with Allura" model.)
    
* Update wiki pages if Allura will not be used to,provide the Wiki hosting

* Consider alternatives for maintaining project documentation.
    * Markdown provides a convenient, though generic syntax for project documentation. As a simple markup syntax, Markdown may not be altogether suitable for complex documentation tasks.
    * DocBook may be considered as an alternative.
        * DocBook provides an extensible syntax for project documentation. However, the XML of a DocBook document can be tedious to write and to maintain. Modularization of project documentatiom may not serve alleviate all of that tedium. 
        * DocBook document layout - in production of deliverable documents in standard document formats (e.g HTML, PDF) - DocBook document layout is managed via DocBook XSL stylesheets. As features of a document process, the regular formatting of content in DocBook XML, together with the regular layout of documents with the DocBook stylesheets, overall may  serve to ensure a certain sense or normalcy in document layout. However, it would likewise serve to remove some features of _document format control_ from the document itself. Document authors may be unfamiliar with such _separation of concerns_, in regards to document formatting. Simply, in contrast to  _user experiences_ with more conventional _word processing_ platforms, it may seem _disorienting_, to not be able to _immediately_ or _directly_ preview the visual layout and presentation of a document, from within the authoring tool.
        * When writing DocBook XML directly - namely, when writing a DocBook document in XML source format - the document author must repeatedly enter the text of XML markup, in addition to the textual content of the document. The time required for markup entry can be decreased - to a limited extent - with application of _template_ features in the editing platform, for repetitive entry of normative, typically block-formatted markup. However, inline formatted markup must still be entered manually, via the document editing tool. The overall duration of time utilized for _document authoring_ is necessarily inclusive of the duration of time utilized for _markup entry_ (whether direcly or via GUI tools), for _document formatting_ (whether directly, or as via stylesheets) and for _content authoring_. It may seem, then, that _the ideal editing platform_ would minimize - within any bounded time frame - the durations of time required for _markup entry_ and _document formatting_, thus allowing a maximum duration of time for _content authoring_.
        * The DocBook toolchain - inclusive of the DocBook schemas, the DocBook stylesheets, and any selection of tools for document validation and document transformation in application of the stylesheets - the DocBook toolchain provides features for automation of some complex editing tasks, such as _index creation_, _footnote creation_, _cross referencing_, _image selection and captioning_, _content block annotation by callout_, and _page-level "Chunking"_. The toolchain may be extended with custom features, furthermore, for integration of DocBook content items within broader information systems, as by way of _linked data_. Such complex toolchain features - as available with the DocBook toolchain, in a manner "Out of the box", and as possible in extension of DocBook within new and innovative _semantic web_ frameworks - such features may be taken into consideration, when estimating the possible effectiveness of DocBook as providing a document format and overall document toolchain.
    * HTML5 may serve as providing another interesting alternative for document formatting. If viewed in contrast to DocBook, HTML5 document layout must be managed altogether manually by document authors, in addition to direct management needed in HTML5, for such  more relatively complex document features as supported with automated formatting and specialized markup in DocBook. However, for projects in which those matters may not serve to pose expressed concerns, HTML5 is well supported with editing tools, on desktop and mobile platforms.
    * This project will not provide documentation in any conventional word processing formats - namely, due to those formats' limitations, with regards to content markup and online publication.
    * One might assume as though it becomes essentially a matter of _"Which format has the nicest GUI?"_ However, in regards to broader _feature sets_, perhaps a decision as in regards to the format of project documentation may be developed from a more complex standpoint.
    * In conclusiom: This project will endeavor to migrate all existing project documentation into DocBook format, and to use DocBook format in new documentation, as may be developed henceforward.
    * This project may continue to use simple Markdown format, in the meanwhile