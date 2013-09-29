UML Modeling Tools
==================

_**Table of Contents**_

[TOC]

## Modeling Platforms

Project Lupine is surveying the following UML modeling platforms, on the desktop computing platform:

* [Modelio][modelio]
* [ArgoUML][argo]
* [Papyrus][papyrus]

Project Lupine is also making use of the following UML modeling tools, on the mobile computing platform:

* [Cubetto][cubetto]


### Modelio

**Availability**

* _[Free](http://archive.modeliosoft.com/en/products/modelio-free-edition.html)_ and _[Enterprise](http://archive.modeliosoft.com/en/products/modelio-enterprise-edition.html)_ Editions

**Noted Features**

* [Modelio][modelio] is extended with [Modelio Modules](http://www.modeliosoft.com/en/products/modeliosoft-modules-extensions.html)
    * Modules may downloaded and/or purchased via the [Modelio store](http://www.modeliosoft.com/en/modelio-store.html)
* UML Profile Development supported with [MDA Designer](http://archive.modeliosoft.com/en/modules/modelio-mda-designer.html) features (unknown availability, not presently available in the module store), and with some features in recent editions of Modelio Free Edition (baseline)
    * _[Creating Stereotypes](http://forge.modelio.org/projects/modelio-user-manual-english-22/wiki/Modeler-_modeler_mda_services_basic)_
	* _[Adding Stereotypes to Elements](http://forge.modelio.org/projects/modelio-user-manual-english-221/wiki/Modeler-_modeler_building_models_add_stereotypes)_

**Noted Crtiticisms (Modelio 2.2.1)**

* Profile modeling and application, in Modelio, is not straightforward. 
    * The _MDA Designer_ features discussed at the web site (as previously denoted) do not appear to be available as an extenson to Modelio free edition, at this time (are those features only available with the commercially licensed Enterprise edition?)
	* Profiles cannot be modeled as stand-alone diagrams, in the Free edition. 
    * Model element stereotypes can be created, in the Free edition, however stereotype application is unclear. When stereotypes are applied to diagram elements, the user is not given an option to enable display of the typical guillemet notation for stereotypes, e.g. _`<<metaclass>>`_. The diagram does not, by default, display the stereotypes applied to model elements. Stereotype application is only visible by way of the _properties_ view, on a selected diagram element

### ArgoUML

**Noted Features**

* [ArgoUML][argo] implements UML 1.4
* UML profiles can be developed within stand-alone diagrams
* UML profiles can be loaded and applied, relatively easily, within UML diagrams
* Display of Attribute and Operation fields on Class elements can be disabled
* Associations can be illustrated among diagram elements, using convenient "Drag and drop" interaction
* Supports _notation languages_ for UML 1.4, Java, and C++
* Supports _code generation_ for Java and C++


**Noted Criticisms**

* Implements an edition of UML older than the most currently available edition
* Diagram type (diagram, or profile diagram) must be selected at diagram creation time.  (Without close integration between the M1 _profile model development_ and M1 _model development_ features, profiles may be more difficult to develop in parallel to the models in which the profiles are applied.)

### Papyrus 

(To Do)

### Cubetto Mobile

(To Do)


[modelio]: http://www.modeliosoft.com/en/products/features.html
[argo]: http://argouml.tigris.org/
[papyrus]: www.eclipse.org/papyrus/
[cubetto]: http://www.semture.de/en/cubetto-mobile-en/overview
