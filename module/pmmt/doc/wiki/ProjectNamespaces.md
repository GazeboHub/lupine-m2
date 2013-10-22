Project Namespaces
==================

## Overview

[Lupine PMMT], in its implementation,  essentially provides a subset of project management features , in extension and constraint of an ASDF _component structure_ model onto abstrsct _resource_ and _project management_ models.

## Project Namespaces

[Lupine PMMT] defines a concept of a structured _project namespace_. 

* There exists, a root project namespace
* There exists, a class _project_
* Within each _project_, there exists a _project module_
	* A _project module_ is a _resource_
	* Each _project module_ is an _instance_ of a single _project module type_
		* Each _project module type_ must be defined as a _resource type_
		* Each _project module type_ must be defined within a _project model_
			* Each _project model_ is a _resource_
	* Each _project module_ will serve as a container for zero or more _resource_ instances_, such that:
		* Each such _resource_ instance may be identified with a URI
		* Each such _resource_ instance will identify its _containing project management module_ in a procedure available to a program system
	* Each _project module type_ should be defined with characteristics unique to a set of qualities unique to the _resources_ contained by the _project module_, such as:
		* Media type (? this may be redundant, or perhaps too "fine grained," as a project module type qualifier)
		* Intended _resource usage role_, as in _use cases_:
			* 'software code' or 'component documentation' in a 'software project' _view_
			* 'text content' or 'illustration' in a 'document' _view_
			* 'project' or 'subproject', in a 'project management' _view_
			* 'network', 'host', 'service', or 'application', in a 'network administration' _view_
	* A _project module_ may in itself define a _project_
		* Not every _project module_ will in itself define a _project_
		* A _project_ `B` defined as a _module_ of a project `A` shall be denoted as  `B` being defined as a _subproject_of`A`
* cf. [FOAF]
	* A _ontological class_, `C` , having a name _project management context_, may be defined within an OWL _ontology_, `O`
	* An _instance_ `M` of `C` must be denoted as a _project management context_
	* Each _agent_ `A` recognized within a _project management context_ `M` shall _own_ a _root namespace_, 
		* There exists a function `N` such that `N(A, M)` produces a set of projects managed by `A` onto `M`
		* There exists a function, `U`, such that `U(N(A,M))` produces a URI `X` uniquely identifying each `A` formally managing a _project_ within `M`
		* Each _project_ `P` formally defined within `N(A,M)` and defined as having no _containing project_ may be denoted as a _baseline project_ within `N(A,M)`
		 * There exists a function `V` such that `V(A,M,P)` produces a URI unique to each `P` managed by `A` onto `M` -- whether as `P` representing a _baseline project_ of `A` onto `M` or alternately a _subproject_ of a project `P'`
			 * `V` may or may not be defined such that `V(A,M,P')` produces a structured URI containing `V(A,M,P)`
			 	* (Informative) In regards to the former case, a structured URI may be produced as to interpreted by a human user and a program system
			 	* (Informative) In regards to the latter case, an "unstructured" URI (e.g/cf. CORBA) may be interpreted primarily by a program system
* `O` may define _ontological classes_ and _ontological property types_ such as may serve in an informstive role, with regards to denoting -- within an `O'` including `O` -- a set of features `F` (within `O`) for each instance of `A`, `M`, and `P` 
	* `A`, `M`, and `P` may be denoted within `O'` with _instances_ of those _classes_ as defined within `O`, such that those _instances_ may then be denoted with _properties_ as defined within `O`
* `O` and `O'` may be _published_ using a normative _syntax_ (cf. RDF, N3, Turtle) and may then be used such as for purposes of _task management_ within each respective `A` (onto `M`)
* Within a [SPEM] model, `Q` 
	* each `P` may be denoted as a _work product definition_ managed by `A`
	* each resource `R` created within `P` may be assigned to one or more _tool definitions_ `T` within `Q`
		* in utilizing an `implementation` property -- such that may be defined as a property applicable about `ToolDefinition`min a SPEM model --,for each `T` within `Q` representing a _software tool_, `T` may be denoted with a _platform-specific_ _implementation_ on each __software platform type_ (of `T`) defined within `Q` (e.g Ubuntu Linux, Android, iOS Mobile. OS X, MS Windows 7, etc)
		* each _software platform_ defined within `Q` may be defined similarly as a _tool definition_
* SIDEBAR (**TO DO**): A definition of `O` (and an inheriting `O'`) may be facilitated with an additional _work product_ representing an encoding of [SPEM] into [OWL]
* At some point, P may be denoted within an ASDF system definition
	* Class: Resource
	* Class: Project
		* Superclass: Resource
	* etc.
* With regards to definitions of project namespaces within Project Lupine system definitions, it may seem as though it was a matter of "putting the cart before the horse". Alternately, however, it may be regarded as a matter of developing a use case and prototype for `V` and for such _platform-agnostic_ ontologies `O` as may incorporate a URI produced of `V` within a networked project management framework, and ultimately towards such _platform-identifying_ SPEM models as may extend the model described in the previous, into the [MDA] modeling domain.
	* In the interest of _host portability_, in a model for _unique resource definition_ onto ASDF components, an ASDF Component definition may be denoted with an `xsd:Token` (more specifically, an `xsd:NCName`) unique within the component's containing  _resource context_
		* For a _component_ of type, _asdf:system_, the component's containing _resource context_ should be defined as an instance of a _project_ class.
		* For a _component_ of type, _asdf:source-file_, a definition of the component's containing _resource context_ -- such as may be _denoted_ within the component's _component definition_ --the_resource context_ definition of a _source file_ component may be dependant on the _media type_ of the _component_.
			* A _Lisp source file_ may be defined within a _resource context_ such as may be named, colloquially, _"Writing software". That context may be defined, seperately, as it being effectively contained by the project to which the containing system definition is assigned.
			* The similar may apply to a _documentation_ component (e.g `asdf:doc-file`, if not `lupine/doc:docbook-book` (**TO DO**)), in manual or automatic assigmnent to a "Write documentation" _resource context_
	* Broadly, this may serve to suggest that some project management tooling would be defined as to utilize such an extension on ASDF as wouls utilize the resource context identifiers of _components_ and their containing _system definitions_, if not (sometime) also an OWL model such as `O` and its dependamt `O'`
		* Use case: Integrate Omnifocus, iThoughts, and WebDAV such as may be used in a model for mobile synchronization of task management and project outline resources (nb. using ABCL, consider exo-jcr as a candidte WebDAV implementation, as may be interfaced with a normal CL API model)
