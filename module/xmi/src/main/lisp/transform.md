
# Names and XML Namespaces in the Lupine-XMI UML Transformations

XMI encodes UML elements as XML elements, for purpose of model
serialization. In each model serialized in XMI, there is a metamodel
used in modeling the serialization, and there is the model represented
in the serialized metamodel. For instance, excerpting the definition
of `UML::Class` in `UML.xmi`

	<?xml version="1.0" encoding="UTF-8"?>

	<xmi:XMI xmlns:xmi="http://www.omg.org/spec/XMI/20110701"
		xmlns:uml="http://www.omg.org/spec/UML/20110701"
		xmlns:mofext="http://www.omg.org/spec/MOF/20110701">

		<uml:Package xmi:type="uml:Package" xmi:id="_0" name="UML"
		URI="http://www.omg.org/spec/UML/20110701">

		<packagedElement xmi:type="uml:Class" xmi:id="Class" name="Class">

		  <!-- ... -->

		  <ownedRule xmi:type="uml:Constraint"
				xmi:id="Class-passive_class" name="passive_class"
				constrainedElement="Class">
			<ownedComment xmi:type="uml:Comment"
				xmi:id="Class-passive_class-_ownedComment.0"
				annotatedElement="Class-passive_class">
				  <body>A passive class may not own receptions.</body>
			</ownedComment>
			<specification xmi:type="uml:OpaqueExpression"
				xmi:id="Class-passive_class-_specification">
				<language>OCL</language>
				<body>not self.isActive implies
					self.ownedReception.isEmpty()</body>
			</specification>
		  </ownedRule>

		  <generalization xmi:type="uml:Generalization"
				xmi:id="Class-_generalization.0"
				general="EncapsulatedClassifier"/>

		  <generalization xmi:type="uml:Generalization"
				xmi:id="Class-_generalization.1"
				general="BehavioredClassifier"/>

		  <ownedAttribute xmi:type="uml:Property" xmi:id="Class-extension"
				name="extension" visibility="public" type="Extension"
				isReadOnly="true" isDerived="true"
				association="A_extension_metaclass">
				<ownedComment xmi:type="uml:Comment"
					xmi:id="Class-extension-_ownedComment.0"
					annotatedElement="Class-extension">
				  <body>References the Extensions that specify additional
						properties of the metaclass. The property is derived from
						the extensions whose memberEnds are typed by the
						Class.</body>
				</ownedComment>

				<upperValue xmi:type="uml:LiteralUnlimitedNatural"
					xmi:id="Class-extension-_upperValue" value="*"/>
				<lowerValue xmi:type="uml:LiteralInteger"
					xmi:id="Class-extension-_lowerValue"/>
		  </ownedAttribute>

		  <!-- ... -->

		</packagedElement>

	  </uml:Package>
	  <!-- ... -->
    </xmi:XMI>


In order to unmarshal a model serialized in XMI, the metamodel used in
the serialization must be recognized in the unmarshaling
procedures. (In regards to UML metamodel, essentially
the UML metamodel may be viewed as representing its own
serialized form -- thus introducing a concept of _metacircularity_ to
the process of unmarshaling the UML metamodel.)

In order to implement a set of procedures for unmarshaling and
implementing the complete UML metamodel in Common Lisp, Lupine-XMI
defines a limited set of UML model elements, manually, as a _bootstrap
set_ prior to the unmarshalling of `UML.xmi`. The set of elements that
would exist as necessary members of that _bootstrap set_ has been
determined through manual analysis of the contents of UML.xmi.

In the current revision of the _bootstrap set_ implemenation, it may
seem as though it was an arbitrary methodology used for the selection
of elements for that _bootstrap set_. Essentially, the first class to
be implemented in that _bootstrap set_ is the class, `UML::Class`. It
was considered that that element's _serialized form_ may serve as a
convenient initial _use case_ for the development of the XMI
interpreter in Project Lupine. The serialized form of that class - in
itself, representing an element of the UML metamodel, serialized in
the UML metamodel - aside to the matter of _semantic metacircularity_,
the class' definition, as the class representing a UML model element,
presents an interesting range of model properties in the metamodel
used to serialize the element's definition.

In order to successfully unmarshal the definition of `UML::Class`, a
number of supporting model elements may be defined, in a sense of
presenting a set of _metamodel_ _stub_ definitions - essentially, the
_bootstrap set_, denoted in the previous.

_**Bootstrap Set for UML::Class** (Current Revision)_

* `UML-CLASS`, in a _stub_ implementation corresponding to
  `UML::Class`, providing transformation of the `ownedAttribute`
  property in its XML element encoding and the `isAbstract` property
  in its XML attribute encoding, as well as inheriting property
  transformations from the class' _stub_ superclasses (whose relations
  as _superclasses_  corresponds to a limited, _stub_ set of
  generalizations of the _UML::Class_ class, itself)

* `ELEMENT` cf. the UML Class `UML::Element`, in a stub implementation
  providing a transformation for the `ownedComment` property

* `NAMESPACE` cf.  the class, `UML::Namespace` and its `ownedRule`
  property

* `CLASSIFIER`, cf. the class, `UML::Classifier` and its
  `generalization` property

* **TO DO:** NAMED-ELEMENT, cf. `UML::NamedElement` and the `name`
    attribute (a good use case for the initial beta testing of the
    attribute transformation model)

That set of _stub_ clsss definitions may serve to provide such
features as would be necessary to unmarshal the set of properties
present in the definition of `UML::Class` in `UML.xmi`. An additional
set of `stub` classes must be defined, for representimg that class`
relation to its containing package, namely thr UML package named
`UML`, with package URI <http://www.omg.org/spec/UML/20110701>

_**Bootstrap Set for UML::Package** (Current Revision)_

(**TO DO**)

**To Do:** Persistent xml:id values in unmarshalled model elements;
  add an XML ID table to `standard-model` (cf. qname registry); also,
  move the namespace registry features into the TRANSFORMSTIOM-MODEL
  class

**To Do:* Describe the nature and usage of `QNAME-REGISTRY`, with
  regards to XML serialization of UML model elements. Also, denote the
  syntactic limitations implicit on UML named element names - namely,
  that the XML encoding of UML named element names serves to impose an
  implicit restriction on the syntax of UML named element names, such
  that a UML named element name must be of type xsd:NCName (TO DO:
  Also introduce a syntax checker to the QNAME-REGISTRY
  implementation, such that it will ensure that each QName's local
  part will be valid onto the syntax of type `xsd:NCName` - type
  `satisfies ncname-p` or somesuch, perhaps mto CXML's existing XSD
  datatypes implementation)

# Types and Refinment within XMI Serialized UML Element Definitions

## Use Case: Modeling of `packagedElement` members

cf. results of shell command

	 grep "xmi:type" UML.xmi |
	  sed 's|<||g' | awk '{print $1 " " $2}' |
	  sort | uniq

There are exactly three types of `packagedElement` defined
  in UML.xmi:

	  packagedElement[@xmi:type="uml:Association"]
	  packagedElement[@xmi:type="uml:Class"]
	  packagedElement[@xmi:type="uml:Enumeration"]

## Metamodels and Types, in Pratice

The set of possible types of definitions that may be validly specified
in the `packagedElement` relation is constrained in UML itself. Note
the `PackageableElement` feature in the following - from UML.xmi, in
the definition of the type `uml:Package`

	<ownedAttribute xmi:type="uml:Property"
	  xmi:id="Package-packagedElement"
	  name="packagedElement"
	  visibility="public"
	  type="PackageableElement"
	  aggregation="composite"
	  subsettedProperty="Namespace-ownedMember"
	  association="A_packagedElement_owningPackage">  ... </ownedAttribute>

The `xmi:type` attribute describes the type of the definition
represented by  the `ownedAttribute` declaration itself. As such,
the xmi:type attribute essentially describes the type of the
definition as a metamodel feature.

The `type` value within that `ownedAttribute` declaration - the
`type` attribute having the same XML namesapace as the
`ownedAttribute` declaration - that attribute may be understood as
describing the type of the value that may be contained in such a
relation as defined in the `ownedAttribute` property. Essentially, it
denotes the type of the model feature described by the metmodel
feature in its XMI encoding.


## Discussion of Types and Element Names in the XMI Specification

In reference to [XMI 2.4.1], subclause 7.6.3:

  "The type attribute is used to specify the type of object being
  serialized, when the type is not known from the model"

Regarding element names, from subclause 7.8.1:

  "The name for XML tags corresponding to model Properties is the
  short name of the property. The name of XML attributes corresponding
  to model properties (DataType-typed or Class-typed) is the short
  name of the property, since each tag in XML has its own naming
  context"

Albeit, there may not appear be a lot of guidance in that, for
integration with components implementing other semantically
complimentary OMG specifications. As far as "Proof of concept,"
however...

## Core Model Features

In order for the UML metamodel defined in UML.xmi to be interpreted, a
corresponding UML implementation may be developed manually, such as to
implement the program and semantics necessary for unmarshaling that
UML metamodel. (Once the UML metamodel defined in UML.xmi can be
interpeted competely, it may serve to add features to the initial
"Boostrap" or "Core" model defined for its intepretation)

## Pakaged Elements

A `packagedElement` relation represents a relation between a UML
Package definition and a definition of a UML element of type
PackageableElement.

The `packagedElement` and `PackageableElement` both may represent
types of UML "core" model elements, such that should be manually
supported in the UML interpreter, if simply for the sake of being able
to load the UML model itself, in its normative XMI encoding
(cf. UML.xmi).

## Core Model Metamodel Types

There are exactly 21 unique element/type relations visible in
UML.xmi - sorted in alphabetical order:

	defaultValue xmi:type="uml:InstanceValue"
	defaultValue xmi:type="uml:LiteralBoolean"
	defaultValue xmi:type="uml:LiteralInteger"
	defaultValue xmi:type="uml:LiteralUnlimitedNatural"

	generalization xmi:type="uml:Generalization"

	lowerValue xmi:type="uml:LiteralInteger"

	mofext:Tag xmi:type="mofext:Tag"

	ownedAttribute xmi:type="uml:Property"

	ownedComment xmi:type="uml:Comment"

	ownedEnd xmi:type="uml:Property"

	ownedLiteral xmi:type="uml:EnumerationLiteral"

	ownedOperation xmi:type="uml:Operation"

	ownedParameter xmi:type="uml:Parameter"

	ownedRule xmi:type="uml:Constraint"

	packagedElement xmi:type="uml:Association"
	packagedElement xmi:type="uml:Class"
	packagedElement xmi:type="uml:Enumeration"

	packageImport xmi:type="uml:PackageImport"

	specification xmi:type="uml:OpaqueExpression"

	uml:Package xmi:type="uml:Package"

	upperValue xmi:type="uml:LiteralUnlimitedNatural"


## Prototype

The UML-CLASS class represents the first working prototype of a "stub"
class for unmarshalling the UML metadmodel described in UML.xmi
