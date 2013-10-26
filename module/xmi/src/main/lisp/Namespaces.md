XML Namespaces, UML XMI Encoding, and Lupine-XMI
================================================

## Summary: XML Namespace Qualification and Namespace Defaulting

In regards to `UML.xmi` [cf. [UML 2.4.1][uml2.4.1]] there is a matter of inconsistency with regards to application of XML element namespace qualifications. The namespace prefix, _"uml"_ is initially bound to the namespace URI _"http://www.omg.org/spec/UML/20110701"_.That namespace prefix is then used for the initial `uml:Package` element defining the package, _"UML"_. That namespace prefix is not used in the serialization of any later model elements, however. No default namespace binding is estsblished at any point, in UML.xmi. Easentially, those elements contained within that uml:Package element have no XML namespace. (That could be resolved with a simple `xmlns="http://www.omg.org/spec/UML/20110701"` default namespace declaration on the specified `uml:Package` element, although that would alter the original input model)

It being a known feature of the normative XMI serialization of the UML 2.4.1 metamodel, an effective workaround can be defined without altering the original input model. The workaround may be implemented in such a way as to permit forward compatibility with any improved XML namespace declaration practices, such as may be sought with regards to later editions of the UML metamodel.

## An Instance of Ambiguity in the Specification

[[XML-NAMES][xml-names]] itself may seem to present an ambiguous specification in regards to a certain matter of namespace defaulting, as for an element `N` having neither an explicitly qualified nor default namespace declaration, such that `N` would be a child element of `M`, with `M` being qualified with a namespace `A` - whether or not `M` itself is contained within any element having a qualified or default namespace declaration. 

(It may seem that `UML.xmi` and other UML metamodel serializations may have been produced with an XML writer implementation assuming that the qualified namespace of `M` would establish the namespace for `N`, when `N` lacks an _nqualified_ or _default_ namespace declaration. However, not all XML reader implementations may be developed with such an assumption, in regards to that ambiguous quality of [[XML-NAMES][xml-names]]. An XML reader implementation may assume that `N` has no namepace declaration, though that may not be altogether compatible with the XML model encoding practices applied in prooducing the UML metamodel serialization in `UML.xmi` and other normative XMI documemts published by the OMG)

The following XML document presents the pattern denoted in the previous example.

    <A:M xmlns:A="http://a.example.com/">
    	<N/>
    </A:M>

One may notice the distinction between the instance of `M` in that XML document and in the following:

    <M xmlns="http://a.example.com/">
    	<N/>
    </M>

In the latter example, `N` - as an element not being otherwise _namespace qualified_ - `N` naturally assumes the default namespace declaration of `M`. 

In the former example, it may be assumed that `N` has _no namespace_ or rather, has a _Null namespace_. That assumption may seem consistent with regards to behaviors of namespace qualification and namespace defaulting, as specified in [[XML-NAMES][xml-names]]. However, when that assumption is applied in unmarshaling the UML metamodel, it results effectively in that only a very small percentage of the UML metamodel elements encoded in the normative XMI serialization - as encoding the features of the UML metamodel itself - that most of those elements would not be interpreted as having an XML namespace. With regards to the implementation of a namespace-aware - moreover - essentially namepace-dependant processing model, the absence of an XML namespace on those elements may pose a discrete challenge, in regards to the implementation of the same processing model.

## `:QNAME-OVERRIDE` - Specification of Behaviors in XML Namespace Defaulting

The Lupine-XMI UML metamodel unmarshaling components - with regards to `UML.xmi` and other normative XMI model serialization documents published by the OMG - **shall** implement a special namespace handling protocol, within its XML reader components. That special namespace handling protocol **shall** be activated within the XML reader with a runtime feature, `:QNAME-OVERRIDE T`. The purpose of that special namespace handling protocol may be denoted as so: _"A qualified XML name's namespace qualification overrides the null namespace, on contained XML elements having no default namespace to inehrit"_. That feature **shall not** be active when `:QNAME-OVERRIDE NIL`.

The behaviors of the XML processor, either with or without that feature being active, **shall** be denoted in the following specification, within Project Lupine.

> _"Someone open up a new can of coffee, it'll be a long night yet - Anon."_


### Examples

#### First Example

    <A:M xmlns:A="http://a.example.com/">
    	<N/>
    </A:M>

#### Second Example

    <M xmlns="http://a.example.com/">
    	<N/>
    </M>

#### Third Example

     <M xmlns="http://a.example.com/">
     	<B:P xmlns:B="http://b.example.com/">
	    	<N/>
	    </B:P>
     </M>


#### Fourth Example

    <B:M xmlns="http://a.example.com/" xmlns:B="http://b.example.com/">
    	<N/>
    </B:M>


### Behaviors for First, Third Examples - Qualified/Unspecified Element/Child-Element Namespace Interpretation

When `:QNAME-OVERRIDE T`, as when processing a document fulfilling the pattern of the first example denoted in the previous, the _qualified namespace_ of `M` **shall** be interpreted as denoting the namespace of `N`, when `N` has no _qualified namespace_ declaration, no explicit _default namespace_ declaration itself, and no explicit _default namespace_ delcaration to inherit from any 'ancestor' element. 

When `:QNAME-OVERRIDE NIL`, as when processing a document fulfilling the pattern of the first example denoted in the previous, the qualified namespace of `N` **shall** be _"Null"_, whereas `N` does not have a qualified namespace declaration, has no default namespace declaration, and has no 'ancestor' element from which it would otherwise inherit any default namespace declaration.

The behavior of the processor, in either regards, would be the similar as for namespace defaulting when processing the document denoted in the third example, in which the namespace of `N` would be

* _"http://b.example.com/"_ when `:QNAME-OVERRIDE T`
* _"http://a.example.com/"_ when `:QNAME-OVERRIDE NIL`

### Behaviors for Second, Fourth Examples - Default Namespace Inheritance

When processing a document fulfilling the pattern of the second example denoted in the previous, as regardless of the value of `:QNAME-OVERRIDE` in initializing the processor, `N` would naturally assume the default namespace declared of `M`.
 
In processing the document denoted in the fourth example, the namespace of `N` would be, in all instances, _"http://a.example.com/"_

[uml2.4.1]: http://www.omg.org/spec/UML/2.4.1/
[xml-names]: http://www.w3.org/TR/xml-names/