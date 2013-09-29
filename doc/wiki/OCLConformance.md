OCL Conformance in Lupine Core
==============================

_**Table of Contents**_

[TOC]

# Summary

[OCL][ocl] defines a typed expression language for specification of refinements onto [MOF][formal-11-08-07] metamodels (M2) and [UML][uml] models (M1). [OCL][ocl] serves as a means for defining unambiguous constraints on model elements, in a platform-neutral approach. 

# Conformance Goals

_[Object Constraint Language, v2.3.1][formal-12-01-01]_ establishes three primary tiers for _OCL Conformance_, in application tooling.

* **Syntax Compliance**
* **XMI Compliance**
* **Evaluation Compliance**

The _Evaluation Compliance_ tier is furthermore detailed with the following comformance points:

* _allInstances()_
* _pre-values and oclIsNew() in postconditions_
* _OclMessage_
* _navigating across non-navigable associations_
* _accessing private and protected features of an object_

A design for compliamce at those tiers and conformance points is outlined in the following sections of this document.

## Syntax Conformance

### Character Set Conformance

The Lisp implementation must support the Unicode Code Set (UCS), or _character set conformance_ cannot be ensured onto OCL, without the application implementing a workaround for the implementation's limitations with regards to character encoding. As demonstrated in [citation needed], strings containing bytes possibly outside the range of the Lisp implementation's supported _character repertoire_ may be parsed into byte vectors, rather than conventional strings, and all of the application's operations on strings and character streams then modified as to support the specialized _byte encoding_. _(Ed note: 'Runes' - where in Lisp is that?)_

### OCL Expressions

#### Classifiers and Packages

An OCL constraint is defined on a model _classifier_ or _type_. A model _classifier_ may be defined as an element within a _package_ in its containing model. The OCL expression language defines constructs for identifying the package context of the classifiers in an expression, explicitly. Refer to _[Object Constraint Language, v2.3.1][formal-12-01-01]_ clause 7.4.5.

#### Variables and Assignments

Variable names in OCL, presumably, may conform to the syntax of _classifiers_ in UML _(ToDo: Verify that onto Appendix A of [Object Constraint Language, v2.3.1][formal-12-01-01])_

In OCL, types must be assigned to variables, as within constraint context headers or other syntactic elements such as in the header of an OCL _let_ expression, or a _def_ expression (using the stereotype, _`<<definition>>`_)

In OCL, values may be assigned to variables as within any of:

* An OCL _`let`_ expression
_(**TODO:** Continue here)_

_(EdNote: ...and it`s simply a constraint language onto MOF and UML. What a tedious constraint language it may seem like, though well defined once the dense explanation of the thing is sorted out, across the main clauses of formal-12-01-01 and its all-important Appendix A.)_

#### Object Reference Expressions

##### OCL `self`

Refer to _[Object Constraint Language, v2.3.1][formal-12-01-01]_ clause 7.4.1.

#### Context Expressions

OCL defines a concept of semantic _context_, as bounded ultimiately by the model in which an OCL expression is defined. 

##### Constraint Context Expressions

Additionally, a sense of syntactic _context_ is defined in OCL, as in regards to effective _headers_ on constraint expressions, whether of _invariant constraints_ or _pre-/postcondition constraints_.

Refer to _[Object Constraint Language, v2.3.1][formal-12-01-01]_ clauses 7.4.2 and A.5.1.4.

###### Invariant Constraint Context Expressions

A _constraint context_ expression for an _invariant comstraint_ may explicitly declare variables and their types, for purpose of predicate and collection operations within the effective constraint definition form.

    context v1 : C1, . . . , vn : Cn inv: 
        <form>

In the evaluation of that expression, the variables `v1` through `vn` - respectively, of types `C1` through `Cn` - will be available for assignments (?) and operations within the invariant constraint _form_. Additionally, the variable `self` - of type, `C` - will be available for operations within the invariant constraint _form_

_ToDo: Determine OCL's syntax and semantics for variable assignment not by way of 'let'_

Alternately, a _constraint context_ expression for an _invariant comstraint_ may simply declare the type of the contextually scoped variable, _`self`_

    context C inv: 
        <form>

In the evaluation of that expression, the variable `self` - of type, `C` - will be available for operations within the invariant constraint _form_

###### Pre-/Postcondition Constraint Context Expressions

The context expression of a pre-/postcondition constraint specifies an operation name, a set of operation paramter names and their types, the result type of the operstion, and the type of the variable, _`self`_. The parameter names will be available for reference within the pre- and postcondition expressions of the constraint, as will the variables _`self`_ and _`result`_.

    context C :: op(p1 : T1, . . . ,  pn : Tn) : T 
        pre: P 
        post: Q
        

In the evaluation of that expression: The variable _`self`_ will be of type _`C`_; the variable _`result`_ will be of type _`T`_; the parameters _`p1`_ through _`pn`_, respectively, will be of types _`T1`_ through _`Tn`_. _`P`_ and _`Q`_ denote, respectively, the precondition and postcondition expressions of the containing constraint expression.


#### Constraint Expressions

##### Invariant Constraint Expressions

##### Conditional Constraint Expressions (pre- and postconditions)


## XMI Conformance

OCL may be serialized as [XMI][xmi], using the EMOF (for OCL in M2) and CMOF (for OCL in M1) metamodels provided at the OMG web site, onto the conventional [MOF to XMI mapping][xmi]. In that approach to OCL serialization as XMI, a design may serve to recommend a certain degree of programmatic coupling between OCL language objects and MOF model objects, such that Lupine may design to implement by way of metaclass properties onto an OCL language model for Common Lisp.

Note, furthermore, that UML itself defines a _Costraints_ model. Refer to _UML Infrastructure Specification, v2.4.1_ ([formal-11-08-05][formal-11-08-05]), clause 11.5.

## Evaluation Conformance

(To Do)


[ocl]: http://www.omg.org/spec/OCL/
[formal-11-08-07]: http://www.omg.org/spec/MOF/2.4.1/
[formal-12-01-01]: http://www.omg.org/cgi-bin/doc?formal/12-01-01.pdf
[formal-11-08-05]: http://www.omg.org/spec/UML/2.4.1/
[uml]: http://www.uml.org/
[xmi]: http://www.omg.org/spec/XMI/
