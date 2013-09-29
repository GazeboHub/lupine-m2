Modeling Relations Among Common Lisp Data Types in Lupine M2
============================================================

_**Table of Contents**_

[TOC]

## Some Types of Model Element Relation in UML (M2)

Considering three specific modes of model element relation in UML, namely:

* Generalization
* Extension
* Realization

(referencing Cubetto)

This discussion will address how those three modes of relation can be factored onto the Common Lisp type system.

## Class and Type Relations in Common Lisp

Common Lisp defines concepts both of _Class_ and of _Type_.

* A Common Lisp _class_ has one or more _superclasses,_ as well as an ordered _class precedence list_ typically including at least the class itself, and the _system class_ 'T'.
* A Common Lisp class, once defined in the Lisp environment, serves as a type designator, both as the class itself and in the class' name, for a named class. 
* A Common Lisp class has a _metaclass_, namely a class that the class itself is an _instance of_. (Classes, in Common Lisp, are first-order objects.)
* The matter of _generalization_ with regards to the definition of _types_, for types defined as by way of _cl:deftype_ or lexically scoped declarations, becomes slightly more complicated. The complexity of the model, in that context, may be attributed to the complexity of type definitions permitted in CLtL2.

### Complex Type Relations in Common Lisp

The Common Lisp type system permits the definition of types that may not present an exacting relation of generalization, though being no less relevant to the Common Lisp compiler, for purpose of type-oriented optimizations. 

A modeling system based on MOF may endeavor to represent the relations of types in a complex Common Lisp type definition. Some reference may be drawn onto class diagrams in UML (as functionally within the MOF M2 and M1 tiers) however the complexity of the Common Lisp type system may serve to require something of an original approach to the metamodel (M2) design, for accurate modeling of Common Lisp type relations, in Common Lisp programs.

This discussion proposes to draw knowledge from the CType functionality in CMUCL, latterly forked in SBCL. The intention of drawing on that implementation-specific feature is not as if to lock the metamodel design into the features of any one Common Lisp implementation, but rather to inform the development of the Lupine M2 metamodel in accord with a graph defined of the CTypes model, in compiler interpretation of Common Lisp type definitions.

#### Common Lisp Compound Type Specifiers

_(ToDo: Also model these type specifiers, in the Lupine M2 metamodel. Firstly, determine how to best denote relations between these compound type specifiers and the respective types combined with relations in the same.)_

##### Type Specifiers, 'Or', 'And,' and 'Not'

The Common Lisp type system includes type specifiers for defining a _type_ as a _union_ or _intersection_ of one or more types, or alternately as a _complement_ of one type - respectively, the 'Or', 'And', and 'Not' type specifiers.

##### Type Specifiers, 'Eql' and 'Member'

The compound type specifiers 'Eql' and 'Member' provide a means for defining an object's type based on object equivalence, using the object equality predicate, 'EQL'.

##### Type Specifier, 'Satisfies'

The compound type specifier, 'Satisfies,' provides a means for defining an object's type as a consequence of the return value of a predicate function, when that predicate function would be applied to an object for purpose of type comparison. 

(The behaviors are unspecified, if a predicate function used in a 'Satisfies' type definition may produce side effects beyond the computation of a _generalized boolean_ return value.)

#### Function Types in Common Lisp

Common Lisp defines functions as first-order objects. A Common Lisp program may declare that an object is a function, and may furthermore specify the argument value types and - furthermore, optionally - the return value types for the function object. Though typically optional from a perspective of Common Lisp program behaviors, an accurate function declaration  may serve to support optimizations in compilation and runtime behaviors, for the function on which a function type is declared and for functions calling that function, in a program being compiled and/or ran in a given Common Lisp implementation.


### Superclass and Supertype 

Considering that a class' name may also be used as a type name, and that not all types are classes, the Lupine M2 Model proposes that the _superclass_ relation inherits from the _supertype_ relation _(ToDo)_ - more generally, considering that the set of _types_ is a superset of the set of _classes_.

Furthemore, the Lupine M2 model proposes that the _supertype_ relation and the _type_ relation may be generalized, both, to a common ancestor in the metamodel. _(ToDo)_

### Class and Metaclass

Every Commmon Lisp class defined in CLtL2 is defined with a specific _metaclass_.

* System Class
* Built-In Class
* Structure Class
* Standard Class

Additionally, the type, _Condtion Type_ is defined in CLtL2. It would be assumed that _a Condition Type_ is a class, in a sense, considering that a specific _class precedence list_ is defined for each type of _Condition_. 

#### The ANSI CL 'System Class' Type

Essentially, a _System Class_ defined in ANSI CL may be implemented as exactly one of a _Structure Class_, a _Standard Class_ or an implementation-specific _Built-In Class_, in any single Common Lisp implementation. Inasmuch, the exact relation between a _system class_ and any of a _built-in_, _structure_, or _standard class_ would be comprised as an _implementation specific_ feature - as explained in the book, CLtL2, by Guy Steele. 

The _System Class_ type may be understood as it probably comprising a feature that would have been defined by ANSI X3J13, after the CLtL2 the book and prior to the finalization of ANSI Common Lisp (CLtL2, the programming language). It may be expected that definitions using metatypes _Built-In Class_ and _System Class_ will not be encountered in portable Common Lisp programs, except if to alter the behaviors of fundamental _implementation classes_, in any specific Common Lisp implementation.

In a portable Common Lisp program, a class may not be defined as having a _System Class_ as a _superclass_. The Lupine M2 model should seek to specify that limitation, within the metamodel, using OCL. _(ToDo)_

#### Class/Metaclass Relations onto UML (MOF M2, M1)

This discussion proposes that an M3 model for Common Lisp, if implemented as a _UML profile_, may utilize a _realization_ relation in UML, for denoting the relation between a _class_ and its _metaclass_. It would be recommended that a _realization_ relation, as such, would be denoted as a _"metaclass"_ relation, in order to indicate the application of that type of _relation,_ within a visual model.

For M2 and M1 modeling of Common Lisp programs onto UML, a UML stereotype extending _realization_ may be defined, for indicating the _metaclass_ relation in UML diagram tools supporting _UML profile_ extension. _(ToDo)_
