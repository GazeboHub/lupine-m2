

# Criticisms of the OMG Official Lisp IDL Mappig


## Module to Package Mapping - Import Package of Containing Module

cf.
* [CORBA 3.1 Part 1][formal-08-01-04] sublause 7.19, _CORBA Module_
* [CORBA 3.1 Part 1][formal-08-01-04] sublause 7.20, _Names and Scoping_
* [Lisp IDL Mapping][formal-00-06-02] subclause 2.11, _Mapping of Module_

**Summary**

The formal _Lisp IDL Mapping_, subclasuse 2.11, specifies that an IDL
_Module_ will be interpreted as a Lisp _Package_. However, the
subclause does not specify any features in regards to _name scoping_
within the IDL _Module_ thusly interpreted.

**Proposed Resolution**

The Lupine-CORBA IDL mapping for Lisp proposes to extend the formal
_Lisp IDL Mapping_, subclasuse 2.11, with an added feature of
_importing the package interpreted of any containing module or other
scope, within the packge interpreted of a contained module or other
scope_.

**Notes**

In regards to interpretation of IDL modules, within Common Lisp,  the
Lupine-CORBA IDL Mapping proposes to define a distinct class,
_Module_, with the following slots and accessors (as may be defined on
a superclass, named _Naming-Context_)

* **Package** - _context-package_, to the effect of returning the Lisp
  package interpreted as the name scope for the module (or naming
  context)
* **Name** - _context-name_, returning a value representting an IDL
  _qualified name_ for the  module (or naming context)
* **Containing-Context**, _context-containing-context_, returning the
    _Naming-Cotext_ object representing a naming context's containing
    context. For the root context, may return _null_

**Further Notes**

TO DO: Define a class, _qualified name_, cf _module-name_ and general
sematics of CORBA name scoping

**Motivations**

The goal of this revision is to support a full model of IDL name and
scope semantics, within the CLOS model defied for the IDL
interpreter.

It is hoped that this may ultimately serve to be of use within
reflective procedures by ORB clients, on an object request broker
implementing this object model - cf [CORBA Reflection][omg-reflec]


[formal-08-01-04]: http://www.omg.org/spec/CORBA/3.1/
[formal-00-06-02]: http://www.omg.org/spec/LISP/1.0/
[omg-reflec]: http://www.omg.org/spec/RFLEC/
