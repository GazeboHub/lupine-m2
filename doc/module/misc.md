Miscellaneous Modules - Notes
=============================

## Interpreting CXML-RNG Elements for Metaclass Representation

### Summary

[CXML-RNG] interprets an RNG schema element, _A_, as to produce an
instance _A'_ of a structure class, `cxl-rng:element`

>>`cxml-rng:element` is a subclass of `%named-pattern`
>>
>>  `%named-pattern` extends its supeclass, `%parent` with a slot, `name`
>>  having no exacting type
>>
>> `%parent` extends its superclass, `pattern` wih a slot, `child`,
>> having no exacting type
>>
>> `pattern` has no superclasses, and a single slot, `nullable` with no
>> exacting type and an initform, `:UNINITIALIZED`
>>
>> This model - in itself - does not preserve XML namespace
>> information, and does not retain information in regards to
>> attribute/element assignments. Further interpretation of the RNG
>> schema model produced by CXML-RNG must be required, in order to
>> derive any such structural information from the base schema model.

Hypothetically, it may be possible to derive a meataclass, _Ca_ from
_A'_. That process may involve:

* Deriving the target namespace of _A'_, for application of that
  namespace as a _naming_ quality onto _Ca_

* Computing the set of requied and optional _attributes_ and required
  and optional _content elements_ for _A'_, such as to map each
  respective "child" item onto a slot definition or slot value in _Ca_


## Use Cases

### DocBook Object Model

Hypothetically an object model may be derived from the DocBook schema,
in is RELAX NG edition, using CXML-RNG and an unspecified amount of
extensional program code. That object model, in turn, may be used with
the CLIM user interface model, in developing a framework for
presentation and editing of DocBook documents.

## Notes

### Model and Semantics

Though it may be possible to develop an algorithm by which a RELAX NG
schema may be applied in an effective tranformation of RELAX NG
schema elements into a CLOS model, there may be additional semantic
information not directly conveyed in the structure of the input RELAX
NG schema, such that that information should however applied in
regards to the design of the resulting object model.

### Similar Work: JAXB

cf. [JSR 222: Java(TM) Architecture for XML Binding (JAXB) 2.0][jsr-222]


[jsr-222]: http://jcp.org/en/jsr/detail?id=222
