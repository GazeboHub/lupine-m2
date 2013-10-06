Miscellaneous Modules - Notes
=============================

## Interpreting CXML-RNG Elements for Metaclass Representation

### Summary

[RELAX NG] defines a semantic model for validation of XML markup productions with a deterministic semantics - effectively, an _XML schema language_, in that rolr alternate to [XML Schema] and the [XML DTD] language. A _schema rule_ in the [RELAX NG] model may be encoded within a _RELAX NG schema_ serialized  in _RELAX NG XML schema syntax_ or a _RELAX NG schema_ serialized in _RELAX NG compact syntax_. (FIXME: Denote RELAX NG's coverage for XML semantics - cf. XML elements, attributes in general, ID and IDREF/IDREFS attributes, etc) (FIXME: Also denote how RELAX NG integrates XML Namespaces. Denote, then, how CXML-RNG implements that facet of RELAX NG, cf. `CXML-RNG:NS-NAME`, `CXML-RNG:NAME`, and lastly, `CXML-RNG:PATTERN-NS` from the _pattern_ structure class `CXML-RNG:VALUE`)

[CXML-RNG] defines a model for interpretation of RNG schema in Common Lisp, using CXML as an _XML interpreter_.

[CXML-RNG] interprets an RNG schema element, _A_, as to produce an
instance _A'_ of a structure class, `cxl-rng:element`. (It may be assumed that _A_ can then be produced of _A'_, symmetrically. Note that _A'_ is defined for purpose of validating an XML production within a RELAX NG semantics for XML markup schema. RELAX NG represents, effectively, an _XML schema language_, in parallel to the official XML Schema model and its possible extensions, and alternately, XML DTD semantics. )

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

Hypothetically, it may be possible to derive a meataclass, _C(A)_ from
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
