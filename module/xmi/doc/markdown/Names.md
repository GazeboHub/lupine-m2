UML Names in Common Lisp
=======================


Though it is not explicitly required by the UML specification, UML
models typically utilize _"[Medial Capitals][mcap]"_ in compound
element names. Lupine-XMI **shall** support two conventions for
interpretation of such compound names:

1. `CamelCaseName` to `Lisp-Name`

In this behavior, the function `LUPINE/AUX:DASH-TRANSFORM-CAMEL-CASE`
is called for producing a "Dash transformed" compound name from the
input name.

2. `CamelCaseName` to `|LispName|`

In this behavior, the input name is not modified. This **shall** be
the default behavior


[mcap]: http://en.wikipedia.org/wiki/Medial_capitals
