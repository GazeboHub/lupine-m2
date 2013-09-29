_**Table of Contents**_

[TOC]

## Summary

The _[CL-XML upstream][cl-xml]_ codebase provides a mechanism for effectively _bootstrapping_ the CL-XML system definition facility, onto CL-XML systems, using ASDF in the _bootstrapping_ process. 

In the interest of providing a range of system definitions more closely integrated with ASDF, as for purpose of _dependency management_, Project Lupine defines an _effectively equivalent_ range of system definitions, using ASDF in defining all of the respective systems' _components_. Some of those system definitions will utilize _extensions_ onto ASDF, such that those extensions will be developed in Project Lupine and will serve to ensure _effective equivalence_ of the _third party_ CL-XML _system definitions_ provided by Project Lupine.

Those _extensions_ onto ASDF may be used in other systems. Those _extensions_ will be defined in the _[CL-REL-TOOL][cl-rel-tool]_ system, as managed within Project Lupine.

[cl-xml]: http://common-lisp.net/project/cl-xml/