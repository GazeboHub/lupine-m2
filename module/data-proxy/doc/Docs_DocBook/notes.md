The goal of the Lupine Data Proxy module is to define a scalable
framework for mediation of data objects via data access protocols
operating via interfaces onto services external to a Common Lisp
image. In reference to Gamma _et. al._,
_[Design Patterns: Elements of Reusable Object-Oriented Software](http://st-www.cs.illinois.edu/users/patterns/DPBook/DPBook.html)_
(sometimes colloquially referred to as _GoF book_,
[cf. C2 Wiki](http://c2.com/cgi/wiki?GangOfFour)) a Lupine Data Proxy
service would define  a _mediator_ framework onto _proxy_ objects
classes accessible via individual information service protocols.

Prospective extensions would include support for (onto cl-service-protocol)
* Networked Knowledge Representation (KR) Services
    * SPARQL
	* CORBA IDL mapping for OWL (to do)
* Networked Object Services
    * CORBA (updating CLORB to CORBA 3.3 compliance and CCM support,
      defining an original IDL-to-Lisp mapping utilizing features of
      CLOS) (cf, [OMG Object Management Architecture (OMA)](http://www.omg.org/oma/))
    * SOAP, WSDL, etc (extending CXML, cf. Lupine-M2-XSD, optionally with HTTP integration onto Hunchentoot, if not a new HTTP peer framework under cl-service-protocol)
* Host Message Bus Frameworks
    * [DBus](http://www.freedesktop.org/wiki/Software/dbus/) message bus framework (cf. desktop frameworks, typically in the Linux desktop domain)
    * JMX (cf. [JSR 3: JavaTM Management Extensions (JMX) Specification](http://jcp.org/en/jsr/detail?id=3), [JSR 160: JavaTM Management Extensions (JMX) Remote API](http://jcp.org/en/jsr/detail?id=160)) (also cf. [Apache ActiveMQ](http://activemq.apache.org/))
* External Devices and Message Bus Frameworks (cf. cl-service-protocol)
    * I2C hardware interface (Linux)
    * SMBus hardware (?)
