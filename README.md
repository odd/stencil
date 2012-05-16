stencil
=======

A simple Scala templating library that uses extra attributes to produce output based on plain HTML and XML files containing example markup.

The main idea is to keep the templates (or stencils) fully viewable in a browser with all example markup still there. The templating directives are therefore expressed as extra attributes on the existing markup elements. When the stencil is applied these attributes are removed from the output along with any surplus example markup.

Directives:
* Let - binds an expression to a name
* Set - sets an attribute value to the resolved value of an expression
* Set Body - sets the body of the corresponding element to the resolved value of an expression
* Do - repeats the corresponding element zero or more times based on the resolved value of an expression, each time binding a new value to the specified name
* Do Body - repeats the corresponding element zero or more times based on the resolved value of an expression

Examples:
'<person x:set-name="person.name" name="Kalle"/>' will produce the output '<person name="Pelle"/>' iff the expression 'person.name' resolves to 'Pelle'
'<person x:let-name="person.name"><name x:set="name">Kalle</name></person>' will produce the output '<person><name>Pelle</name></person>' iff the expression 'person.name' resolves to 'Pelle'
