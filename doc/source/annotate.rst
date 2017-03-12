========
Annotate
========

Overview and Syntax
===================

Description of what `annotate` accomplishes...


What are annotations to LilyPond?

Annotation properties are set at context mods (i.e. in the ``\with`` block).


A simple example:

.. code-block:: lilypond

  \criticalRemark \with {
    message = "my annotation about the following slur"
  } Slur c4( e d) d



So what happens when we compile this example? If you are working in Frescobaldi,
the annotated grob (in this case, a slur) will be highlighted in a color other
than the default black; the line of code with ``\criticalRemark ...`` should be
highlighted as well. Don't worry, that isn't an error! scholarLY does this to
give visual ..

In any case, scholarLY's annotation engraver behind-the-scenes has
created a list with all the information about each annotation including
location, grob type, message, etc. We'll go over several ways in which that
information can be retrieved, displayed, and exported.

Firstly, here's a closer look at the syntax annotations use.


invocation, properties, grob, music...


Builtin Annotation Types
------------------------

The builtin annotation types.

List...

A new type can be specified by using the generic ``\annotation`` command. A likely
gotcha with this is that scholarLY won't have default options that it needs for
any `type` of annotation; that isn't a crucial problem to know how to resolve yet,
but keep it in mind if you intend to work with custom annotation names.


In-Score Annotations
====================

We can apply annotations as footnotes and/or balloon text markups on a case-by-case
basis. Of course, both of these things are already available in LilyPond - and,
in fact, scholarLY simply wraps those same builtin functions - but the advantage
of implementing these through the annotation interface is that all of the information
used for the footnote or balloon text is now accesible beyond the printed score.

scholarLY provides a simple integration of both features, where they can be
easily toggled and configured in a sensible, organized fashion.


Footnotes
---------

How the footnotes work..


Balloon Text
------------

As with footnotes, the minimum argument to print an annotation as ballon text
is the ``balloon-offset`` property.


Export Options
==============

Exporting extends annotations...


Configuration
-------------

How to configure..


Plaintext
---------

about plaintext output


LaTeX
-----

about LaTeX output

We can use LaTeX code within the message.


HTML
----

About HTML output
