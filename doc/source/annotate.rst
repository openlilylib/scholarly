========
Annotate
========


Overview and Syntax
===================

scholarLY is equipped to work with and produce a broad variety of annotation
techniques, with the general aim of producing (or helping to produce) high
quality scholarly documents. There are many reasons one might want to `annotate`
a musical score -- and, for that matter, any number of interpretations of what
an `annotation` might actually be and in what context it serves a role.  Here are a
few scenarios in which annotations may be useful within the typesetting process:

- Collaborative (perhaps version-controlled) workflows in which multiple members are contributing to the engraving of a musical score
- Critical reports / editions for scholarly publications
- Large projects involving lengthy score documents

All of the features are highly extendable and mostly independent of each other,
meaning that they can be individually toggled and configured for any number of
unique projects.


Minimum Example
---------------

Before going through the bells and whistles of `annotate`'s functionality, we'll
begin with this minimum example which includes an *annotation* in the context of
a compilable LilyPond document:

.. literalinclude:: ../examples/annotate/min.ly




Compiling this example will print the music ``c4 e( d) d`` mostly as expected,
but with the annotated grob (in this case, a slur) printed in a color other than
the default black.(link to cutout from the printed score)
If you are working in Frescobaldi, the line of code with ``\criticalRemark ...``
should be highlighted as well.(link to a screenshot of Frescobaldi)

**Here should be an image of the resulting output from the example.**

**Here should be a screenshot of the Frescobaldi text input panel.**

These coloring effects are default features of scholarLY, and they confirm
that the annotation engraver has recognized the annotation and created a list
of annotations for this score.


Syntax
------

We'll go over all the ways that annotations can be rendered within -- and continuing
after -- the engraving process. But first, here's a closer look at the syntax
annotations use. Observe the portion of the excerpt that creates an annotation:

.. literalinclude:: ../examples/annotate/min.ly
  :lines: 17-19

This is the common interface of the `annotate` module. The syntactic breakdown
of that code is as follows:

.. table:: scholarLY annotation syntax
   :widths: auto

   ============= ===================== =======================================
    role          example               description
   ============= ===================== =======================================
    `type`        ``\criticalRemark``   invokes the engraver, and announces *type* of annotation
    `properties`  ``\with { ... }``     contains a list of unique properties as key = value pairs
    `item`        ``Slur``              the symbol name for the affected/annotated grob
    `music`       ``e'(``               the music to which the annotation is applied
   ============= ===================== =======================================

A few annotation **types** are predefined by scholarLY, but, as described in the
next section, it is just as easy to create new ones. The minimum **property** to
apply is `message`, which is expected to contain a string. We'll go over other
useful properties as they are explained later in the documentation. **Item** can
be anything from a notehead or stem to a glissando, tremolo, tempo marking, or
any other grob which LilyPond can identify at the instance of the annotation. If
the named item isn't immediately followed by **music** containing one of it, the
document may not successfully compile (this is actually dependent on what
properties, if any, will be applied as music functions or context settings);
typically, you won't encounter such an error, but it is a reasonable thought to
keep in mind when debugging.


Builtin Annotation Types
------------------------

The following builtin annotation types (their hooks, each followed by a general --
not necessarily official -- description) are provided as a convenience:

``\annotation``
  A generic annotation command, for which a ``type`` must be defined in the
  properties (``\with``) block.

``\criticalRemark``
  Final annotation about an editorial decision

``\musicalIssue``
  Annotation about a musical issue that hasn't been resolved yet

``\lilypondIssue``
  Annotation about a LilyPond issue that hasn't been resolved yet

``\annotateQuestion``
  Annotation about a general question

``\annotateTodo``
  Annotate a task that *has* to be finished


Specifying custom types
_______________________

A new type can be specified by using the generic ``\annotation`` command.
Include ``type = <name>`` as a property of the annotation.

::

  \annotation \with {
    message = "my annotation about the following slur"
    type = "my-custom-type"
  } Slur e'(

A likely gotcha with this is that scholarLY won't have default options that it
needs for any `type` of annotation; that isn't a crucial problem to know how to
resolve yet, but keep it in mind if you intend to work with custom annotation
names.


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

Annotations can be made into LilyPond footnotes by including at least the
``footnote-offset`` property (which implicitally triggers the footnote engraver). By
default, scholarLY assumes ``message`` is the footnote text;

Let's take the same example from before and make it a footnote:

.. literalinclude:: ../examples/annotate/footnotes-01.ly
  :lines: 9-17

**Insert result here.. cut page size down and make sure footnote is included**


The ``footnote-text`` property may be included to indicate text different than
the annotation message.

.. literalinclude:: ../examples/annotate/footnotes-02.ly
  :lines: 9-18

**Insert result here.. cut page size down and make sure footnote is included**


This functionality only affects footnotes engraved by LilyPond. Footnotes
specific to HTML, LaTeX, or other languages are dealt with in sections specific
to those methods.


Balloon Text
------------

Annotations can also be engraved as balloon text. As with footnotes, the minimum
argument to print an annotation as ballon text is the ``balloon-offset``
property. message is the default text, and ``balloon-text`` can optionally
specify an alternative message.

.. literalinclude:: ../examples/annotate/balloon-text-01.ly
  :lines: 9-17


And with a unique message for the balloon text:

.. literalinclude:: ../examples/annotate/balloon-text-02.ly
  :lines: 9-18


Keep in mind that LilyPond isn't able to engrave balloon text to spanners (Slur,
Hairpin, TrillSpanner, etc.) yet; attempts to do so will typically cause
LilyPond to crash upon compiling. This is a known problem listed on the
`LilyPond issue tracker`_.

.. _LilyPond issue tracker: https://sourceforge.net/p/testlilyissues/issues/2567/

For convenience, scholarLY avoids even trying to engrave balloon text in these
instances and sends an error message to the console log, making it a little less
frustrating to debug.


Simultaneous Footnotes and Balloon Text
---------------------------------------

It is possible to use both footnotes and balloon text in the same annotation.
Another `known issue`_ is the inability to use footnotes within to balloon text.
Typically, markups in LilyPond can apply footnotes using the alternative
``\auto-footnote`` command. But, as markups within music are technically
different functions than those outside of music, the functionality isn't
compatible and unfortunately hasn't been adapted yet.

.. _known issue: https://sourceforge.net/p/testlilyissues/issues/2819/


General Options
===============

Explain those here...








Exporting Annotations
=====================

Exporting extends annotations...

The currently available output methods are plaintext, LaTeX and HTML (with
highly configurable CSS generation/options). We plan to extend that list to
include at least JSON, markdown and Scheme.


Configuration
-------------

How to configure..

::

  \setOption scholarly.annotate.export-targets #'(plaintext latex html)

So..

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
