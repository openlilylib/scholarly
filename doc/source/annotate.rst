========
Annotate
========


Overview and Syntax
===================

Description of what `annotate` accomplishes...

There are many reasons one might want to `annotate` a musical score - and, for
that matter, several interpretations of what an `annotation` might be. scholarLY
is equipped to work with and produce a broad variety of annotation techniques,
with the general aim of producing high quality scholarly documents. Here are the
different scenarios in which annotations may be useful within a scholarLY workflow:

- One way
- Another way
- and a third

All of the features are independent of each other, meaning that they can be
individually toggled and configured for any


Minimum Example
---------------

Before going through the bells and whistles of `annotate`'s functionality, we'll
begin with this minimum example which includes an *annotation* in the context of
a compilable LilyPond document:

::

  \version "2.18.2"   % NB: scholarLY is typically tested with the latest
                      %     development release of LilyPond

  % load OLL; always required
  \include "oll-core/package.ily"

  % load scholarLY with the `annotate` module
  \loadPackage \with {
    modules = annotate
  } scholarly

  mus = {
    c4
    % the annotation:
    \criticalRemark \with {
      message = "my annotation about the following slur"
    } Slur e(
    d) d
  }

  \score { \mus }

Compiling this example will print the music ``c4 e( d) d`` as expected, but the
annotated grob (in this case, a slur) will be printed in a color other than
the default black.
If you are working in Frescobaldi, the line of code with ``\criticalRemark ...``
should be highlighted as well.

**Here should be an image of the resulting output from the example.**

These coloring effects are default features of scholarLY, and they confirm
that the annotation engraver has recognized the annotation and created a list
of *all* the annotations for this score behind-the-scenes.


Syntax
------

We'll go over all the ways that annotations can be rendered within -- and continuing
after -- the engraving process. But first, here's a closer look at the syntax
annotations use. Observe the portion of the excerpt that creates an annotation:

::

  \criticalRemark \with {
    message = "my annotation about the following slur"
  } Slur e(

This is the common interface of the `annotate` module. Here's the syntactic
breakdown of it:

.. table:: scholarLY annotation syntax
   :widths: auto

   ============= ===================== =======================================
    role          example               description
   ============= ===================== =======================================
    `type`        ``\criticalRemark``   invokes the engraver, and announces *type* of annotation
    `properties`  ``\with { ... }``     contains a list of unique properties as key = value pairs
    `item`        ``Slur``              the symbol name for the affected/annotated grob
    `music`       ``e(``                the music to which the annotation is applied
   ============= ===================== =======================================

A few annotation **types** are built into scholarLY as a convenience, but, as
described in the next section, it is just as easy to create new ones. The
minimum **property** to apply is `message`, which is expected to contain a
string. We'll go over other useful properties as they are explained later in the
documentation. **Item** can be anything from a spanner to a `Notehead`, `Stem`
or other note-column objects. If the named item isn't immediately followed by
**music** containing one of it, the document may not successfully compile (this
is actually dependent on what properties, if any, will be applied as music
functions or context settings); typically, you won't encounter such an error,
but it is a reasonable thought to keep in mind when debugging.


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

Let's take the same example from before and make it a footnote:

::

  \criticalRemark \with {
    message = "my annotation about the following slur"

  } Slur e(


Balloon Text
------------

As with footnotes, the minimum argument to print an annotation as ballon text
is the ``balloon-offset`` property.


Exporting Annotations
=====================

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
