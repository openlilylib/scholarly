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
  :name: ex. 1
  :lines: 1-23

Compiling this example will print the music ``c4 e( d) d`` mostly as expected,
but with the annotated grob (in this case, a slur) printed in a color other than
the default black. If you are working in Frescobaldi, the line of code with
``\criticalRemark ...`` should be highlighted as well.

.. figure:: ../examples/annotate/min-from-1.0.1-to-2.0.1-clip.png
  :name: bla
  :align: center

  Output of :ref:`ex. 1 <ex. 1>`

These coloring effects are default features of scholarLY, and they confirm
that the annotation engraver has recognized the annotation and created an
internal list of annotations for this score.


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
-----------------------

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
  :name: ex. 2
  :lines: 9-17

.. figure:: ../examples/annotate/footnotes-01.png
  :align: center

  Output of :ref:`ex. 2 <ex. 2>`

The ``footnote-text`` property may be included to indicate text different than
the annotation message.

.. literalinclude:: ../examples/annotate/footnotes-02.ly
  :name: ex. 3
  :lines: 9-18

.. figure:: ../examples/annotate/footnotes-02.png
  :align: center

  Output of :ref:`ex. 3 <ex. 3>`


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
  :name: ex. 4
  :lines: 9-17

.. figure:: ../examples/annotate/balloon-text-01-from-1.0.1-to-2.0.1-clip.png
  :align: center

  Output of :ref:`ex. 4 <ex. 4>`


And with a unique message for the balloon text:

.. literalinclude:: ../examples/annotate/balloon-text-02.ly
  :name: ex. 5
  :lines: 9-18

.. figure:: ../examples/annotate/balloon-text-02-from-1.0.1-to-2.0.1-clip.png
  :align: center

  Output of :ref:`ex. 5 <ex. 5>`

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
Another `known issue`_ is the inability to use footnotes within balloon text.
Typically, markups in LilyPond can apply footnotes using the alternative
``\auto-footnote`` command. But, as markups within music are technically
different functions than those outside of music, the functionality isn't
compatible and unfortunately hasn't been adapted yet.

.. _known issue: https://sourceforge.net/p/testlilyissues/issues/2819/


General Options
===============

These are global options which aren't necessarily dependent on or related to a
single feature of the `annotate` module. Continue to `Exporting Annotations` to
see the remaining options currently available.


Print to Console
----------------

**scholarly.annotate.print** `boolean`
  Print list to console using plaintext export's format. Default is ``#t``.

::

  \setOption scholarly.annotate.print ##t

Ignored Types
-------------

**scholarly.annotate.ignored-types** `list`
  `Don't` annotation these types. Default is ``()`` (empty).

::

  \setOption scholarly.annotate.ignored-types
    #'(todo question)



Sort Annotations
----------------

**scholarly.annotate.sort-criteria** `list`
  Sort list by each criteria in order. Available sort criteria is ``rhythmic-location``,
  (chronological) ``type`` (alphabetical), and ``author`` (alphabetical).
  Default is ``(rhythmic-location)``.

::

  \setOption scholarly.annotate.sort-criteria
    #'(rhythmic-location type)


Color Annotations
-----------------

**scholarly.colorize** `boolean`
  Globally toggle all coloring options. Default is ``#t``.

::

  \setOption scholarly.colorize ##t

**scholarly.annotate.colors** `association list`
  Assign colors to annotated grobs by type. Default is as the following example:

::

  \setOption scholarly.annotate.colors
    #`((critical-remark . ,darkgreen)
       (musical-issue . ,green)
       (lilypond-issue . ,red)
       (question . ,blue)
       (todo . ,magenta))


Exporting Annotations
=====================

scholarLY annotations can be used well beyond the score through its numberous
export options. The currently available output methods are plaintext, LaTeX and
HTML (with highly configurable CSS generation/options). We plan to extend that
list to include at least JSON, markdown and Scheme.

scholarLY doesn't automatically convert macros between languages, so keep that
in mind if you plan on switching between various outputs. It isn't necessarily
problematic to have stuff from one language (such as LaTeX) show up in another
(such as HTML), but be prepared for some uninterpreted macros to be retained if
something like that becomes part of your workflow.


Configuration
-------------

**scholarly.annotate.export-targets** `list`
  Methods through which to export annotations. Default is ``()`` (empty).

::

  \setOption scholarly.annotate.export-targets
    #'(plaintext latex html)


**scholarly.annotate.export.filenames** `list`
  Filenames for each export routine. Default is as in the following example:

::

  \setOption
    #`((html . "index.html")
       (latex . default)
       (plaintext . default))

Default for filenames are ``<doc>.annotations.<ext>`` where `doc` is the name of
the lilypond document and `ext` is the extension associated with that type.
Default extensions are `.html` for HTML, `.tex` for LaTeX, and `.log` for
plaintext.

For clarification, scholarLY sets the HTML default as ``index.html`` as a
convenience during its option registration; you can override that and apply the
`actual` default (``*.annotations.html``) by setting it to ``default``.

This option is currently waiting to be pulled with the export-html functionality.
Note that CSS names are handled differently as HTML-specific options.


Plaintext
---------

Plaintext output goes to the console as well as a generated ``*.log`` file.
Annotations are nicely formatted and  printed in order from first to last.
The following option determines the printed label for each annotation type:

**scholarly.annotate.export.plaintext.labels** `association list`
  Specify what labels to print for each type of annotation. Default is as in the following example:

::

  \setOption scholarly.annotate.export.plaintext.labels
    #`((critical-remark . "Critical Remark:")
       (musical-issue . "Musical Issue:")
       (lilypond-issue . "LilyPond Issue:")
       (question . "Question:")
       (todo . "TODO:")
       (my-custom-type . "Unique Problem:"))

**scholarly.annotate.property-labels** `association list`
  Configure the rest of the property labels. Default is as in the following example:

::

  \setOption scholarly.annotate.property-labels
    #`((message . "Message")
      (author . "Author(s)")
      (context-id . "Context")
      (source . "Affected Source")
      (voice-name . "Voice")
      (segment-name . "File")
      (ann-footnote . "Annotation Footnote")
      (grob-type . "Affected Item"))


LaTeX
-----

scholarLY has the ability to export annotations to a LaTeX-readable list. This
generates a ``*.inp`` file which the scholarLY LaTeX package will turn into a
well formatted document. The settings of that package are already highly
customizable and extendable, and even more flexibility is planned following the
first official release.

Many LilyPond users already employ LaTeX for combining multiple score documents
together after the LilyPond engraving has been done. scholarLY fits comfortable
within that process; by simply loading the scholarLY LaTeX package and adding
the minimal ``\annotations{<*.inp>}`` hook, annotations are printed with the
document in a beautifully typeset list.

That's all to say: this is one of the most robust features of scholarLY, and so
it is well worth consieration as an addition to your workflow, even if only for
intermediate annotations lists. The next chaper `LaTeX Package` goes into fuller
detail on how to configure annotations in LaTeX after exporting them, and the
official `scholarLY LaTeX package` documentation provides the comprehensive
overview of that functionality.


LaTeX macros in the message
^^^^^^^^^^^^^^^^^^^^^^^^^^^

This means that we can use any LaTeX code within the message iself. Scheme uses
the backslash (``\``) as an escape character, so sometimes you'll need to
actually use `two` where in LaTeX you'd normally use just one. Hooks beginning
with the letter `t` are a good example.

::

  \criticalRemark \with {
    message = "my \\textit{annotation} about the following {\color{red} slur}"
  } Slur e'(


LaTeX Footnotes
^^^^^^^^^^^^^^^

Earlier in the section about in-score annotations as footnotes, we saw how the
annotation messages or unique footnote text could be turned into footnotes
recognized and engraved by LilyPond. But we may also need to apply footnotes to
the text `within` the annotation message (and not to the grob printed in the
actual music) to appear in an endnote section with the annotations.

Insert the footnote within the message as you typically would in LaTeX with its
builtin ``\footnote`` hook:

::

  \criticalRemark \with {
    message = "my annotation\footnote{Here's a LaTeX footnote!} about the following slur"
  } Slur e'(

Alternatively, apply a footnote to the entire message, in which case the LaTeX
package will ensure that the superscript comes after the final punctuation or
other characters surrounding or following the message (those are things you'll
actually be able to configure in the LaTeX document later):

::

  \criticalRemark \with {
    message = "my annotation about the following slur"
    ann-footnote = "my (LaTeX) footnote for the entire annotation"
  } Slur e'(

In that case, there is no need to place a macro within the message string; that
is handled automatically later on the LaTeX side.


Options
^^^^^^^

**scholarly.annotate.export.latex.use-lilyglyphs** `boolean`
  If set the grob location is formatted as a lilyglyphs command. Default is ``#f``.

::

  \setOption scholarly.annotate.export.latex.use-lilyglyphs ##f

Additional options, including extensive styling and formatting utilities, are
provided later by the LaTeX package.


HTML
----


This section is pending with the pull request which will implement html export.

**scholarly.annotate.export.html.labels** `alist`

::

  % Annotation types for html text output
  \setOption scholarly.annotate.export.html.labels
  #`((critical-remark . "Critical Remark")
    (musical-issue . "Musical Issue")
    (lilypond-issue . "Lilypond Issue")
    (question . "Question")
    (todo . "TODO"))

**scholarly.annotate.export.html.full-document** `boolean`

::

  % Print full document with header (including CSS link) and body, or just
  % annotations div
  \setOption scholarly.annotate.export.html.full-document ##t


For each annotation list exported, the division of items, from outermost to
innermost, is



**scholarly.annotate.export.html.divs** `alist`

::

  % Annotation div types (can technically be anything, since they get directly
  % converted to string; so even a new type of div, or `a`, or whatever else.)
  \setOption scholarly.annotate.export.html.divs
  #`((full-ann-list . ol)
     (each-ann-outer . li)
     (each-ann-inner . ul)
     (each-ann-props . li))

**scholarly.annotate.export.html.annotations-div-tags** `alist`
  Change the tags for the annotations list div.

::

  \setOption scholarly.annotate.export.html.annotations-div-tags
     #`((class . "my-annotations")
        (id . #f))

**scholarly.annotate.export.html.props** `list`
  Which properties to print to html, and in what order. Default is as follows:

::

  \setOption scholarly.annotate.export.html.props
    #`(type grob-location grob-type message)

**scholarly.annotate.export.html.prop-labels** `alist`
  Which labels to print for props (only affects props included in the
  `export.html.props` list). Default is as follows.

::

  \setOption scholarly.annotate.export.html.prop-labels
    #`((type . "<em>Type:</em> ")
       (grob-location . #f)
       (grob-type . #f)
       (message . #f))


Working with CSS
^^^^^^^^^^^^^^^^

Like the output filenames option, you can of course specific files in other
directories in the following options. Note that the directories must already
exist, even for generated files.

**scholarly.annotate.export.html.external-css-name** `string`
  What is the filename of any external file you provide. Note that scholarLY
  currently doesn't read this and will only link to it if requested. Default
  is `#f`.

::

  \setOption scholarly.annotate.export.html.external-css-name
    #"my-external-styles.css"

**scholarly.annotate.export.html.generate-css-name** `string`
  What is the filename of the generated file. Default is an arbitrary suggestion
  "my-generated-styles.css".

::

  \setOption scholarly.annotate.export.html.generate-css-name
    #"my-generated-styles.css"

The following two have a subtle naming difference, but together they determine
the exact CSS behavior scholarLY will implement. Not every combination is
possible (such as loading and printing an external file to the header (yet) or
inline).

**scholarly.annotate.export.html.with-css** `symbol`
  How to route/reference CSS. Default is ``linked``. Applicable settings:

  - *inline*: embed css inline (not yet implemented)
  - *header*: print css in header
  - *linked*: link to file in header

::

  \setOption scholarly.annotate.export.html.with-css
    #`linked

**scholarly.annotate.export.html.use-css** `symbol`
  Which CSS to use. Default is ``default``. Applicable settings:

  - *default* the default CSS provided by the repository
  - *generate* a generated CSS
  - *external* external stylesheet

::

  \setOption scholarly.annotate.export.html.use-css
    #`default

The valid combinations of the previous two options are:

  - `with-css` ``inline`` - `use-css` ``default`` or ``generate``

  - `with-css` ``header`` - `use-css` ``default`` or ``generate``

  - `with-css` ``linked`` - `use-css` ``external`` or ``generate``

Designing the Generated CSS
^^^^^^^^^^^^^^^^^^^^^^^^^^^

**scholarly.annotate.export.html.generate-css-settings** `alist`

For generated CSS, we create styles in a sort of cascading `alist`.

The left hand argument or `key` of each pair, such as ``class`` and ``id`` in
the example below, describes what type of tag the enclosed elements should have.
This basically prepends a ``.`` or ``#`` in front of the element's name for
``class`` and ``id``,  respectively, and any other type will be printed without
such an alteration.

The right hand argument is a list containing one or more pairs. The `key` of each
pair is the name, as mentioned before, and the value is another list containing
one or more strings and/or one or more `additional` lists of pairs. For those
additional lists, the `key` is again the name of the element, and the `value` is
a string or a list of one or more strings.

The characters ``_`` and ``-`` in any `key` name will be replaced with spaces.
This means that the following two settings are equivalent:

::

  \setOption scholarly.annotate.export.html.generate-css-settings
    #`((class . ((annotations . (ul_li ("background: gray"))))))

  % AND

  \setOption scholarly.annotate.export.html.generate-css-settings
    #`((class . ((annotations_ul_li . ("background: gray")))))

Those examples would generate the following CSS:

.. code-block:: css

  .annotations ul li {
    background: gray;
  }

  .annotations {
  } ul li {
    background: gray;
  }

You'll notice that the two CSS examples don't exactly get formatted the same
way in the end. This is because scholarLY groups annotations within the same
`class`, `id` or other top level pair together, and the second example was
list in such a way that it noticed a hierarchical difference between ``annotations``
and ``ul_li``.

Here's another simple example extending the previous one which illustrates this
difference within a more complex setting:

::

  \setOption scholarly.annotate.export.html.generate-css-settings
    #`((class . ((myannotations . ("background: gray"
                                    "margin: 0.5em"))
                 (anotherclass . ("color: blue"
                                  (ul . "list-style-type: none")
                                  (ul_li . "margin: 0.25em"))))))

Which gets printed to:

.. code-block:: css

  .myannotations {
    background: gray;
    margin: 0.5em;
  }

  .anotherclass {
    color: blue;
  } ul {
    list-style-type: none;
  } ul li {
    margin: 0.25em;
  }

And, finally, another excerpt showing the full scope of this hierarchy with
multiple div types and elements.:

::

  \setOption scholarly.annotate.export.html.generate-css-settings
    #`((class . ((full-ann-list . ("background: gray"
                                  "margin: 0.5em"
                                  "line-height: 1.2"
                                 (ul . "list-style-type: none")))
                 (annotation . ((ul . "background: lightgray")
                                (ul_li . "margin: 0.25em")))
                 (todo . ("background: red"))
                 (question . ("background: green"))))
       (id . ((my-unique-annotations-list . ("foo: bar")))))

In a header or generated file, the above would be rendered to:

.. code-block:: css

  .annotations {
    background: gray;
    margin: 0.5em;
    line-height: 1.2;
  } ul {
    list-style-type: none;
  }

  .annotation {
  } ul {
    background: lightgray;
  } ul li {
    margin: 0.25em;
  }

  .todo {
    background: red;
  }

  .question {
    background: green;
  }

  #my-unique-annotations-list {
    foo: bar;
  }

Other things to note are that semicolons are automatically appended to each
string, and that the `class`, `id` and other `key` types can each be used
multiple times within the setting without fault. The latter point would be
useful for trying out CSS settings before comitting to the task of embedding
them more concisely within a minimal alist.

The names `full-ann-list` is r

scholarLY prints the CSS in a nicely indented, readable format. Inline CSS isn't
yet implemented, but once it is the same option for setting the generated CSS
will be automatically compatible with that option.
