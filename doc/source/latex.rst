===========================
The scholarLY LaTeX Package
===========================

scholarLY's robust LaTeX package extends the ability to create beautiful
editions beyond the LilyPond environment. While HTML and other output methods
provide interfaces for more dynamic, paperless paradigms, the LaTeX package is
concerned specifically with the goal of typesettings annotations to beautifully
formatted, printable documents.

**example image here...**

The package includes its own comprehensive documentation which should be
referenced for specific usage instructions. Therefore, this chapter of the
current manual  will mostly serve the purpose of bridging the gap betweeen the
LilyPond and LaTeX workflows. Assuming that some users are new to LilyPond
and/or LaTeX, this chapter includes minimal copy-and-pastable code examples for
quick demos and to hopefully encourage to the .. of deeper investigation and
practice of the quite extensive possibilities for this .. of scholarLY.

**New to LaTeX?**

As a general description, LaTeX documents are compiled (from the user's
standpoint) with nearly the same approach as LilyPond projects. LaTeX doesn't
require a standard GUI, but there are several ones available that may be
especially useful to  newcomers. A few of the more popular ones are

- one
- two
- three

LaTeX's main website includes a simple introduction to the program's grammer and
core features. There are numerous other resources across the web which provide
beginner's tutorials and example projects. It isn't absolutely necessary to
completely familiarize yourself with LaTeX before diving into it with scholarLY,
but at least some amount of prior introduction will make the integration a
little less confusing or abstract, so it is highly recommended that you do so.


Installation
============

Tex Live
--------

The LaTeX package is published to CTAN and will be most easily acquired
automatically through your unique TeX distribution. You can check if the package
is already on your system by searching the list (according to whichever distro
you have); if it is not, you may simply need to run an update. You can also
download it directory from CTAN, in which case you'll have to first include the
path to the package file `scholarly.sty` in that that directory.

From Source
-----------

The package can also be manually installed from source. You can download and
navigate into the package directory from CTAN to rebuild it. Or, the recommended
way, navigate into the folder `scholarly/latex-package` which is included with
the repository via git (or manually download from Github). Run the following:

.. code-block:: console

  latex scholarly.ins

Which will extract the package source from `scholarly.dtx` and place it into a
new file `scholarly.sty`. Include the path to this new file at the top of your
LaTeX document before invoking the package (which is demonstrated in the
Minimum Example section).


Building and Viewing the Documentation
======================================

If you have installed the package via a maintained TeX distribution, its
documentation can be opened by using the command `texdoc scholarly` anywhere on
your system. This will open the compiled documentation in your default PDF
viewer.

Alternatively, you can build the documentation yourself from source. As before,
navigate into `scholarly/latex-package`. This time, do the following (assuming
you have `latex` / `pdflatex` access through the command line) to build and open
the PDF:

.. code-block:: console

  $ pdflatex scholarly.dtx
  $ pdflatex scholarly.dtx
  $ open scholarly.pdf

We have to pass `scholarly.dtx` through `pdflatex` twice in order to generate
and then apply the table of contents. This generates the official documentation
for the package with the most in-depth explanations of its functionality in the
current release.

Updating TeXLive will automatically update the documentation pulled up with
``texdoc <package-name>``, but


Examples
========

Minimum Example
---------------

The basic usage of scholarLY's LaTeX features include engraving an annotated
score with LilyPond, then processing the exported annotations list with LaTeX.

If we compile the following example with LilyPond, making sure to include the
setting to export annotations configured for LaTeX, LilyPond will engrave the
music as a single-page score as well as the ``.inp`` file.

.. literalinclude:: ../examples/latex/min-lp.ly
  :name: ex. 10
  :lines: 1-26

We process that list in LaTeX by invoking the package and using the global
`\annotations` command with the ``{<path/to/filename>}`` argument.

.. literalinclude:: ../examples/latex/min-lx.tex
  :name: ex. 11
  :lines: 1-8
  :language: latex

The resulting document will contain the single annotation formatted similar to
the following:

**image cropped of the latex result here**


Combining Scores and Annotations
--------------------------------

Many LilyPond users already use LaTeX for combining multiple scores together,
and we can do the same to combine the score and annotations into a single
document. After compiling LilyPond and creating the PDF of the score and the
``.inp`` annotations file, adapt the previous LaTeX example to include the printed
score in front of the annotations:

.. code-block:: latex

  \documentclass{article}

  \usepackage{scholarly}

  \begin{document}

  \annotations{some-example-anns.inp}

  \end{document}

One can imagine extending this final document to include a title page, maybe a
table of contents, a forward, appendices and so on. We'll include a few of those
in the next few examples to demonstrate their applicaiton in context. Some of
those resulting documents are included in the appendix as well and accordingly
referenced.


Complete Documents with ``lilypond-book``
-----------------------------------------

More options for combining LilyPond with LaTeX are available through the
``lilypond-book`` program, which also natively supports output to HTML. scholarLY
hasn't yet been tested with HTML output using this command, but more info on the
general ``lilypond-book`` protocol is available here and would be the most useful
reference for integrating that and other options. The following exemplifies a
simple scholarLY document built with ``lilypond-book``, .


.. code-block:: latex

  \documentclass[a4paper]{article}

  % make sure latex finds this:
  \usepackage{scholarly}

  \begin{document}

  \section{Preface}

  This is a section of the document called ``Preface''. In this section, we
  include an inline music example:

  \begin{lilypond}
  \relative {
  c'2 e2 \tuplet 3/2 { f8 a b } a2 e4
  }
  \end{lilypond}

  Environments in \LaTeX{} are bookended by begin and end hooks. Anything within
  a lilypond environment will be compiled by LilyPond then added to the LaTeX
  document as an image.

  As the online reference for lilypond-book shows, options are put in brackets:

  \begin{lilypond}[fragment,quote,staffsize=26,verbatim]
  c'4 f16
  \end{lilypond}

  Those options are explained in that documentation, so we won't get into them
  here. But essentially they are how you will handle certain

  Larger examples can be put into a separate file, and introduced with
  \verb+\lilypondfile+.

  \lilypondfile[quote,noindent]{lp-book.ly}

  The file lp-book is an external LilyPond source document; you can find it in
  scholarly/doc/examples/latex.

  \end{document}


Formatting Annotations
----------------------

The
