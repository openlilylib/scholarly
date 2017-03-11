.. openLilyLib: scholarLY documentation

Welcome to scholarLY's documentation!
=====================================


*scholarLY* is a project of openLilyLib, a collection of community resources
around GNU LilyPond music notation software. It provides a framework for extending
scores engraved by LilyPond with annotations, editorial functions, and a growing
number of other features. See the project on `Github`_ for the latest updates;
follow or contribute to the issue tracker there for feature requests and bug reports.

.. _Github: http://github.com/openlilylib/scholarly

Currently, most openLilyLib projects' documentations are provided in the README's
which can be found on Github in the respective `repositories`_.
Since there currently isn't a manual for installing/using openLilyLib, this
version of scholarly's documentation includes a preamble *introduction to openLilyLib*
which explains the current state/functionality of OLL in general and, more importantly,
how to setup OLL on your system.

.. _repositories: http://github.com/openlilylib/scholarly


.. toctree::
   :maxdepth: 4
   :caption: Contents:

   OpenLilyLib Primer <oll-preamble>
   Introduction <introduction>
   Installation <installation>
   ScholarLY modules <modules>
   LaTeX package <latex>
   Contributing <contributing>



About this documentation
========================

This documentation is built using `Sphinx`_, which makes
HTML (mobile-friendly), PDF, and ePub versions from the same source.
To build it yourself, install Sphinx and ``cd`` into scholarly/doc, then
``make builder`` where `builder` is one of ``html``, ``latexpdf``, or ``epub``. Other
options (which haven't been tested with this documentation) are listed in
the Sphinx docs.

Currently, the LilyPond lexer for Pygments, which provides syntax
highlighting for LilyPond code blocks used by the Sphinx builder,
isn't packaged with Pygments itself.
You'll need to `download`_ and install it manually in order for Sphinx to
properly highlight the lilypond code blocks.

.. _sphinx: www.sphinx-doc.org
.. _download: https://github.com/jefferyshivers/lilypondlexer
