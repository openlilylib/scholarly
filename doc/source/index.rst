Welcome to scholarLY's documentation!
=====================================

.. include:: introduction.rst


----


.. toctree::
   :maxdepth: 4
   :caption: Contents

   openLilyLib <oll-preamble>
   Installation <installation>
   Annotate <annotate>
   Editorial Functions <editorial-fns>
   The scholarLY LaTeX package <latex>
   Contribute to scholarLY <contributing>
   Appendix <appendix>





About this documentation
------------------------

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

.. _Sphinx: http://www.sphinx-doc.org
.. _download: https://github.com/jefferyshivers/lilypondlexer
