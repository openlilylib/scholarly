==================
OpenLilyLib Primer
==================

Besides hosting a growing list of unique projects, openLilyLib (OLL) provides a
robust infrastructure for building new libraries/frameworks from the ground up.
Its most important and ubiquitous features are the package and option handling
systems. Though this is not a comprehensive guide to all of OLL,
we will go over every feature that is at least relevant to using scholarLY. More information and an updated, unified documentation of all of openLilyLib (including all of its child projects') is
forthcoming.


Introduction
============

The best way to appreciate what OLL can do is to see it in action. Most of its
packages include examples (typically in a subdirectory called `usage-examples`
or similar). With the packages installed (following the instructions in this guide),
you can compile any of the examples to see the functionalities in the context of
a complete LilyPond document.

Here is a brief list of the currently available packages



.. note! If this is made into the OLL manual, we can intersphinx into each of
  the other documentations. Or otherwise, link to those subsections (though in
  that case it might be redundant to the TOC..)



Installation
============

Get `oll-core` from github.com/openlilylib/oll-core.
The recommended method for doing this is to clone the repository with git
so that you can pull updates as the project develops over time.
Otherwise, your can directly download it from Github as well.




Using git
---------

If this is your first encounter with git, here's how to use it in our case.

Install git if it isn't already on your system, then, on your preferred command
line interface, create and ``cd`` into a new directory. We'll make one called "oll-core":

.. code-block:: console

  $ mkdir oll-core
  $ cd oll-core

Now, clone the repository, referencing it with the Github URL:

.. code-block:: console

  $ git clone https://github.com/openlilylib/oll-core

This will install the repository into our new directory. To be sure it worked,
we should be able to see a list of the top-level subdirectories and files:

.. code-block:: console

  $ ls
  README.md          oll-core.ily                   usage-examples
  alist-example.ly   package.ily                    util
  include-pattern    scheme
  internal           temp-package-declaration.ily

Check the project on Github to find out about new updates and upcoming features.
To synchronize your clone of the repository with the latest release, you can ``pull`` any changes
into this local directory without having to redownload the entire project.

To do this, make sure you are in the directory as before, and also in the master
git branch. This will almost certainly be the case, and is by default,
but sometimes mistakes happen [#f1]_, so here's how to double-check:

.. code-block:: console

  $ git branch
  some-other-branch
  * master


The asterisk (and probably some text coloring as well) shows that we are in
fact on the master branch. If that wasn't the case, the way to maneuver back to
it would be:

.. code-block:: console

  $ git branch
  * some-other-branch
  master
  $ git checkout master
  $ git branch
  some-other-branch
  * master


Now we can pull in any new changes from the remote repository by simply:

.. code-block:: console

  $ git pull




Git will update everything in the directory if there are any changes, otherwise
it will report that your local repository is current with the remote one.

These instructions are the same for each child project of OLL. If you experience
any compatibility issues, please get in touch with the package maintainers. Every
package and module is tested with the latest release of `oll-core`, so be sure
that your copy for `all` package directories are up-to-date before posting to
the issue tracker.




.. rubric:: Footnotes

.. [#f1] If you aren't developing stuff in the repository yourself, you should only ever need to be in the master branch, so be sure that is always the case. If you do want to experiment on a new feature, you can always make a new (local) branch yourself and go back to the stable master branch when you need to use it.



Using lyp
---------

Another option is to use `lyp`, a third-party package management system for
LilyPond. `lyp` may not include all OLL packages (such as scholarLY -- stay tuned),
but it is worth considering as an alternative to git if you are hesitant about the
git protocol. It is installed as a Ruby gem, and provides pretty simple and
convenient commands for installing and updating the packages on its list.

`lyp` is well-documented, so refer to its `website`_ for the (relatively
simple) instructions on how to install and use it.

.. _website: http://lyp.noteflakes.com/#/


Basic Usage and Configuration
=============================

Any subproject of openLilyLib must first load 'oll-core' at the top of the document.

.. code-block:: lilypond

  % mandatory invocation of openLilyLib's core infrastructure:
  \include "oll-core/package.ily"

If you are using LilyPond from the command line, make sure it is configured
to include the path to `oll-core`. In Frescobaldi, this is done in `Frescobaldi > Preferences > LilyPond Preferences > "LilyPond include paths:"`.

This is the minimum requirement for the `oll-core` utilities, and it
`must precede any code in the project which uses it`, including the
loading of packages themselves.



Loading Packages and Modules
----------------------------

OLL libraries are maintained as packages of modules. scholarLY, for example,
currently has two available modules: annotate and editorial functions.
We load them like so:

.. code-block:: lilypond

  \loadPackage \with {
    modules = annotate
  } scholarly

And that's it! Now you should see a confirmation of any loaded packages at
the top of the output log each time you compile your document.




Option Handling
----------------

Some OLL libraries come with a set of options which can be configured using
OLL's global option handling system. Regardless of the specific details of
each option, the same generic command, ``\setOption``, is available as a standard hook, both
as a means of convenience and as a way to avoid naming collisions between
packages (through its robust tree system).

Here is an example of an option that only takes a single boolean.

.. code-block:: lilypond

  \setOption scholarly.colorize ##f

This option tells the `scholarLY` package to turn coloring, for any grobs that
acknowledge that option, to false. The following example takes a more complex argument:

.. code-block:: lilypond

  \setOption scholarly.annotate.colors
    #`((critical-remark . ,darkgreen) % Notice the usage of "`" and ","
       (musical-issue . ,green)
       (lilypond-issue . ,green)
       (question . ,blue)
       (todo . ,red))

This option is an association list which describes what colors to apply to grobs
of a certain `annotation` type. Notice that the option includes the ``annotate``
branch which wasn't a part of the previous one we observed. In this case, that
means that the colors set here are only specifically applied to grobs that are
affected by the annotate module, while the `scholarly.colorize` option may
toggle all grobs under the `scholarly` umbrella.

As pointed out in the comment in the above example, the punctuations
(the "backquote" or "quasiquote" before the list, and "unquotes" of each of the
color names) help us to manage the symbols as they are being sorted into and out
of the options tree. This is a common gotcha, particularly where arguments in a
list are intended to evaluate to `music` or `scheme` functions.


Contributing
=================

As an open source community built around developing state of the art tools for
beautiful, high quality musical documents, openLilyLib welcomes new contributors
and new project ideas. Eventually, a contributor's guide will be included in
OLL's official documentation. For more details at this time, please contact
us either through the relevant issue tracker or the official email (which is
listed at the organization's own `homepage`_ and on `Github`_).

.. _homepage: https://openlilylib.org
.. _Github: https://github.com/openlilylib
