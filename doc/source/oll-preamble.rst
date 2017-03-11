==================
OpenLilyLib Primer
==================

Besides hosting a growing list of unique projects, openLilyLib (OLL) provides a
robust infrastructure for building new libraries/frameworks from the ground up.
Its most important and ubiquitous features are the package and option handling
systems. Since this is only a brief (and unofficial [#f1]_ ) introduction to OLL,
we'll focus mostly on these main features. More information and an updated, unified
documentation of all of openLilyLib (including all of its child projects') is
forthcoming.


.. rubric:: Footnotes

.. [#f1] In the not-too-distant future, a single manual (ideally available in web, pdf, and ePub formats, all downloadable and buildable from source as an .. part of the system.) will be a standard and integral part of the project.



Installation
===================

Get OLL-core from github.com/openlilylib/oll-core.
The recommended method for doing this is to clone the repository with git
so that you can pull updates as the project develops over time.
Otherwise, your can directly download it from Github as well.




Using git
---------

If this is your first encounter with git, here's how to use it in our case.

Install git if it isn't already on your system, then, on your preferred command
line interface, make and cd into a new directory. We'll make one called "oll-core":

.. code-block:: bash

  $ mkdir oll-core
  $ cd oll-core

Now, clone the repository, referencing it with the Github URL:

.. code-block:: bash

  $ git clone https://github.com/openlilylib/oll-core

This will install the repository into our new directory. To be sure it worked,
we should be able to see a list of the top-level subdirectories and files:

.. code-block:: bash

  $ ls
  README.md          oll-core.ily                   usage-examples
  alist-example.ly   package.ily                    util
  include-pattern    scheme
  internal           temp-package-declaration.ily

Check the repository to see the status of the repository and if any recent updates
have been made. Most importantly, pull the repository the update your local
directory without having to redownload the entire project.

Make sure you are in the master branch. This will almost certainly be the case,
but sometimes mistakes happen (especially when we're curious), so let's double-check:

.. code-block:: bash

  $ git branch
  some-other-branch
  * master


Good! If the asterisk showed we were in a different branch, all we should do is:

.. code-block:: bash

  $ git branch
  * some-other-branch
  master
  $ git checkout master
  $ git branch
  some-other-branch
  * master


Now we can pull in any new changes, by simply:

.. code-block:: bash

  $ git pull


If you aren't developing stuff in the repository yourself, you should only ever
new to be in the master branch, so be sure that is always the case. If you do want
to experiment on a new feature, you can always make a new (local) branch yourself
and go back to the stable master branch when you need to use it.

These instructions are the same for each child project of OLL. For more details
on contributing to any of them (or a new one), get in contact with us either
through the respective issue trackers or the official email (which is listed at the organization's homepage).



Basic Usage and Configuration
=============================

Any project that requires an openLilyLib module must load it at the top of the
document.

.. code-block:: lilypond

  % mandatory invocation of openLilyLib:
  \include "oll-core/package.ily"

If you are invoking LilyPond from the command line, make sure you have configured
it to include the path to oll-core. In Frescobaldi, this is done in Frescobaldi > Preferences > LilyPond Preferences > "LilyPond include paths:".





Loading Packages and Modules
----------------------------

OLL libraries are maintained as packages of modules. scholarLY, for example,
is a library with two available modules: annotate and editorial functions.
We load them like so:

.. code-block:: lilypond

  \loadPackage \with {
    % modules = annotate <<-| TODO: add lilypondlexer kev = val tokens
  } % scholarly          <<-|

And that's it! Now should should see a confirmation of the specific package (and its
version) at the top of the output log each time you compile a document with a
package.




Option Handling
_______________

Some OLL libraries come with a set of options which can be configured using
OLL's globals option handling system. Regardless of the specific details of
each option, the same command, \setOption, is available as a standard hook, both
as a means of conveinience and as a way of avoiding naming collisions between
packages.


.. code-block:: lilypond

  % TODO
