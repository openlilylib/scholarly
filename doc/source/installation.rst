============
Installation
============

If you have read through the openLilyLib primer, this section will be mostly
redundant.


Getting scholarLY
=================

The recommended method of acquiring scholarLY is through git. The project is
maintained at and should be cloned from
https://github.com/openlilylib/scholarly. The best practice is to put *oll-core*
and *scholarLY* (and any other OLL packages) in the same parent directory.
Wherever scholarLY is installed, be sure that LilyPond can find it on your
system.


Loading the Package
===================

OLL provides the ability to invoke particular modules needed for each project
without imposing the inclusion of *all* available modules. After loading
`oll-core`, load scholarly with optional modules like so:

::

    \loadPackage \with {
      modules = annotate
    } scholarly % notice the lowercase name here

**The currently available modules are `annotate` and `editorial-functions`.**
The `annotate` module implicitly loads `editorial-functions`, so there is no
need to specify both. If it were necessary, the value of ``modules`` above would
be set like ``modules = #'(annotate editorial-functions)``.

Alternatively, load a specific module with the ``\loadModule`` command. The
following accomplishes the same as the previous code example:

::

  % just load `annotate`
  \loadModule scholarly.annotate


Or load the package without any modules:

::

  \loadPackage scholarly
