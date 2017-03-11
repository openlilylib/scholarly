=================
scholarLY modules
=================

openLilyLib provides the ability to invoke particular modules needed for each
project without imposing the inclusion of *all* available modules. After loading
`oll-core`, list unique modules like so:

.. code-block:: latex

    \loadPackage \with {
      modules = annotate
    } scholarly 

The currently available modules are `annotate` and `editorial-functions`.
The `annotate` modules implicitly loads `editorial-functions`, so there is no
need to specify both.

Since `editorial-functions`' functionality is built into `annotate`, we'll explain
the former first before diving into all that the latter provides.


Editorial Functions
===================

TODO


Options
-------

TODO



Annotate
========

TODO


Options
--------

TODO
