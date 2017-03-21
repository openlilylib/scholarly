===================
Editorial Functions
===================


Overview
========

Editorial functions can be used to apply and organize certain critical changes
made to musical scores. They can be implemented concurrently with annotations or
through their own standalone hooks.


Minimum Example
---------------

Editorial functions are applied either through the annotation interface or
independently through its own similar functionality. The following example
does both to the same excerpt in consecutive measures.

.. literalinclude:: ../examples/editorial-functions/min.ly
  :lines: 1-23

The above edition, by default, will make the slur a dashed line instead of the
typical solid line. Comment the line with ``apply = addition`` in the second
example to manually remove this addition and see that difference. This example
also shows that standalone editorial functions are not highlighted like
annotations. In the *Options* section later, we'll see how to actually configure
what effects are applied to specific items per specific types of editions, and
how to toggle them.

.. figure:: ../examples/editorial-functions/min-from-1.0.1-to-3.0.1-clip.png
  :align: center

  Caption

Choosing a method to invoke editorial functions, either standalone or through
annotations, isn't only a matter of taste. One can imagine the need for some
items to be both or either, and this close integration of the two offers a
simple and intuitive environment in which to switch between the two (e.g. you
must apply an edition to an item but also annotate it with a *TODO* note; later,
the *TODO* is resolved, so the annotation is unecessary, thus you'd shave it
down to a standalone edition).


Editorial Functions by Themselves
=================================

For simple editions (not referring to entire *editions* of scores, but rather to
changes of individual score items), this module provides a minimal interface
for applying such changes. This is taken from the first measure of the previous
example:

.. literalinclude:: ../examples/editorial-functions/min.ly
  :lines: 18


Syntax
------

This is effectively a trimmed down version of the annotation interface (most
notably, excluding the ``\with`` block):

.. table:: scholarLY editorial-functions standalone syntax
   :widths: auto

   ========= ======================== =======================================================
    role      example                  description
   ========= ======================== =======================================================
    `type`    ``\editorialAddition``   invokes the function, and announces *type* of edition
    `item`    ``Slur``                 the symbol name for the affected grob
    `music`   ``e'(``                  the music to which the edition is applied
   ========= ======================== =======================================================


Predefined Functions
--------------------

`editorial-functions` comes with three preconfigured commands:

``\editorialAddition item music``
  Triggers an editorial `addition` to the `item` in `music`;

``\editorialDeletion``
  Triggers an editorial `deletion` to the `item` in `music`.

``\editorialEmendation``
  Triggers an editorial `emendation` to the `item` in `music`.

Though they are maybe the most common types of editions one might use, we aren't
limited to only these three.

Also available is the``\editorialFunction`` command which scholarLY uses
internally; the hook is more of a generic invocation, followed by ``type`` and
then the standard arguments. Therefore, the following two accomplish the same
edition:

::

  \editorialAddition Slur e'(
  \editorialFunction #addition Slur e'(

Obviously the above usage of ``\editorialFunction`` is redundant since
``addition`` already maps to a builtin macro, but the availability of that
command means that one could apply any number of unique editions throughout a
score without necessarily assigning them to new hooks.


Defining New Functions
----------------------

As previously alluded, the builtin macros are actually quite simple constructs
which wrap the more complex ``editorialFunction``. Ultimately, what they provide
is a shorthand that cuts out the need for an explicit ``type`` argument when
used.

For example, here is how scholarLY defines the ``\editorialAddition`` macro:

::

  editorialAddition =
    #(define-music-function (item mus)
       (symbol-list? ly:music?)
       (editorialFunction 'addition item mus))

The hook ``editorialAddition`` names the macro, and the first argument to
``editorialFunction`` in the fourth line (in this example, ``'addition``)
determines what edition `type` the macro will apply when used. Note that this
doesn't actually configure what the edition does; that is set later via
options.


Editorial Functions within Annotations
======================================

Annotations can hook all of the editorial functions commands dynamically through
its property interface. We trigger them implicitly by their inclusion in the
context (similar to how `annotate` applies footnotes and balloon text
automatically when either's ``-offset`` property is included).

This is done in the second measure of this chapter's minimal example:

.. literalinclude:: ../examples/editorial-functions/min.ly
  :lines: 23-27


Syntax
------

Since this editorial function is applied as an annotation *property*, it takes
the form of ``key = value``; the ``apply`` property key triggers the edition,
and its value, which must be one recognized by the module, describes the type to
apply. The next section addresses how to actually configure that list.

*Refer to the previous chapter on the* **annotate** *module for a more comprehensive explanation of its syntax.*

Currently, ``apply``-ing a type of edition which isn't paired with the item in
the relevant option (such as ``addition`` with ``slur``, mapping to something
like ``slurDashed``) will do nothing to the music. LilyPond will ignore the
attempted edition as if it weren't invoked, and scholarLY will send a warning to
the console.




The ``\edit`` Shorthand
=======================

The ``\edit`` macro can be hooked to any editorial function as a shorthand. This
is mostly a convenience function to cut down on visual complexity in the
LilyPond document.


Map an editorial function to the ``\edit`` shorthand which will then retain
those settings until changed again using the same method:

.. literalinclude:: ../examples/editorial-functions/shorthand.ly
  :lines: 16-26

.. figure:: ../examples/editorial-functions/shorthand-from-1.0.1-to-5.0.1-clip.png
  :align: center

  Caption


Syntax
------

.. table:: scholarLY editorial-functions shorthand definition syntax
   :widths: auto

   ========= ======================== ========================================================
    role      example                  description
   ========= ======================== ========================================================
    `hook`    ``\editorialShorthand``  invokes the function to update ``\edit``'s mapping
    `type`    ``#'deletion``           the type of edition to apply
    `item`    ``Stem``                 the symbol name for the affected grob
   ========= ======================== ========================================================


.. table:: scholarLY editorial-functions shorthand macro syntax
   :widths: auto

   ========= ======================== ========================================================
    role      example                  description
   ========= ======================== ========================================================
    `macro`   ``\edit``                apply the currently mapped editorial function to music
    `music`   ``d'(``                  the music to which the edition is applied
   ========= ======================== ========================================================


Editorial Sections
==================

With editorial sections, scholarLY can automatically apply functions to all
applicable grobs in a music expression.

::

  \editorialSection #'addition NoteHead {
    c'4 e'
  }

To apply an aditional function to the entire selection `once` (such as a special
parenthesize function, for example), add an optional ``\with`` argument.
In this context mod, the ``function`` key can be set to whatever music function
will be applied to the expression:

.. literalinclude:: ../examples/editorial-functions/sections.ly
  :lines: 19-31

.. figure:: ../examples/editorial-functions/sections-from-1.0.1-to-5.0.1-clip.png

Syntax
------

.. table:: scholarLY editorial-functions section syntax
   :widths: auto

   ============== ======================= =========================================================
    role           example                 description
   ============== ======================= =========================================================
    `hook`         ``\editorialSection``   invokes the function for the pair of `type` and `item`
    `properties`   ``\with { ... }``       contains key = value pairs (expects ``function = ...``)
    `type`         ``#'deletion``          the type of edition to apply
    `item`         ``Stem``                the symbol name for the affected grob
    `music`        ``{ c'4 e' }``          the music expression containing the affected grobs
   ============== ======================= =========================================================


Options
=======

Editorial functions are a somewhat tricky feature for scholarLY to implement due
to the fact that not all *functions* are evaluated in the same way. Some are, in
fact, evaluated as bonified LilyPond music functions, while others simply
trigger modifications to a context. For example, one might want to affect any
notehead of type `addition` with ``\parenthesize`` and any slur of type
`addition` with ``\slurDashed``.

In order to facilitate a common interface that handles all of these operations
in a consistent syntax, we have to require a little bit of extra Scheme in the
options. This means prepending to each of the "functions" (using that word
loosely) a comma. scholarLY will sort and apply them accordingly later.

**scholarly.editorial-functions.addition** `alist`
  description

::

  \setOption scholarly.editorial-functions.addition
    #`((NoteHead . ,parenthesize)
       (Slur . ,slurDashed))

Prepend to each

The next two options are configured similarly.

**scholarly.editorial-functions.deletion** `alist`
  description

**scholarly.editorial-functions.emendation** `alist`
  description






**scholarly.editorial-functions.ignored-types** `list`


And finally, we have available

**scholarly.editorial-functions.apply**

::

  \setOption scholarly.editorial-functions.apply ##t
