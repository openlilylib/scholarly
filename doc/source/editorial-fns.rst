===================
Editorial Functions
===================

Todo


Overview
========

Explanation of what they are..


Minimum Example
---------------

Editorial functions are applied either through the annotation interface or
independently through its own similar functionality. The following example
does both to the same excerpt.

::

  \code

Choosing a method to invoke editorial functions, either alone or through
annotations, isn't only a matter of taste (though it certainly could be). One
can imagine shifting  the uncertainty of what deserves an annotation, or what
only needs a more straightfoward  editorial function to toggle on/off, and the


Editorial Functions by Themselves
=================================

Explain how to do them on their own


Syntax
------

Syntax Chart


Editorial Functions within Annotations
======================================

Annotations can hook all of the editorial functions commands dynamically through
its property interface. We trigger them

From within the ``\with`` block, apply an editorial function

::

  \criticalRemark \with


Syntax
------

The syntax here is relatively simple. Since the editorial function is applied
as an annotation *property*, it takes the form of ``key = value``.

Syntax table here...

This embedded functionality is one of the stronger cases for why *editorial
functions* and *annotate* are both members of the scholarLY package.


Options
=======

Editorial functions are inherantly a complex feature for scholarLY to implement.
This is mostly due to the fact that not all *functions* are really functions at
all. Some are, in fact, evaluated as bonified LilyPond music functions, while
others simply trigger changes within some context. For example,

these two are totally different...

In order to facilitate a common interface that handles all of these .. in the
same guise, we have to require a little bit of Scheme trickery in the options.
Here's a literal copy of the default editorial functions (a rather short list,
but one meant to be extended):

::

  \setOption scholarly.check.this.path
  #`((NoteHead . ,parenthesize)
     (Slur . ,slurDashed)
     (etc ... ))

And so on...
