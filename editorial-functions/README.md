# scholarLY: Editorial Functions

Editorial functions can be used to indicate certain critical changes
made to musical scores. They can be applied concurrently with
annotations, or standalone through hooks provided by the `scholarLY`
library. Global changes, such as the toggling of all editorial changes, will affect all editorial functions the same.

## Using Editorial Functions

### With Annotations

In the context of annotations, functions are called implicitly by calling the `apply` property in the annotation interface.

```lilypond
\criticalRemark {
  message = {My annotation.}
  apply = addition
}
NoteHead c4
```

`scholarLY` provideds three predefined types: `addition`, `deletion` and
`emendation`. The user can supply new types by using the `\registerOption`
utility provided by `openLilyLib`.

```lilypond
\registerOption scholarly.editorial.functions.my-type
#`((NoteHead . ,parenthesize)
   (Slur . ,slurDashed))
```

To alter the effects of different types, use `\setOption` followed by *all* options for that type (even those not changed -
because this utility does not keep track of what has been previously for the option set once it
is redefined). `\registerOption` should not be used to alter the built-in typ

```lilypond
\setOption scholarly.editorial.functions.addition
#`((NoteHead . ,parenthesize)
   (Slur . ,slurDotted))
```

* * *
**Note** the use of quasi/un-quotes in these syntaxes. This allows lilypond to accept music functions and non-music functions (such as `\slurDashed` which is a .. change) in the same context, and then decide how to parse them

***

### Standalone

Standalone hooks can be used as well with the syntax `<hook> <item> <music>`.

```lilypond
\editorialAddition Slur a4( b)
```

A generic hook allows the
use of additional types predefined by users: `<hook> <type> <item> <music>`.

```lilypond
\editorialFunction #'my-type Slur a4( b)
```

### Custom Functions

We can also utilize new user-defined functions by defining them in the lilypond document as usual. Here is an example of valid use cases taken from the example document for this module `(~/usage-examples/editoral-commands.ly)`.

```lilypond
% define custom function
longerfunction =
#(define-music-function (mus) (ly:music?)
   #{ \once \set fontSize = -4 \parenthesize #mus #})

% set function as option for `deletion` type
\setOption scholarly.editorial.functions.deletion #`(
  (NoteHead . ,longerfunction))

% apply the function
\editorialDeletion NoteHead a
```

## Global Options

TODO
toggling, prioritizing, and changing the configurations, etc.
