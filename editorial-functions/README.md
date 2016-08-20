# scholarLY: Editorial Functions

Editorial functions can be used to apply and organize certain critical changes
made to musical scores. They can be implemented concurrently with
annotations or through their own standalone hooks. *scholarLY* provides a set 
of built-in options
and uses *openLilyLib*'s `\registerOption`/`\setOption`/etc. infrastructure to
handle all options and to potentially create new ones.

## Using Editorial Functions

### With Annotations

In the context of annotations, functions are called implicitly by including the `apply` property in the annotation `with` block.

```lilypond
\criticalRemark \with {
  message = "My annotation."
  apply = addition
}
NoteHead c4
```

*scholarLY* provides three predefined types: `addition`, `deletion` and
`emendation`. The user can supply new types by using the `\registerOption`
utility provided by *openLilyLib*.

```lilypond
\registerOption scholarly.editorial.functions.my-type
#`((NoteHead . ,parenthesize)
   (Slur . ,slurDashed))
```

To alter the effects of different types, use `\setOption` followed by *all* options for that type (even those not changed -
because this utility does not keep track of what has been previously for the option set once it
is redefined). `\registerOption` should not be used to alter the built-in type.

```lilypond
\setOption scholarly.editorial.functions.addition
#`((NoteHead . ,parenthesize)
   (Slur . ,slurDotted))
```

**Note** the use of quasi/un-quotes in these syntaxes. This allows lilypond to accept music functions and non-music functions (such as `\slurDashed`) in the same context, and then decide how to parse them automatically.

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

### Toggle
Editorial functions are, by default, applied to the music in every instance.
Turn all functions *off* with the global toggle at the beginning of the document:

```lilypond
\setOption scholarly.editorial.functions.apply ##f
```

Set to true (`##t`) to explicitly turn editorial functions back on.

This can be applied at specific points within the document as well. 
If a project contains multiple scores,
for example, the editorial functions could be quickly toggled *on* and *off* inline.

### Prioritize / Filter / Sort
*Not yet implemented, these features are TODO's.*
