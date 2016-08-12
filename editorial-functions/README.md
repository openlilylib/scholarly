# scholarLY: Editorial Functions

Editorial functions can be used to indicate certain critical changes
made to musical scores. They can be applied concurrently with
annotations, or *standalone* through hooks provided by the `scholarLY`
library. Global changes, such as toggling all editorial changes on and
off, affect all editorial functions the same (even if, for example,
..).

## Using Editorial Functions

### With Annotations

In the context of annotations, functions are called implicitly by calling the `apply` property in the annotation interface.

```
\criticalRemark {
  message = {My annotation.}
  apply = addition
}
NoteHead c4
```

`scholarLY` provideds three predefined types: `addition`, `deletion`
and `emendation`. The user can supply new types by using the
`\registerOption` utility provided by `openLilyLib`. To alter the
effects of those predefined types, use `\setOption` followed by *all*
options, even those not changed (because this utility doesn't keep
track of what has been previously set once it is redefined).

### Standalone

Standalone hooks can be used as well, with the syntax `hook <item>
<music>`.

```
\editorialAddition Slur a4( bc)
```

Another syntax can be used with a generic hook, which thus allows the
use of types predefined by users: `hook <type> <item> <music>`.

```
\editorialFunction #'my-type Slur a4( b)
```

## Global Options

TODO
toggling, prioritizing, and changing the configurations