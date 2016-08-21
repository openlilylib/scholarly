# scholarLY: Annotate

The *scholarLY* `annotate` module provides functionality to create annotations of musical 
scores engraved with GNU LilyPond. Annotations can be controlled through a number of highly customizable 
parameters, one of the most significant of which is its export capabilities which allow 
annotations to be further processed by LaTeX (see `scholarly/latex-package`).

## Basic Syntax

Annotations are implemented a few possible ways. The most common usage is `<hook> <properties> <item> <music>`.

```lilypond
\criticalRemark \with {
  message = "This slur is too long."
} Slur a( b)
```

The ***hook*** typically indicates the type of annotation: one of `\criticalRemark`, `\musicalIssue`, 
`\lilypondIssue`, `\annotateTodo` and `\annotateQuestion`. Also available is the generic `\annotation`
which requires a *type* property to be specified in the context mod (the `\with` block) that follows.

The only mandatory ***property*** is usually `message`, though sometimes also `type` as just explained. Other
properties unique to individual annotations can be specified. This is where properties such as `priority`
(see *Sorting, Filtering* below), `apply` (see `scholarly/editorial-functions`) and `footnote-..` (see
*Footnotes* below) are placed.

***Item*** is the symbol name of the item being annotated. In the above example, "Slur" indicates we are 
annotating the slur that follows.

***Music*** is simply the instance of music that is engraved.

## Footnotes

Annotations can be made into footnotes by including the `footnote-offset` property. `footnote-text` may be
included as well to indicate a footnote message different than the annotation message. If `footnote-text`
is not present, *scholarLY* will automatically use `message`.

```lilypond
\lilypondIssue \with {
  footnote-offset = #'(1 . 2)
  message = "This is my message, and also the footnote text."
} NoteHead a4

\annotateTodo \with {
  footnote-offset = #'(2 . 0)
  footnote-text = "This is my footnote."
  message = "This is my annotation message."
} Accidental bf
```

These footnotes will be engraved to the lilypond score as usual. Note that they are not acknowledged by
LaTeX if/when the annotations are typeset later in that manner.

## LaTeX-Specific Code

Messages may include LaTeX code. Note that Guile, LilyPond's Scheme interpreter, will see escape characters
as usual; so double backslashes are necessary for LaTeX hooks that begin with `t`, `something else?`, etc.

(example)

..

Mention hash symbol, etc. in LaTeX.

## Prioriizing, Filtering

..

## Etc?
