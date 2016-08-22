# scholarLY: Annotate

The *scholarLY* `annotate` module provides functionality to create annotations of musical 
scores engraved with GNU LilyPond. Annotations can be controlled through a number of highly customizable 
parameters, one of the most significant of which is its export capabilities which allow 
annotations to be further processed by LaTeX (see `scholarly/latex-package`).

The basic behavior for annotations is to be printed in the console log. *By default, 
annotations are printed* (see `scholarly/annotate/config.ily`). At the beginning of
the music document, we can tell *scholarLY* which types of documents to export:

```lilypond
\setOption scholarly.annotate.export-targets #'(plaintext latex)
```

Those output files will automatically format annotations to be further processed by the
relevant programs. Currently, *plaintext* and *latex* are available, while other types
are on the wishlist (such as *scheme* and *markdown*).

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

Messages may include LaTeX code for annotations intended to be typeset later using the *scholarLY* LaTeX
package. In the meantime, this means nothing for the LilyPond side except that Guile, LilyPond's 
Scheme interpreter, will still interpret escape characters and will thus alter the code during compilation. 
Therefore, in some cases, double backslashes 
are necessary for certain LaTeX hooks to make it to the exported document.

In a similar vein, it is prudent to be conscious of symbols that may cause trouble for LaTeX such as
`#`, `_`, and so on. Even if such a character is not present in the `message` property, *everything* 
contained by any property will be parsed by LaTeX at least once.

Please refer to the relevant subdirectory for more information on the *scholarLY* LaTeX package.

## Coloring Annotated Grobs, and Other Options

By default, *scholarLY* will colorize the annotated grobs according to the OLL option `scholarly.annotate.colors`.
Those colors, which correspond to the annotation types, may be changed using OLL's `\setOption` utility. Similarly,
we can use a global boolean to turn this colorization on or off.

```lilypond
\setOption scholarly.colorize ##t
```

Other options are made available in `scholarly/annotate/config.ily`. That code includes further comments to 
explain the handling of the various options.

## Sorting, Filtering

Annotations are sorted by *rhythmic location* by default. This can be changed to *author* or *type*
by using `\setOption`. As above, more details about this utility are written in `scholarly/annotate/config.ily`.

**Not yet implemented**, annotations will also be able to be filtered by various criteria. Foremost, *priority*
can indicate the importance of some annotations over others. We will be able to *annotate* (which means printing
to console *and* to export) only the priorities that are specified and/or in the order that is specified.

## Etc?
