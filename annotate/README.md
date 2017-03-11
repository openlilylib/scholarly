# scholarLY: Annotate

The *scholarLY* `annotate` module provides functionality to create annotations of musical
scores engraved with GNU LilyPond. Annotations can be controlled through a number of highly customizable
parameters, one of the most significant of which is its export capabilities which allow
annotations to be further processed by LaTeX (see `scholarly/latex-package`). Note that there are some
issues with using `lilypond-book` and footnotes; see the issue tracker for updates on that until
a workaround is implemented.

The basic behavior for annotations is to be printed in the console log. *By default,
annotations are printed* (see `scholarly/annotate/config.ily`). At the beginning of
the music document, we can tell *scholarLY* which types of documents to export:

```lilypond
\setOption scholarly.annotate.export-targets #'(plaintext html latex)
```

Those output files will automatically format annotations to be further processed by the
relevant programs. Currently, *plaintext*, *latex* and *html* are available, while other types
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

Annotations can be made into LilyPond footnotes by including at least the `footnote-offset` property (which
implicitally triggers the footnote engraver). By default, *scholarLY* assumes `message` is the footnote text;
the `footnote-text` may be included to indicate a footnote message different than the annotation message.

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

These footnotes will be engraved to the LilyPond score, by LilyPond, as usual. Note that they are not acknowledged by
LaTeX if/when the annotations are typeset later in that manner.

## Balloon Text

Annotations can also be engraved as balloon text. As with footnotes, the `balloon-offset` property tells *scholarLY*
to do so. `message` is the default text, and `balloon-text` can optionally specify an alternative message.

```lilypond
\lilypondIssue \with {
  balloon-offset = #'(1 . 2)
  message = "This is my message, and also the balloon text."
} NoteHead a4

\annotateTodo \with {
  balloon-offset = #'(2 . 0)
  balloon-text = "This is my balloon text."
  message = "This is my annotation message."
} Accidental bf
```

Keep in mind that LilyPond isn't able to engrave balloon text to spanners (Slur, Hairpin, TrillSpanner, etc.) yet;
attempts to do so will cause it to crash upon compiling. This is listed on the [LilyPond issue tracker] (https://sourceforge.net/p/testlilyissues/issues/2567/ "LilyIssues 2567").

## LaTeX-Specific Code

This section is only relevant to projects which will later incorporate the *scholarLY* LaTeX package for
typesetting the exported annotations.

### General Usage

Messages may include LaTeX code for annotations intended to be typeset later using the *scholarLY* LaTeX
package. In the meantime, this means nothing for the LilyPond side except that Guile, LilyPond's
Scheme interpreter, will still interpret escape characters and thus may alter the code during compilation.
Therefore, in some cases, double backslashes
are necessary for certain LaTeX hooks to make it to the exported document.

```lilypond
\musicalIssue \with {
  message = "This is a \\textit{message} with {\color{red} some LaTeX code}."
} NoteHead b
```

In a similar vein, it is prudent to be conscious of symbols that may cause trouble for LaTeX such as
`#`, `_`, and so on. Even if such a character is not present in the `message` property, *everything*
contained by any property will be parsed by LaTeX at least once.

### Footnotes

We can organize LaTeX footnotes using similar protocol to the `footnote-text` property. As mentioned
previously, `footnote-..` properties are only applied in LilyPond. They are purposed as *in-score*
footnotes as traditionally used in LilyPond. LaTeX-specific footnotes can be implemented within a
message just as in a typical LaTeX document.

```lilypond
\criticalRemark \with {
  message = "This is an annotation\footnote{some footnote text} with a footnote for LaTeX."
}
```

*scholarLY* also provides a more comprehensive infrastructure for using such footnotes. Use the
built-in `ann-footnote` property to write a footnote for the entire annotation. *scholarLY* will
automatically place the superscript at the end of the message (meaning after punctuation and any
particular message wrappers specified later by the user).

```lilypond
\annotateTodo \with {
  message = "This is my annotation."
  ann-footnote = "This is a footnote for the entire annotation."
}
```

*scholarLY* doesn't yet support custom footnote functionality, which is a major enhancement that
would allow the use of unique footnote hooks within the `message` property (like `\footnote<custom-name>`
or similar) which can be automatically routed by *scholarLY* to the corresponding text (set as
additional properties, like `footnote-<custom-name>-text = "Here is the corresponding text."`).

### The LaTeX Package

Please refer to the documentation in `scholarly/annotate/latex-package` for more information on the
*scholarLY* LaTeX package.

## Coloring Annotated Grobs, and Other Options

By default, *scholarLY* will colorize the annotated grobs according to the OLL option `scholarly.annotate.colors`.
Those colors, which correspond to the annotation types, may be changed using OLL's `\setOption` utility. Similarly,
we can use a global boolean to turn this colorization on or off.

```lilypond
\setOption scholarly.colorize ##t
```

Other options are made available in `scholarly/annotate/config.ily`. That code includes further comments that
explain the abilities and handling of the various options.

## Sorting, Filtering

Annotations are sorted by *rhythmic location* by default. This can be changed to *author* or *type*
by using `\setOption`. As above, more details about this utility are written in `scholarly/annotate/config.ily`.

**Not yet implemented**, annotations will also be able to be filtered by various criteria. Foremost, *priority*
can indicate the importance of some annotations over others. We will be able to *annotate* (which means printing
to console *and* to export) only the priorities that are specified and/or in the order that is specified.

## Further

`annotate` is currently *scholarLY*'s most rubost module. The possibilities are constantly shifting / growing; the
best way to stay up-to-date with the project is by pulling the latest state of the repositories (both `oll-core` and
`scholarly`) and viewing the issue trackers, where new feature requests are frequently being added to the list.
