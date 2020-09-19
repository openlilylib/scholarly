\include "oll-core/package.ily"
\loadModule scholarly.annotate
\loadModule scholarly.annotate.legacy
\setOption scholarly.annotate.export-targets #'(latex)
{
  \musicalIssue \with {
    message = "This is a legacy annotation"
  }
  NoteHead c' d'
  \tagSpan modern \with {
    ann-type = lilypond-issue
    message = "modern-style annotation \\emph{emphasized}"
  } e' f'
}