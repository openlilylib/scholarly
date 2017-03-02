\version "2.19.42"

\include "oll-core/package.ily"
\loadPackage \with {
  modules = annotate
} scholarly


\markup \vspace #1

\setOption scholarly.annotate.export-targets #'(plaintext latex)

music = \relative c'{
  c4 d e
    \criticalRemark \with {
      message = "Go to \\textit{school} and \\textcolor{red}{sit back}! This
        annotation has a footnote which LilyPond will ignore."
      ann-footnote = "Here is a footnote for the entire annotation; it
        is only used by the \\LaTeX{} package."
    }
    NoteHead
  f ( |
  g a ) b c
    \musicalIssue \with {
      message = "This is a musical issue with no footnotes."
    }
    Staff.KeySignature
  \key a \major
  a d
  <<
    { \voiceOne
        \criticalRemark \with {
          message = "An annotation for the top voice."
        }
        NoteHead
      cis d
    }
     \new Voice = "voice two"
    { \voiceTwo
        \criticalRemark \with {
          message = "A note about the second voice."
        }
        NoteHead
      ais b
    }
  >>
  \oneVoice
  e
    \lilypondIssue \with {
      message = "A message about the trill."
    }
    TrillSpanner
  cis2\startTrillSpan
  d4\stopTrillSpan
  b
    \question \with {
      message = "A question with a footnote about the slur."
      ann-footnote = "A random footnote for the question."
    }
    Slur
  gis8( a) b4
}

\score { \music }