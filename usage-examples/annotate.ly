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
        is a second sentence, which\fnblue has a footnote."
      fn-blue-txt = "This is my \uppercase{first nested footnote}, for the \\textcolor{red}{second sentence} of the first annotation."
      fn-yellow-txt = "This is another nested footnote for the first annotation, but it isn't used."
      ann-footnote = "This is a footnote for the entire annotation."
    }
    NoteHead
  f ( |
  g a ) b c
    \musicalIssue \with {
      message = "This is a musical issue with not footnotes."
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
          message="A note about the second voice."
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
      message = "A question with a footnote\fnRandom about the slur."
      fn-R-a-n-d-o-m-txt = "A random footnote for the question."
    }
    Slur
  gis8( a) b4
}

\score { \music }