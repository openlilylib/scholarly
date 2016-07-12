\version "2.19.42"

\include "scholarly/package.ly"

#(display "loaded\n")

%\registerOption documentation.include-file "scholarly/annotate"
%\loadModule "_internal/doc-include/usage-example.ily"

\markup \vspace #1

\setOption scholarly.annotate.export-targets #'(plaintext latex)

music = \relative c'{
  c4 d e
    \criticalRemark \with {
      message = "Go to \\textit{school} and \\textcolor{red}{sit back}! This
        is a second sentence, which\fnblue has a footnote."
      fn-blue-text = "This is my \uppercase{first nested footnote}, for the \\textcolor{red}{second sentence} of the first annotation."
      fn-yellow-text = "This is another nested footnote for the first annotation, but it isn't used."
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
      fn-R-a-n-d-o-m-text = "A random footnote for the question."
    }
    Slur
  gis8( a) b4
    \todo \with {
      message = "A todo for the flag."
    }
    Flag
  \appoggiatura a8
    \lilypondIssue \with {
      message = "Message about this note."
      offset = #'(1 . 5)
    } NoteHead
    gis4

}

\score { \music }