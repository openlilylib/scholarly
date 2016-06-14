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
      message = "Go to \\textit{school}\fnOne and\\\ \\noindent{sit back}! This
        is another sentence\fnThree with a footnote."
      context = "Some staff"
      fn-one-text = "This is my first nested footnote"
      fn-two-text = "This is another nested footnote, but it isn't used."
      ann-footnote = "This is an annotation footnote"
    }
    NoteHead
    f ( |
  g a ) b c
  \musicalIssue \with {
    message = "Another test"
  }
  Staff.KeySignature
  \key a \major
  a d
    <<
      {
        \voiceOne
        \criticalRemark \with {
          message = "go to @\emph{school}@!"
        }
        Accidental
        cis? d
      }
      \new Voice {
        \voiceTwo
        ais b
      }
    >>
    \oneVoice
    cis d
}

\score {
  \new Staff = "My Staff" \music
}