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
      message = "Go to \\textit{school} and \noindent{sit back}! This
        is a second sentence, which\fnblue has a footnote."
      fn-blue-text = "This is my first nested footnote, for the second sentence of the first annotation."
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
  e  
    \lilypondIssue \with {
      message = "A message about the trill."
    }
    TrillSpanner
  cis2\startTrillSpan
  d4\stopTrillSpan
  b 
    \question \with {
      message = "A question with a footnote\fnTemporary about the slur."
      fn-Temporary-text = "A temporary footnote for the question."
    }
    Slur
  gis8( a) b4 
    \todo \with {
      message = "A todo in the same measure as the last annotation."
    }
    Stem
  a
  
}

\score { \music }