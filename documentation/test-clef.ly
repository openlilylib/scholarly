\version "2.19.82"

\include "oll-core/package.ily"
\loadModule stylesheets.span
\loadModule scholarly.annotate

{
  c'1
  \tagSpan something \with {
    ann-type = critical-remark
    message = ""
  }
  \clef bass
  %\time 1/4
  c'4
}
%}

%{
test =
#(define-music-function (music)(ly:music?)
   (ly:message "~a" (ly:music-property music 'name))
   music)

{
  c'1
  \displayMusic {
    \once \override Staff.Clef.color = #red
    \once \override Staff.Clef.input-annotation = #'((ann-type . critical-remark) (b . 2))
    \clef bass
  }
}

%}