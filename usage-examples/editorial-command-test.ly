\version "2.19.42"

\include "scholarly/package.ly"

music = {
    \criticalRemark
      \with{
        message = "my message/footnote about the slur"
        %offset = #'(-1 . -2)
        apply = addition
    } Slur a4( b c') c'
}

\score {
  \new Staff = "my staff" \music 
}
