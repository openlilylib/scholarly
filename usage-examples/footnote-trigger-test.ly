\version "2.19.42"

\include "scholarly/package.ly"

music = {
    \criticalRemark
      \with{
        message = "my message/footnote about the slur"
        offset = #'(1 . -2)
    } Slur a4_\lyfootnote ( 
    b c') b
    \criticalRemark
      \with{
        message = "my message without a footnote"
    } NoteHead a1
}

\score {
  \new Staff = "my staff" \music 
}
