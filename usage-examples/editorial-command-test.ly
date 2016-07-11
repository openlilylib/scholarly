\version "2.19.42"

\include "scholarly/package.ly"

music = {
    \criticalRemark
    \with{
        message = "my message/footnote about deleting the slur"
        apply = #'deletion
        offset = #'(1 . -2)
    } Slur
    a4( b c')
    \criticalRemark
    \with{
        message = "my message about adding the note"
        apply = #'addition
    } NoteHead
    b
}

\score {
  \new Staff = "my staff" \music 
}
