\version "2.19.42"

\include "scholarly/package.ly"

\setOption scholarly.editorial.addition #`(
  (Slur . ,slurDashed)
  (NoteHead . ,parenthesize))
  
\setOption scholarly.editorial.deletion #`(
  (Slur . ,slurDotted))
  
music = {
    \criticalRemark
    \with{
        message = "my message/footnote about deleting the slur"
        apply = #'addition
    } Slur a4( b c')
    \musicalIssue
    \with{
        message = "my message about adding the note"
        apply = #'addition
    } NoteHead
    b
    \todo
    \with{
        message = "remove the slur"
        apply = #'deletion
    } Slur
    c'2( d')
}

\score {
  \new Staff = "my staff" \music 
}
