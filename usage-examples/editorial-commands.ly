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
        message = "A remark about adding the slur."
        apply = #'addition
    } Slur a4( b c') d'
    
    \musicalIssue
    \with{
        message = "This note needs to be added."
        apply = #'addition
    } NoteHead a4( b c') d'
    
    \question
    \with{
        message = "Should we remove the slur?"
        apply = #'deletion
    } Slur a4( b c') d' 
}

\score {
  \new Staff = "my staff" \music 
}
