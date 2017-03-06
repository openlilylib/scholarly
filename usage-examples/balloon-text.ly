\version "2.19.56"

\include "oll-core/package.ily"
\loadPackage \with {
  modules = annotate
} scholarly

music = {
  \new Voice \with { \consists "Balloon_engraver" } {
    \criticalRemark
      \with{
        message = "my message/balloon-text"
        balloon-offset = #'(-1 . -2)
    } NoteHead a4(
  b c') c'
    \musicalIssue
      \with{
        message = "my message"
        balloon-text = "my balloon text."
        balloon-offset = #'(1 . 2)
    } Stem a
  b b
    \question
      \with{
        message = "my message, but not balloon text"
        % LilyPond can't engrave balloons to spanners yet,
        % so it doesn't work here:
        balloon-offset = #'(-0.5 . -2)
      } Hairpin a\p\< b c'\!
  }
}


\score {
  \new Staff = "my staff" \music
}
