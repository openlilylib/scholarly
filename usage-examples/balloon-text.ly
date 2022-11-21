\version "2.23.80"

% This file is deprecated since (at the moment) balloon text only works with
% the 'legacy module.

\include "oll-core/package.ily"
\loadModule scholarly.annotate.legacy

music = {
  \new Voice \with { \consists "Balloon_engraver" } {

    \once \override BalloonTextItem.X-extent = ##f
    \criticalRemark
      \with{
        message = "message/balloon-text"
        balloon-offset = #'(0.5 . -3)
    } NoteHead a4(

    b c') c'

    \musicalIssue
      \with{
        message = "my message and footnote"
        balloon-text = "my balloon text."
        balloon-offset = #'(1 . 3)
        footnote-offset = #'(-1 . 3)
    } Stem a

    b b

    \question
      \with{
        message = "my message, but not balloon text"
        % LilyPond can't engrave balloons to spanners yet,
        % so it doesn't work here:
        balloon-offset = #'(-0.5 . -2)
    } Hairpin a\p\<

    b c'\!

  }
}


\score {
  \new Staff = "my staff" \music
}
