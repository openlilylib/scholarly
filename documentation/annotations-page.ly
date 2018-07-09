\include "oll-core/package.ily"
\loadModule scholarly.annotate
\markup "Annotating a single note, with and without attached elements."
\new Score = "basic"
\new Staff = "basic-staff" \relative {
  c'
  \criticalRemark \with {
    message = "Attaches to a single note, not to the attached elements"
  } d
  -. \f ^\markup "Hi" ( e f ) |
  c
  \lilypondIssue \with {
    message = "If that is desired the elements have to be enclosed in a sequential music expression."
  } { d -. \f ^\markup "Hi" ( }
  e f ) |
}

\markup "Stacking post-events"
\new Score = "post-events"
\new Staff \relative {
  c' -\musicalIssue \with {
    message = "Attaches as a post-event, affecting the articulation"
  } -!
  -\lilypondIssue \with {
    message = "Multiple post-events can be stacked"
  } ^\f
  d e2
    -\todo \with {
    message = "A message about the trill. Arbitrary post-events work."
  } \startTrillSpan
  f4\stopTrillSpan
}

\markup "Annotations in polyphony"
\new Score = "polyphonics"
\new Staff \relative {
  r8
  <<
    {
      \voiceOne
      \lilypondIssue \with {
        message = "An annotation for the top voice."
      } cis''
      d
    }
    \new Voice = "voice two"
    {
      \voiceTwo
      \question \with {
        message="A question about the second voice. Applies to the accidental"
        item = Accidental
      } ais
      b
    }
  >>
}

\markup "Various ways of annotating sequential music."
sequence =  { c8-. \p d e-- d-! c4 -\fermata }
\setSpanColor nothing #grey
\new Score = "sequential"
\new Staff \relative c' {
  \todo \with {
    message = "This applies to the whole music."
  } \sequence

  \musicalIssue \with {
    % message is set to default value
    % annotation is attached to the fist note head,
    % but coloring applies to all Script items in the expression.
    item = Script
  } \sequence

  \tagSpan nothing \with {
    ann-type = critical-remark
    message = "Generic \\tagSpan. The whole span is styled,
but only the anchor has the annotation."
  } \sequence
}

\markup "Annotating non-rhythmic events."
\new Score = "non-rhythmic"
\new Staff {
  \musicalIssue \with {
    message = "This annotates the non-rhythmic key signature."
  } \key a \major
  \question \with {
    message = "This annotates the non-rhythmic key signature."
  } \time 2/4
  a'2
  \criticalRemark \with {
    message = "This annotates the non-rhythmic clef."
  } \clef bass
  e
}

\annotateSetGrobNames
#'((NoteHead . "Notenkopf")
   (Hairpin . "Gabel")
   (Slur . "Bogen"))
\markup "Creating footnotes."
\new Score = "footnotes"
\new Staff \relative {
    \musicalIssue \with {
    message = "This annotation has a footnote-text"
    footnote-text = "but that does not trigger the footnote"
  } a'4
  b b c |
  a \p
  -\question \with {
    message = "Footnote is triggered by footnote-offset.
annotation applied as postevent"
    footnote-offset = #'(-0.5 . -2)
    footnote-text = "footnote-text provides a dedicated text to be printed in the footnote"
  } \<
  b c\!
  a4 -\criticalRemark
  \with {
    message = "My message/footnote about the slur.
Footnote text is taken from 'message'."
    footnote-offset = #'(0.5 . 2)
  } ( |
  b c ) c
  \criticalRemark \with {
    message = "Custom footnote mark is possible"
    footnote-offset = #'(0.5 . 1)
    footnote-mark = "?"
  } c
}