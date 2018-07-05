\version "2.19.42"

\include "oll-core/package.ily"
\loadModule scholarly.annotate

music = \relative c'{
  c4 d e
  \criticalRemark \with {
    message = "Attaches to a single note, not to the attached elements"
  }
  f -. \f ( |
  
  g a ) b c |
  
  \musicalIssue \with {
    message = "This annotates the non-rhythmic key signature."
  } \key a \major
  a d
  <<
    { 
      \voiceOne
      \lilypondIssue \with {
        message = "An annotation for the top voice."
      } cis 
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
  |
  
  \oneVoice
  e
  cis2
  -\lilypondIssue \with {
    message = "A message about the trill."
  } \startTrillSpan
  d4\stopTrillSpan |
  
  b gis8 -\question \with {
    message = "A question about the slur, attached as post-event"
  } ( 
  a ) b4 c |
  
  \musicalIssue \with {
    % message is set to default value
    % annotation is attached to the fist note head,
    % but coloring applies to all Script items in the expression.
    item = Script
  } { c8-. \p d e-- d-! c4 -\fermata }
  
  \todo \with {
    message = "This applies to the whole music."
  } { c8-. \p d e-- d-! c4 -\fermata }
  
  \tagSpan nothing \with {
    ann-type = critical-remark
    message = "Generic \\tagSpan. The whole span is styled,
but only the anchor has the annotation."
  } { c8-. \p d e-- d-! c4 -\fermata }

}

\new Score = "my-score" {
  \new Staff \music
}