\include "oll-core/package.ily"
\loadModule scholarly.choice
\new Score = "Named Score"
\new Staff = "Named Staff"
{
  \setChoicePreference substitution #'old
  \choice substitution \with {
    ann-type = lilypond-issue
    responsibility = Mozart
    certainty = obvious
    source = manuscript-prague
  }{
    \editorialMarkup deletion \with {
%      message = "Changed to two half notes."
      agent = "Erasure"
    }{ c'1 }
    \editorialMarkup addition \with {
      message = "Change from a whole note."
      agent = "Blue ink"
    }{ c'2 c' }
  }
  c'1 |
  \tagSpan whatever \with {
    ann-type = critical-remark
  } d'1
}