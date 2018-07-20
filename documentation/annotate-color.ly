\include "oll-core/package.ily"
\loadModule stylesheets.span
\loadModule scholarly.annotate
sequence =  { c8-. \p d e-- d-! c4 -\fermata }
\setSpanColor nothing #grey
\relative c' {
  \todo \with {
    message = "Whole music is colored, annotation attached to first note."
  } \sequence
  \musicalIssue \with {
    message = "Coloring applies to all Script elements."
    item = Script
  } \sequence
  \tagSpan nothing \with {
    ann-type = critical-remark
    message = "Generic tagSpan. The whole span is colored grey,
               but the anchor is colored with the critical-remark color"
  } \sequence
}
