\version "2.19.82"

\include "oll-core/package.ily"
\loadModule stylesheets.span
\loadModule scholarly.annotate
\setOption scholarly.annotate.color-anchor ##f
\relative c' {
  \musicalIssue \with {
    message = "Coloring applies to all Script elements but not the anchor."
    item = Script
  } { c8-. \p d e-- d-! c4 -\fermata }
}
