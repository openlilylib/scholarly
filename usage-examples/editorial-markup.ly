\version "2.19.80"

\include "oll-core/package.ily"
\loadModule scholarly.editorial-markup
\loadModule scholarly.annotate

\markup \bold { Usage examples for \typewriter "\\editorialMarkup" }
\markup \vspace #1

\markup \justify {
  Apply \typewriter "\\editorialMarkup sic" to a sequential expression. Default
  coloring but no further behaviour
}
\relative {
  c' c g' g |
  \editorialMarkup sic { as as } g2 |
}


\markup \justify {
  Apply \typewriter "\\editorialMarkup original" to a sequential expression,
  targeting \typewriter Flag. Default coloring but no further behaviour.
}
\relative {
  c'4 d
  \autoBeamOff
  \editorialMarkup original \with {
    item = Flag
  } { e8 f e f } g2
}

\markup \justify {
  Apply \typewriter "\\editorialMarkup lemma" to a single music expression,
  targeting \typewriter Accidental. Default coloring but no further behaviour.
  Note that with the non-post-event notation it is only possible to address grobs
  that are implicitly created, not those that are manually attached to the
  note.
}
\relative {
  \editorialMarkup lemma \with {
    item = Accidental
    source = OE
  }
  cis'
}


\markup \justify {
  Apply \typewriter "\\editorialMarkup correction" to a single music expression,
  applied as a post-event to address the articulation.
  Creating a \typewriter "\\musicalIssue" annotation.
}

\relative {
  c' -\editorialMarkup correction \with {
    author = UL
    message = "Articulation added, cf. violin."
    ann-type = musical-issue
    type = substitution
  } -!
}


\markup \justify {
  Apply \typewriter "\\editorialMarkup gap" to a sequential music expression,
  creating a footnote.
}
\relative {
\editorialMarkup gap \with {
    message = "Hey"
    reason = damage
    footnote-offset = #'(2 . -2)
    %footnote-mark = \markup "*"
  }
  { c' e }
  d4 e
}

#(define style-correction-seq
   (define-styling-function
     #{
       \slurDashed
       #music
       \slurSolid
     #}))


\setSpanFunc correction #style-correction-seq
%\setOption stylesheets.span.use-colors ##f

\relative {
  \editorialMarkup correction \with {
    item = Slur
    type = substitution
  }
  { c' ( d e ) f }
}

\relative {
  c'1
  \editorialMarkup sic \with {
    footnote-offset = #'(2 . 1)
    footnote-text = "Hey"
  } { c d }
}

%\relative {
%  <c' \span test e g>4
%}
