\include "oll-core/package.ily"
\loadModule scholarly.editorial-markup
\relative {
  c' c g' g |
  \editorialMarkup sic { as as } g2 |
  \editorialMarkup lemma \with {
    item = Accidental
    source = OE
  } fis4 fis
  e -\editorialMarkup correction \with {
    type = addition
  } -> e
}