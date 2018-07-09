\include "oll-core/package.ily"
\loadPackage \with {
  modules = choice.editorial-markup
} scholarly
music =
#(define-music-function ()()
   #{
     \choice variants {
       \editorialMarkup lemma \with {
         source = "OE"
       }{ <c' c''>1 }
       \editorialMarkup reading \with {
         source = "draft"
       }{ <e' e''>2 <e' e''> q q}
       \editorialMarkup reading \with {
         source = "fair-copy"
       }{ c'8 d' e' d' c' d' e' d' }
     }
   #})
{
  \music
  \setOption scholarly.choice.preferences.variants "draft"
  \music
  \setOption scholarly.choice.preferences.variants "fair-copy"
  \music
}

