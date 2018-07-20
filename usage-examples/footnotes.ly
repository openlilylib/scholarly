\version "2.19.80"

\include "oll-core/package.ily"
\loadModule scholarly.annotate

music = \relative a' {
  \musicalIssue \with {
    message = "This annotation has a footnote-text"
    footnote-text = "but that does not trigger the footnote"
  } a
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
    message = "Go to \\textit{school} and \\textcolor{red}{sit back}! This
        is a second sentence, which\fnblue has a footnote."
    fn-blue-text = "This is my \uppercase{first nested footnote}, for the \\textcolor{red}{second sentence} of the first annotation."
    fn-yellow-text = "This is another nested footnote for the first annotation, but it isn't used."
    ann-footnote = "This is a footnote for the entire annotation."
  } c
  b gis8 -\question \with {
    message = "A question with a footnote\fnRandom about the slur."
    fn-R-a-n-d-o-m-text = "A random footnote for the question."
  } ( 
  a ) b4 c |

}

\new Score = "my score" {
  \new Staff = "my staff" \music
}
