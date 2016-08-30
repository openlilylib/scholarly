\version "2.19.42"

\include "oll-core/package.ily"
\loadPackage \with {
  modules = annotate
} scholarly

% \setOption scholarly.editorial.functions.apply ##f


%{
% editorial options can be set:
%   (commented out to verify that options load
%   from editorial-functions/config [y])

\setOption scholarly.editorial.functions.addition #`(
  (Slur . ,slurDashed)
  (NoteHead . ,parenthesize))
%}


% options can incorporate longer function if predefined:

longerfunction =
#(define-music-function (mus) (ly:music?)
   #{ \once \set fontSize = -4 \parenthesize #mus #})

\setOption scholarly.editorial.functions.deletion #`(
  (NoteHead . ,longerfunction))



% options can also be expressed in lilypond code blocks
% (if they don't contain music functions):

\setOption scholarly.editorial.functions.emendation #`(
  (Slur .
    ,#{
    \slurDotted \shape #'((0 . -0.5) (0 . -1.5) (0 . -3.5) (0 . -0.5)) Slur
    #}))





music = {


% using editions:

    \criticalRemark
    \with{
        message = "A remark about adding the slur."
        apply = addition
    } Slur a4( b c') d'

    \musicalIssue
    \with{
        message = "This note needs to be added."
        apply = addition
    } NoteHead a4( b c') d'


% applying edition without an annotation
%   NOTE: global toggle for editorial functions affects
%   both annotated editorial functions and independent
%   ones just the same.

    \editorialAddition Slur a4( b c') d'



% using editions predefined by user:

    \todo
    \with{
        message = "Should we remove the slur?"
        apply = deletion
    } NoteHead a4( b c') d'



% using editions defined in lilypond code blocks within setOption:

    \question
    \with{
        message = "Should we remove the slur?"
        apply = emendation
    } Slur a4( b c') d'

}

\score {
  \new Staff = "my staff" \music
}
