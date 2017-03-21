\version "2.19.42"

\include "oll-core/package.ily"
\loadPackage \with {
  modules = annotate
} scholarly

% \setOption scholarly.editorial-functions.apply ##f


%{
% editorial options can be set:
%   (commented out to verify that options load
%   from editorial-functions/config [y])

\setOption scholarly.editorial-functions.addition #`(
  (Slur . ,slurDashed)
  (NoteHead . ,parenthesize))
%}


% options can incorporate longer function if predefined:

longerfunction =
#(define-music-function (mus) (ly:music?)
   #{ \once \set fontSize = -4 \parenthesize #mus #})
transparentStem =
#(define-music-function (mus) (ly:music?)
   #{ \once \override Stem.transparent = ##t #mus #})

\setOption scholarly.editorial-functions.deletion #`(
  (NoteHead . ,longerfunction)
  (Stem . ,transparentStem))



% options can also be expressed in lilypond code blocks
% (if they don't contain music functions):

\setOption scholarly.editorial-functions.emendation #`(
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
    
    \mark \markup { in annotations }

    \musicalIssue
    \with{
        message = "This note needs to be added."
        apply = addition
    } NoteHead a4( b c') d'

  \break

  

% applying edition without an annotation
%   NOTE: global toggle for editorial functions affects
%   both annotated editorial functions and independent
%   ones just the same.

    \editorialAddition Slur a4( b c') d'

  \mark \markup { standalone }

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
    } Slur a4( b c') d' |
    
    
    \break
    
    
    
    % Set the \edit macro as a shorthand:
    
    \editorialShorthand #'addition NoteHead 
      c'8 \edit d' e' <a' \edit f'>
      
    \mark \markup { editorial shorthands "(\\edit)" }  
      
    \editorialShorthand #'addition Slur 
      c' \edit c'( d') c'( e' f') \edit d'( c') d'8 c' b c'
      
      
    \break 
    
    
      
    % Edit everything in the music of item and type, with
    % optional prepend and append (perhaps large parens, etc.)
    
    \editorialSection #'addition NoteHead { 
      \tuplet 5/4 {
        c'4 b' b 
        <c' e'>
        c'8( d')
      }
    }
    
    \mark \markup { editorial sections }
    
    \editorialSection #'deletion Stem { 
      f'4( g') f'( g')
    }
    
    \editorialSection #'addition Slur {
      f'8( g'4) f'8 a16( b c') e'( c'4)
    }
    
}

\score {
  \new Staff = "my staff" \music
}

\paper {
  system-system-spacing.basic-distance = #20
}