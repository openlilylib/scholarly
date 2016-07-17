
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
% This file is part of ScholarLY,                                             %
%                      =========                                              %
% a toolkit library for scholarly work with GNU LilyPond and LaTeX,           %
% belonging to openLilyLib (https://github.com/openlilylib/openlilylib        %
%              -----------                                                    %
%                                                                             %
% ScholarLY is free software: you can redistribute it and/or modify           %
% it under the terms of the GNU General Public License as published by        %
% the Free Software Foundation, either version 3 of the License, or           %
% (at your option) any later version.                                         %
%                                                                             %
% ScholarLY is distributed in the hope that it will be useful,                %
% but WITHOUT ANY WARRANTY; without even the implied warranty of              %
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               %
% GNU Lesser General Public License for more details.                         %
%                                                                             %
% You should have received a copy of the GNU General Public License           %
% along with ScholarLY.  If not, see <http://www.gnu.org/licenses/>.          %
%                                                                             %
% ScholarLY is maintained by Urs Liska, ul@openlilylib.org                    %
% Copyright Urs Liska, 2015                                                   %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%{
  \annotate - main file
  This file contains the "collector" and "processor" engravers for annotations
  and the interface music functions to enter annotations in LilyPond input files.
  TODO:
  - generate clickable links when writing to file
  - enable the music function to apply editorial functions
    to the affected grob (e.g. dashing slurs, parenthesizing etc.).
    This has to be controlled by extra annotation properties
    and be configurable to a high degree (this is a major task).
  - provide an infrastructure for custom annotation types
%}

\version "2.19.22"

% TODO:
% Once the module handling system in oll-core has been updated
% (see https://github.com/openlilylib/oll-core/issues/9)
% change the following includes accordingly
% From oll-core
\include "util/consist-to-contexts.ily"
\include "util/context-mod->props.ily"
\include "util/grob-location.ily"

% Global object storing all annotations
#(define annotations '())

% Include factored out functionality
\include "config.ily"
\include "sort.ily"
\include "format.ily"
\include "export.ily"
\include "export-latex.ily"
\include "export-plaintext.ily"
\include "engraver.ily"

% Include `editorial-functions` module
\include "../editorial-functions/__main__.ily"

#(define annotate
  (define-music-function (name properties type item mus)
   ((symbol?) ly:context-mod? symbol? symbol-list-or-music? (ly:music?))
   ;; generic (internal only) function to annotate a score item
   (let*
    ( ;; process context-mod with footnote settings
      (props (context-mod->props properties))
      ;; retrieve a pair with containing directory and input file
      (input-file (string-split (car (ly:input-file-line-char-column (*location*))) #\/ ))
      (ctx (list-tail input-file (- (length input-file) 2)))
      ;; extract directory name (-> part/voice name)
      (input-directory (car ctx))
      ;; extract segment name
      ; currently this is still *with* the extension
      (input-file-name (cdr ctx)))
    ;; The "type" is passed as an argument from the wrapper functions
    ;; The symbol 'none refers to the generic \annotation function. In this case
    ;; we don't set a type at all to ensure proper predicate checking
    ;; (the annotation must then have an explicit 'type' property passed in
    ;; the properties argument)
    (if (not (eq? type 'none))
        (set! props (assq-set! props 'type type)))
    ;; pass along the input location to the engraver
    (set! props (assq-set! props 'location (*location*)))
    ;; 'Context-id' property is the name of the musical context the annotation
    ;; references; initially set to name of enclosing directory.
    (set! props (assq-set! props 'context-id input-directory))
    ; Input file name is not used so far (was a remnant of the Oskar Fried
    ; project). As this may become useful one day we'll keep it here.
    (set! props (assq-set! props 'input-file-name input-file-name))
    ;; Check if valid annotation, then process
    (if (input-annotation? props)
        ;; Apply annotation object as override, depending on input syntax
        (let
         ((tweak-command
           (cond
            ((and (ly:music? item) (symbol? name))
             ;; item is music, name specifies grob: annotate the grob
             #{ \tweak #`(,name input-annotation) #props #item #})
            ((ly:music? item)
             ;; item is music: annotate the music (usually the NoteHead)
             #{ \tweak #'input-annotation #props #item #})
            (else
             ;; item is symbol list: annotate the next item of the given grob name
             #{ \once \override #item #'input-annotation = #props #}))))
         #{
          #tweak-command
          #(if (assq-ref props 'footnote-offset)
          ;; If offset present, add automatic footnote
               (begin
                 (if (not (assq-ref props 'footnote-text))
                     (set! props (assoc-set! props 'footnote-text (assq-ref props 'message))))
                 (let ((offset (assq-ref props 'footnote-offset))
                       (text (assq-ref props 'footnote-text)))
		   #{ \footnote #offset #text #item #})))
	  #(if (assq-ref props 'apply)
	  ;; If `apply` property used, apply editorial function
	       (let ((edition (string->symbol (assoc-ref props 'apply))))
                    (editorialFunction edition item mus))
           mus)
         #})
        (begin
         (ly:input-warning (*location*) "Improper annotation. Maybe there are mandatory properties missing?")
         #{ #})))))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Public interface
%%%% Define one generic command \annotation
%%%% and a number of wrapper functions for different annotation types
%
% Annotations may have an arbitrary number of key=value properties,
% some of them being recognized by the system.
% A 'message' property is mandatory for all annotation types.

annotation =
% Generic annotation, can be used to "create" custom annotation types
% Note: a 'type' property is mandatory for this command
#(define-music-function (name properties item mus)
    ((symbol?) ly:context-mod? symbol-list-or-music? (ly:music?))
    (if (symbol? name)
        (annotate name properties 'none item mus)
        (annotate properties 'none item mus)))

criticalRemark =
% Final annotation about an editorial decision
#(define-music-function (name properties item mus)
    ((symbol?) ly:context-mod? symbol-list-or-music? (ly:music?))
    (if (symbol? name)
        (annotate name properties 'critical-remark item mus)
        (annotate properties 'critical-remark item mus)))

lilypondIssue =
% Annotate a LilyPond issue that hasn't been resolved yet
#(define-music-function (name properties item mus)
    ((symbol?) ly:context-mod? symbol-list-or-music? (ly:music?))
    (if (symbol? name)
        (annotate name properties 'lilypond-issue item mus)
        (annotate properties 'lilypond-issue item mus)))

musicalIssue =
% Annotate a musical issue that hasn't been resolved yet
#(define-music-function (name properties item mus)
    ((symbol?) ly:context-mod? symbol-list-or-music? (ly:music?))
    (if (symbol? name)
        (annotate name properties 'musical-issue item mus)
        (annotate properties 'musical-issue item mus)))

question =
% Annotation about a general question
#(define-music-function (name properties item mus)
    ((symbol?) ly:context-mod? symbol-list-or-music? (ly:music?))
    (if (symbol? name)
        (annotate name properties 'question item mus)
        (annotate properties 'question item mus)))

todo =
% Annotate a task that *has* to be finished
#(define-music-function (name properties item mus)
    ((symbol?) ly:context-mod? symbol-list-or-music? (ly:music?))
    (if (symbol? name)
        (annotate name properties 'todo item mus)
        (annotate properties 'todo item mus)))



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Set default integration in the layout contexts.
%%%% All settings can be overridden in individual scores.

\consistToContexts #annotationCollector
  #'(Staff
     DrumStaff
     RhythmicStaff
     TabStaff
     GregorianTranscriptionStaff
     MensuralStaff
     VaticanaStaff
     Dynamics
     Lyrics
     FiguredBass)

\layout {
  \context {
    \Score
    % The annotation processor living in the Score context
    % processes the annotations and outputs them to different
    % targets.
    \consists \annotationProcessor
  }
}
