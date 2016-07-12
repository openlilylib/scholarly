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
\include "footnotes.ily"

annotate =
#(define-music-function (name properties type item)
   ((symbol?) ly:context-mod? symbol? symbol-list-or-music?)
   ;; generic function to annotate a score item
   ;; not to be called by input documents

   (let*
    ( ;; read properties from the \with {} clause
      ;; and check footnote settings
      (props (footnote-proplist (context-mod->props properties)))
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

    ;; The 'context-id' property is the name of the musical context
    ;; the annotation refers to. As our fallthrough solution we
    ;; initially set this to the name of the enclosing directory
    (set! props (assq-set! props 'context-id input-directory))

    ; The input file name is not used so far (as it was a remnant of
    ; the Oskar Fried project). As this may become useful for somebody
    ; one day we'll keep it here.
    (set! props (assq-set! props 'input-file-name input-file-name))

    ;; Check if we do have a valid annotation,
    ;; then process it.
    (if (input-annotation? props)
        ;; Apply the annotation object as an override, depending on the input syntax
        (let
         ((tweak-command
           (cond
            ((and (ly:music? item) (symbol? name))
             ;; item is music and name directs to a specific grob
             ;; annotate the named grob
             #{
               \tweak #`(,name input-annotation) #props #item
             #})
            ((ly:music? item)
             ;; item is music
             ;; -> annotate the music item (usually the NoteHead)
             #{
               \tweak #'input-annotation #props #item
             #})
            (else
             ;; item is a symbol list (i.e. grob name)
             ;; -> annotate the next item of the given grob name
             #{
               \once \override #item #'input-annotation = #props
             #}
             ))))
         ;; If available add automatic footnote hook
         #{
           #tweak-command
           #(if (assq-ref props 'footnote-case)
                (lyfootnote item props))
         #})
        (begin
         (ly:input-warning (*location*) "Improper annotation. Maybe there are mandatory properties missing?")
         #{ #}))))



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
#(define-music-function (name properties item)
    ((symbol?) ly:context-mod? symbol-list-or-music?)
    (if (symbol? name)
        (annotate name properties 'none item)
        (annotate properties 'none item)))

criticalRemark =
% Final annotation about an editorial decision
#(define-music-function (name properties item)
    ((symbol?) ly:context-mod? symbol-list-or-music?)
    (if (symbol? name)
        (annotate name properties 'critical-remark item)
        (annotate properties 'critical-remark item)))

lilypondIssue =
% Annotate a LilyPond issue that hasn't been resolved yet
#(define-music-function (name properties item)
    ((symbol?) ly:context-mod? symbol-list-or-music?)
    (if (symbol? name)
        (annotate name properties 'lilypond-issue item)
        (annotate properties 'lilypond-issue item)))

musicalIssue =
% Annotate a musical issue that hasn't been resolved yet
#(define-music-function (name properties item)
    ((symbol?) ly:context-mod? symbol-list-or-music?)
    (if (symbol? name)
        (annotate name properties 'musical-issue item)
        (annotate properties 'musical-issue item)))

question =
% Annotation about a general question
#(define-music-function (name properties item)
    ((symbol?) ly:context-mod? symbol-list-or-music?)
    (if (symbol? name)
        (annotate name properties 'question item)
        (annotate properties 'question item)))

todo =
% Annotate a task that *has* to be finished
#(define-music-function (name properties item)
    ((symbol?) ly:context-mod? symbol-list-or-music?)
    (if (symbol? name)
        (annotate name properties 'todo item)
        (annotate properties 'todo item)))



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
     Lyrics)

\layout {
  \context {
    \Score
    % The annotation processor living in the Score context
    % processes the annotations and outputs them to different
    % targets.
    \consists \annotationProcessor
  }
}
