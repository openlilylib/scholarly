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
  This file defines the configuration options for \annotate
%}

%%%%%%%%%%%%%%%%%%%%%%%
% fundamental behaviour
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%
% Export targets

% By default annotations are not exported
\registerOption scholarly.annotate.export-targets #'(console)
% Internal options where available routines are registered
\registerOption scholarly.annotate.internal.export-routines #'()
% Convenience method for registering routines
#(define register-export-routine
   (define-void-function (name proc)(symbol? procedure?)
     (setChildOption '(scholarly annotate internal export-routines) name proc)))


%%%%%%%%%%%%%%%%%
% Limiting output

% By default all annotation types are processed.
% Any annotations whose type is included in the symbol-list will
% not be included in the export.
% Possible values for the list elements are:
% - critical-remark
% - musical-issue
% - lilypond-issue
% - question
% - todo
\registerOption scholarly.annotate.ignored-types #'()

%%%%%%%%%%%%%%%%%%%%%
% Sorting annotations

% By default sort annotations in chronological order.
% For other sorting options set this option to a symbol-list.
% Sorting should be cumulative, so the order of criteria is relevant.
% Available sort criteria:
% - rhythmic-location
% - type
% - author
% - score
% - context
\registerOption scholarly.annotate.sort-by #'(rhythmic-location)

%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Coloring annotations
%%%%%%%%%%%%%%%%%%%%%%%%%

% colors are managed as children of scholarly.annotate.colors,
% so they can be retrieved with
% \getOption scholarly.annotate.colors.<type>, e.g.
% \getOption scholarly.annotate.colors.critical-remark
% When custom annotation types are to be used a color has to be set with
% \registerOption scholarly.annotate.colors.<type> <default>
\registerOption scholarly.annotate.colors
#`((critical-remark . ,darkgreen)
   (musical-issue   . ,green)
   (lilypond-issue  . ,red)
   (question        . ,blue)
   (todo            . ,magenta))
annotateSetColor =
#(define-void-function (type color)(symbol? color?)
   (setChildOption '(scholarly annotate colors) type color))
annotateSetColors =
#(define-void-function (mappings)(alist?)
   (for-each
    (lambda (color)
      (setChildOption '(scholarly annotate colors)
        (car color) (cdr color)))
    mappings))

% By default coloring is turned on.
% Only for publication one will want to turn it off
\registerOption scholarly.annotate.use-colors ##t

% By default the annotation *anchor* will also be colored
% (when coloring is on). Setting this option to ##f will
% leave the anchor uncolored.
\registerOption scholarly.annotate.color-anchor ##t


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Annotation type labels
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% For retrieving values and registering custom annotation types
% see above at the coloring section
\registerOption scholarly.annotate.type-labels
#`((critical-remark . "Critical Remark:")
   (musical-issue   . "Musical Issue:")
   (lilypond-issue  . "LilyPond Issue:")
   (question        . "Question:")
   (todo            . "TODO:"))
annotateSetTypeLabel =
#(define-void-function (type label) (symbol? string?)
   (setChildOption '(scholarly annotate type-labels) type label))
annotateSetTypeLabels =
#(define-void-function (mappings)(alist?)
   (for-each
    (lambda (type)
      (setChildOption '(scholarly annotate type-labels)
        (car attribute) (cdr attribute)))
    mappings))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Concise or verbose attribute output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% List of attributes that are suppressed for non-verbose output
\registerOption scholarly.annotate.skip-attributes
#'(context-id
   grob
   grob-location
   grob-type
   input-file-name
   music-type
   location
   score-id
   span-class
   style-type
   ; attributes from stylesheets.span
   example-alignment
   example-direction
   footnote-mark
   footnote-offset
   )

% Returns a list with to-be-skipped attribute keys.
% if <latex> is #t then determine the toggle value for LaTeX output,
% otherwise for general output.
% Depending on that value return either the skip-list or an empty list
#(define (get-skipped-attributes latex)
   (let
    ((show-all
      (if latex
          (getOption '(scholarly annotate export latex all-attributes))
          (getOption '(scholarly annotate export all-attributes)))))
    (if show-all '()
        (getOption '(scholarly annotate skip-attributes)))))

% Export all attributes?
% By default this is set to ##t, otherwise suppress attributes
% listed in scholarly.annotate.internal.skip-attributes
\registerOption scholarly.annotate.export.all-attributes ##f


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Handling of labels for property fields
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Default labels for annotation properties
% Used for console printing and plain text  export
\registerOption scholarly.annotate.attribute-labels
#`((ann-footnote  . "Annotation Footnote")
   (ann-type      . "Annotation Type")
   (author        . "Author(s)")
   (context-id    . "Context (original)")
   (context-label . "Context")
   (grob-type     . "Affected Item (original)")
   (grob-label    . "Affected Item")
   (grob-location . "Musical time (details)")
   (item          . "Explicit target item")
   (location      . "Input location")
   (message       . "Message")
   (score-id      . "Score (original)")
   (score-label   . "Score")
   (source        . "Affected Source")
   (span-class    . "Span Class")
   (style-type    . "Application")
   ;; attributes from scholarly.editorial-markup
   (agent         . "Agent")
   (certainty     . "Certainty")
   (reason        . "Reason")
   (responsible   . "Responsible Entity")
   (source        . "Source")
   (type          . "Type (of something)")
   ;; attributes from stylesheets.span
   (example       . "Music Example")
   (example-alignment . "Horizontal Example Alignment")
   (example-direction. "Vertical Example Direction")
   (footnote-mark . "Footnote Mark")
   (footnote-offset . "Footnote Offset")
   (footnote-text . "Footnote Text")
   )

annotateSetAttributeLabel =
#(define-void-function (name label)(symbol? string?)
   (setChildOption '(scholarly annotate attribute-labels) name label))
annotateSetAttributeLabels =
#(define-void-function (mappings)(alist?)
   (for-each
    (lambda (attribute)
      (setChildOption '(scholarly annotate attribute-labels)
        (car attribute) (cdr attribute)))
    mappings))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Handling of labels for context/score/grob names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% \annotate tries to determine the best labels for the
% musical context, either one
% - one explicitly set in the annotation
% - the actual context (Staff level) name
% - the directory name of the originating file
%
% With this option one can map these values to more speaking
% labels that can for example be used to create localized output.
\registerOption scholarly.annotate.context-names #'()

annotateSetContextName =
#(define-void-function (context display-name)(symbol? string?)
   (setChildOption '(scholarly annotate context-names) context display-name))
annotateSetContextNames =
#(define-void-function (mappings)(alist?)
   (for-each
    (lambda (context)
      (setChildOption '(scholarly annotate grob-names) (car context) (cdr context)))
    mappings))

\registerOption scholarly.annotate.score-names #'()

annotateSetScoreName =
#(define-void-function (score display-name)(symbol? string?)
   (setChildOption '(scholarly annotate score-names) score display-name))
annotateSetScoreNames =
#(define-void-function (mappings)(alist?)
   (for-each
    (lambda (score-id)
      (setChildOption '(scholarly annotate score-names) (car score-id) (cdr score-id)))
    mappings))

\registerOption scholarly.annotate.grob-names #'()

annotateSetGrobName =
#(define-void-function (grob-type display-name)(symbol? string?)
   (setChildOption '(scholarly annotate grob-names) grob-type display-name))
annotateSetGrobNames =
#(define-void-function (mappings) (alist?)
   (for-each
    (lambda (grob-type)
      (setChildOption '(scholarly annotate grob-names) (car grob-type) (cdr grob-type)))
    mappings))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Handling of annotation types for LaTeX output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Default labels for each known annotation type
% Used for LaTeX export
\registerOption scholarly.annotate.export.latex.commands
#`((critical-remark . "\\criticalRemark")
   (musical-issue   . "\\musicalIssue")
   (lilypond-issue  . "\\lilypondIssue")
   (question        . "\\annotateQuestion")
   (todo            . "\\annotateTodo"))

% There is no implementation of convenience commands because this should
% not actually be necessary. The LaTeX implementation is designed to work
% together with the LaTeX package, so it should not be configured on user level.
% If the functionality has to be adapted to a given project the above alist
% can be modified directly.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% using lilyglyphs for grob-location

% If set the grob location is formatted as a lilyglyphs command
\registerOption scholarly.annotate.export.latex.use-lilyglyphs ##f

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Export all attributes to LaTeX?

% By default this is set to ##t, otherwise suppress attributes
% listed in scholarly.annotate.internal.skip-attributes
\registerOption scholarly.annotate.export.latex.all-attributes ##t
