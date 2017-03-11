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
% Console output

% By default annotations are printed
\registerOption scholarly.annotate.print ##t

%%%%%%%%%%%%%
% File export

% By default annotations are not exported
\registerOption scholarly.annotate.export-targets #'()
% Internal options where available routines are registered
\registerOption scholarly.annotate.internal.export-routines #'()
% Convenience method for registering routines
#(define register-export-routine
   (define-void-function (name proc)(symbol? procedure?)
     (let ((opt (getOption '(scholarly annotate internal export-routines))))
       (set! opt
             (assq-set! opt name proc))
       (setOption '(scholarly annotate internal export-routines) opt))))


%%%%%%%%%%%%%%%%%
% Limiting output

% By default all annotation types are processed
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
\registerOption scholarly.annotate.sort-criteria #'(rhythmic-location)

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
   (musical-issue . ,green)
   (lilypond-issue . ,red)
   (question . ,blue)
   (todo . ,magenta))

% By default coloring is turned on.
% Only for publication one will want to turn it off
\registerOption scholarly.colorize ##t




%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Filenames to Export
%%%%%%%%%%%%%%%%%%%%%%%%
% default is <project-name>.annotations.<ext>
\registerOption scholarly.annotate.export.filenames
%   <ext>   <file name>
#`((html . "index.html") ;; html
   (latex . default)     ;; latex
   (scheme . default)    ;; scheme
   (plaintext . default) ;; plaintext
   )






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Handling of annotation types for plain text output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% For retrieving values and registering custom annotation types
% see above at the coloring section
% For custom annotation types this alist has to be extended using
% \setOption scholarly.annotate.export.plaintext.labels
%  #(assq-set! (getOption '(scholarly annotage export plaintext labels))
%    <annotation-type> <label>)
\registerOption scholarly.annotate.export.plaintext.labels
#`((critical-remark . "Critical Remark:")
   (musical-issue . "Musical Issue:")
   (lilypond-issue . "LilyPond Issue:")
   (question . "Question:")
   (todo . "TODO:"))



%%%%%%%%%%%%%%%%%
%%%% HTML options
%%%%%%%%%%%%%%%%%

% Annotation types for html text output
\registerOption scholarly.annotate.export.html.labels
#`((critical-remark . "Critical Remark")
  (musical-issue . "Musical Issue")
  (lilypond-issue . "Lilypond Issue>")
  (question . "Question")
  (todo . "TODO"))

% Annotation types for html text class tags
\registerOption scholarly.annotate.export.html.classes
#`((critical-remark . "critical-remark")
  (musical-issue . "musical-issue")
  (lilypond-issue . "lilypond-issue>")
  (question . "question")
  (todo . "todo"))

% Which props to print to html
\registerOption scholarly.annotate.export.html.props
  #`(type grob-location grob-type message)

% Which stylesheet to link in html (just a proof of concept at the moment)
\registerOption scholarly.annotate.export.html.css
  #"annotate-styles.css"



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Handling of annotation types for LaTeX output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Default labels for each known annotation type
% Used for LaTeX export
#(define annotation-type-latex-commands
   `((critical-remark . "\\criticalRemark")
     (musical-issue . "\\musicalIssue")
     (lilypond-issue . "\\lilypondIssue")
     (question . "\\annotateQuestion")
     (todo . "\\annotateTodo")))

% There is no implementation of convenience commands because this should
% not actually be necessary. The LaTeX implementation is designed to work
% together with the LaTeX package, so it should not be configured on user level.
% If the functionality has to be adapted to a given project the above alist
% can be modified directly.
% An additional advantage is that with LaTeX export no special treatment of
% custom anntotation types is necessary, as these simply map to \annotation

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% using lilyglyphs for grob-location

% If set the grob location is formatted as a lilyglyphs command
\registerOption scholarly.annotate.export.latex.use-lilyglyphs ##f

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Handling of labels for property fields
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Default labels for annotation properties
% Used for console printing and plain text  export
\registerOption scholarly.annotate.property-labels
#`((message . "Message")
   (author . "Author(s)")
   (context-id . "Context")
   (source . "Affected Source")
   (voice-name . "Voice")
   (segment-name . "File")
   (ann-footnote . "Annotation Footnote")
   (grob-type . "Affected Item"))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Handling of labels for voice/context names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% \annotate tries to determine the best labels for the
% musical context, either one
% - one explicitly set in the annotation
% - the actual context (Staff level) name
% - the directory name of the originating file
%
% With this option one can map these values to more speaking
% labels that can for example be used to create localized output.

\registerOption scholarly.annotate.context-names
#'()
