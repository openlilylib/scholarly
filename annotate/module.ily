
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

\version "2.19.80"

\loadModule scholarly.annotate.common
\loadModule stylesheets.span

% Color an annotation in the annotation type's color
% (if option is active)
#(define color-annotation
   (define-styling-function
    (let*
     ((ann-type (assq-ref span-annotation 'ann-type))
      (color (getChildOption '(scholarly annotate colors)
               (if (string? ann-type)
                   (string->symbol ann-type)
                   ann-type))))
     (if (getOption '(scholarly annotate use-colors))
         (case style-type
           ((wrap)
            (if item
                (colorMusic (list item) color music)
                (colorMusic color music)))
           ((tweak)
            (let ((target (if item (list item 'color) 'color)))
              (propertyTweak target color music)))
           ((once)
            (make-sequential-music
             (list
              (once (overrideProperty (append item (list 'color)) color))
              music))))
         music))))
\setSpanFunc annotation #color-annotation

% Helper function to merge the annotation type into the annotation.
% If no <mods> are passed into the function a bare mod is created
#(define (add-type mods type)
   (let ((new-mods
          (append
           (if mods (ly:get-context-mods mods) '())
           (list `(assign ann-type ,type)))))
     (ly:make-context-mod new-mods)))

% Validator to verify the encoding of \tagSpan annotation
#(define validate-scholarly-annotation
   (define-span-validator
    (let ((valid
           (let ((ann-type (assq-ref annotation 'ann-type)))
             (and ann-type
                  (memq ann-type
                    '(critical-remark musical-issue lilypond-issue question todo))))))
      (if (not valid)
          (set! warning-message "Missing or invalid attribute 'ann-type'.
Valid types: '(critical-remark musical-issue lilypond-issue question todo)"))
      valid)))
\setChildOption stylesheets.span.validators annotation #validate-scholarly-annotation



% Shorthand to be able to use a simple string as argument to an annotation
% In that case the string will be converted to 'message' as the only attribute
#(define (context-mod-or-string? obj)
   (or (ly:context-mod? obj)
       (string? obj)))

#(define (check-attrs value)
   (if (ly:context-mod? value)
       value
       #{ \with { message = #value } #}))

% The actual commands are wrappers around \tagSpan

criticalRemark =
#(define-music-function (attrs music)((context-mod-or-string?) ly:music?)
   (tagSpan 'annotation (add-type (check-attrs attrs) "critical-remark") music))

musicalIssue =
#(define-music-function (attrs music)((context-mod-or-string?) ly:music?)
   (tagSpan 'annotation (add-type (check-attrs attrs) "musical-issue") music))

lilypondIssue =
#(define-music-function (attrs music)((context-mod-or-string?) ly:music?)
   (tagSpan 'annotation (add-type (check-attrs attrs) "lilypond-issue") music))

question =
#(define-music-function (attrs music)((context-mod-or-string?) ly:music?)
   (tagSpan 'annotation (add-type (check-attrs attrs) "question") music))

todo =
#(define-music-function (attrs music)((context-mod-or-string?) ly:music?)
   (tagSpan 'annotation (add-type (check-attrs attrs) "todo") music))
