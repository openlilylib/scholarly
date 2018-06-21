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

\version "2.19.80"

% scholarly.editorial-markup module
%
% Provides \editorialMarkup to encode editorial findings and decisions.
% Inspired by corresponding parts of MEI 3.0
% - http://music-encoding.org/guidelines/v3/content/critapp.html
% - http://music-encoding.org/guidelines/v3/content/edittrans.html
%
% \editorialMarkup marks up single or sequential music expressions,
% also accessible as post-event for \tweak-able items.
% Optionally it can
% - create annotations
% - call styling functions
% - create
%   - footnotes
%   - balloonText (currently broken)
%   - music examples (wish)


\loadModule stylesheets.span
\loadModule scholarly.annotate

#(set-object-property! 'anchor 'music-type? ly:music?)
#(set-object-property! 'anchor 'music-doc
   "Pointer to the music element the annotation is attached to")


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Functions to modify the music expression
%
% The following functions modify the music expression *in-place*,
% effectively adding elements to it.


% Create and attach a footnote if one is requested.
% - Footnote is created when 'footnote-offset is set
% - If 'footnote-text is set this is used as footnote text
%   else the 'message is copied to the footnote.
#(define make-footnote
   (define-void-function (mus annot) (ly:music? list?)
     (let ((offset (assq-ref annot 'footnote-offset)))
       (if offset
           (let*
            ;; Determine footnote text
            ((text (or (assq-ref annot 'footnote-text)
                       (assq-ref annot 'message)))
             (mark (assq-ref annot 'footnote-mark))
             ;; chose *first* or *single* element as target
             (target (if (assq-ref annot 'is-postevent?)
                         mus
                         (first (ly:music-property mus 'elements)))))
            (if mark
                ;; specify footnote mark
                (footnote mark offset (string-append mark " " text) target)
                ;; use auto-incremented footnote number
                (footnote offset text target)))))))

% TODO: Adapt this to the new structure:
#(define make-balloon
   (define-void-function (mus annot) (ly:music? list?)
     ))

% Predicate for the type of editorial markup.
% While \span generally can accept arbitrary names
% we have to be more strict here in order to avoid
% ambiguity and especially complications if we should
% ever want to export to MEI.
%
% Also, the attributes module implements checks for
% the interdependency of types and attributes.
#(define (ed-markup-type? obj)
   (and (symbol? obj)
        (member obj
          '(lemma            ;; <lem>, preferred reading
             reading        ;; <rdg>, alternative reading from different source
             addition       ;; <add>, addition *in the source*
             deletion       ;; <del>, deletion *in the source*
             restoration    ;; <restore>, restoration of a deleted text *in the source*
             original       ;; <orig>, original (but not erroneous) text
             regularization ;; <reg>, regularized (but not corrected) text
             sic            ;; <sic>, erroneous text in the source
             gap            ;; <gap>, missing material
             emendation     ;; <corr>, editorial emendation
             ))))

% Load data about the different span-types:
% - default styling colors
% - default styling functions (?)
% - rules about allowed combinations
\loadModule scholarly.attributes

% Encode a source finding or editorial decision
% - span-type (mandatory)
%   specify the type of case, has to be a value from the list in ed-markup-type?
% - attrs (optional)
%   \with {} block with further specification of the case.
%   if this contains an ann-type entry there will be a proper annotation,
%   generated in the AnnotationCollector engraver.
% - mus (mandatory)
%   the music to be annotated
%
% The function works as a standalone music function or as a post-event.
%
% Optionally a footnote can be created
% (BalloonText has to be reimplmented)
% If present a highlighting function is applied, with simple coloring
% as the fallback solution.
editorialMarkup =
#(define-music-function (span-type attrs mus)
   (ed-markup-type? (ly:context-mod?) ly:music?)
   (let*
    ((annot (make-span-description span-type attrs (*location*) mus))
     (anchor (if (memq 'sequential-music (ly:music-property mus 'types))
                 (first (ly:music-property mus 'elements))
                 mus)))
    ;; Attach annotation to anchor grob
    (once (propertyTweak 'input-annotation annot anchor))
    ;; Store reference for use in \span and enclosing \choice
    (ly:music-set-property! mus 'anchor anchor)
    ;; Attach a footnote if requested
    (make-footnote mus annot)
    ;; Attach a balloon text if requested
    ; NOTE: the following function isn't implemented yet
    (make-balloon mus annot)

    ;; Finally return the updated music expression,
    ;; after applying formatting/highlighting
    (format-span annot mus)))
