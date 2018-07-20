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
  This file defines the actual engraver
%}

% Create custom property 'input-annotation
% to pass information from the music function to the engraver
#(set-object-property! 'input-annotation 'backend-type? alist?)
#(set-object-property! 'input-annotation 'backend-doc "Grob property to hold an annotation")

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper functions for the engraver

% Produce highlighting for the anchor of an annotation
#(define (color-anchor grob ann-type)
   (if (getOption '(scholarly annotate use-colors))
       (ly:grob-set-property! grob 'color
         (getChildOption '(scholarly annotate colors) ann-type))))


% Get a context name, recursively going up the hierarchy
% until the 'Score. If <score-only> is #t then *only* the score's
% id is considered.
#(define (get-context-id context score-only)
   (if (and (or (eq? (ly:context-name context) 'Score) (not score-only))
            (not (member (ly:context-id context) '("" "\\new"))))
       (ly:context-id context)
       (letrec
        ((score-ctx
          (lambda (ctx)
            (let ((parent (ly:context-parent ctx)))
              (if parent
                  (if (or (and score-only
                               (eq? (ly:context-name parent) 'Score))
                          (not (or score-only
                                   (member (ly:context-id parent) '("" "\\new")))))
                      (ly:context-id parent)
                      (score-ctx parent))
                  "")))))
        (let ((id (score-ctx context)))
          (if (member id '("" "\\new"))
              #f id)))))

%%%%%%%%%%%%%%%%%%%%%
% Annotation engraver
%
annotationEngraver =
#(let*
  ((annotated-grobs '())
   (all-grobs '())
   (all-contexts '())
   (ignored-types (getOption '(scholarly annotate ignored-types))))
  (lambda (context)
    (make-engraver
     (acknowledgers
      ((grob-interface engraver grob source-engraver)
       ;; 'input-annotation property is only reliably visible in
       ;; stop-translation-timestep, therefore we have to record *all* grobs here
       ;; recording context of source-engraver too for retrieval of context-id
       (set! all-grobs (cons grob all-grobs))
       (set! all-contexts (cons (ly:translator-context source-engraver) all-contexts))))
     ((stop-translation-timestep translator)
      (for-each
       (lambda (grob source-context)
         (let*
          ((annotation (ly:grob-property grob 'input-annotation))
           (ann-type (assq-ref annotation 'ann-type))
           (is-annotation
            ;; A grob is annotated when 'annotation *does* have some content,
            ;; ann-type is set, and it is not marked as ignored.
            (and ann-type (not (member ann-type ignored-types)))))
          (if is-annotation
              (begin
               ;; Coloring is done regardless of annotation export
               (color-anchor grob ann-type)
               ;; If we have any export targets process the annotation
               (if (not (null? (getOption '(scholarly annotate export-targets))))
                   ;; Enrich the annotation with information that is only available now.
                   (let*
                    ((context-id
                      ;; Set context-id to
                      ;; a) an explicit 'context' attribute
                      ;; b) an implicit context name through a named context or
                      ;; c) the directory.file as determined by \tagSpan
                      (or (assq-ref annotation 'context)
                          (get-context-id source-context #f)
                          (assq-ref annotation 'context-id)))
                     ;; Look up a context-name label from the options if one is set,
                     ;; otherwise use the retrieved context-name.
                     (context-label
                      (getChildOptionWithFallback '(scholarly annotate context-names)
                        (string->symbol context-id) context-id))
                     ;; Retrieve name of score if explicitly given
                     (score-id (get-context-id context #t))
                     ;; Look up score-label.
                     ;; If no score-id or no lookup-value is present use "" instead
                     (score-label
                      (if score-id
                          (getChildOptionWithFallback '(scholarly annotate score-names)
                            (string->symbol score-id) score-id)
                          ""))
                     (grob-type (grob::name grob))
                     (grob-label
                      (getChildOptionWithFallback
                       '(scholarly annotate grob-names) grob-type (symbol->string grob-type)))
                     )
                    ;; 'context-id is already present so we overwrite it
                    (assq-set! annotation 'context-id context-id)
                    ;; Add the new properties to the annotation
                    (append! annotation
                      `((context-label . ,context-label)
                        (score-id . ,score-id)
                        (score-label . ,score-label)(grob-type . ,grob-type)
                        (grob-label . ,grob-label)))
                    ;; record annotated grob
                    (set! annotated-grobs (cons grob annotated-grobs))))))))
       all-grobs all-contexts)
      ;; reset all-grobs for next timestep
      (set! all-grobs '())
      (set! all-contexts '()))

     ((finalize trans)
      ;; Iterate over annotated grobs and produce a sorted list of annotations
      ;; (when annotations are neither printed nor logged the list is empty).
      (let
       ((annotations
         (sort-annotations
          (map
           (lambda (g)
             (let*
              ((annotation (ly:grob-property g 'input-annotation))
               ;; Retrieve location info, which seems to be possible only now
               (grob-location
                (grob-location-properties g
                  `((meter . ,(ly:context-property context 'timeSignatureFraction))))))
              (append annotation `((grob-location . ,grob-location)))))
           annotated-grobs))))

       ;; Export iterating over all entries in the
       ;; annotation-export-targets configuration list
       (for-each
        (lambda (target)
          (let
           ((er
             (assq-ref
              (getOption '(scholarly annotate internal export-routines))
              target)))
           ;; skip invalid entries
           (if er (er annotations)
               (ly:warning (format "Invalid annotation export target: ~a. Skipping!" target)))))
        (getOption '(scholarly annotate export-targets))))))))
