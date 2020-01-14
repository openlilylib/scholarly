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

% scholarly.editorial-markup.cancellation module
%
% provides functions to display cancellations
% - \cancelStaff
%   Cancellation of a whole staff with diagonal strikes
% Planned:
% - \erasure
%   visual effect for music enclosed in an expression
%  (maybe to be done with/as an editorial-markup span definition)
% - \cancel
%   cancel individual elements or a range
%   if an 'item' property is present produce cancellation strokes for that item
%   (e.g. NoteHead, Stem, Beam etc.)
%   otherwise determine the horizontal range of all included NoteColumns and
%   apply the cancellation to that (as opposed to the whole staff


% \cancelStaff implementation

%{
  TODO:
  - proper centering
  - vertical protrusion
  - max/fixed angle
  - line style (=> use \line instead of path)
  - line end?
  - color (default #f not black)
  - apply annotation to that
    (it is possible already, but coloring is not applied)
  - starting point (first note, staff symbol)
  - check behaviour/calculations with indent
  - stroke direction: up, down, crossed, criss-cross
%}

#(define (cancel-current-staff grob props)
   "Set the stencil of grob to a markup that cancels a staff with a given number of diagonal strikes"
   (let*
    ((line-count (assq-ref props 'line-count))
     (overlap (assq-ref props 'overlap))
     (line-thickness (assq-ref props 'line-thickness))
     (protrusion (assq-ref props 'protrusion))
     (left-protrusion (car protrusion))
     (right-protrusion (cdr protrusion))
     (sys (ly:grob-system grob))
     ;; First calculate some values
     (sys-extent (ly:grob-property sys 'X-extent))
     (sys-x-offset (car sys-extent))
     (sys-width (- (cdr sys-extent)(car sys-extent)))
     (sys-x-rel (- (ly:grob-relative-coordinate grob sys X)(car sys-extent)))
     ;; Start the grob along with the position of the clef
     (start-x
      (- (oll:system-starters-width grob) left-protrusion))
     ;; Width of the resulting stencil
     (total-width (+ (- sys-width start-x) right-protrusion))
     ;; Bare width of individual strikethrough line elements
     (stencil-offset-unit (/ total-width line-count))
     ;; Width of individual strikethrough line elements
     (stencil-width
      (+ stencil-offset-unit (* 2 overlap)))
     ;; Function to create a single strikethrough line
     (line-stencil
      (lambda (grob)
        (ly:stencil-translate-axis
         (grob-interpret-markup grob
           (markup
            #:override `(thickness . ,line-thickness)
            #:translate '(0 . -2.5)
            #:draw-line `(,stencil-width . 5)
            ))
         (- start-x sys-x-rel overlap)
         X
         )))
     ;; Empty stencil as base
     (stil empty-stencil)
     )
    ;; Combine stencils
    (for-each
     (lambda (index)
       (set! stil
             (ly:stencil-add
              stil
              (ly:stencil-translate-axis
               (line-stencil grob)
               (* index stencil-offset-unit)
               X)))
       )
     (iota line-count))
    (ly:grob-set-property! grob 'stencil stil)
    (ly:grob-set-property! grob 'layer 4)
    ))

% Strike through the staff with three regular lines.
% NOTE: It seems sufficient *in this case* to ignore the issue
% of line steepness. However, for a more general solution this
% would have to be taken care of, by either specifying an angle
% (and have the horizontal constellation be dependent on that)
% or specify an allowed range of angles where the current approach
% would be used.
%

\registerOption scholarly.editorial-markup.cancellation.cancel-staff.line-count 5
\registerOption scholarly.editorial-markup.cancellation.cancel-staff.overlap 0.5
\registerOption scholarly.editorial-markup.cancellation.cancel-staff.line-thickness 3
\registerOption scholarly.editorial-markup.cancellation.cancel-staff.protrusion #'(0 . 0)

#(define (get-cancel-staff-option name)
   (getOption `(scholarly editorial-markup cancellation cancel-staff ,name)))

cancelStaff =
#(with-options define-music-function (options music)((ly:context-mod?) ly:music?)
   `(strict
     (? line-count ,integer? ,(get-cancel-staff-option 'line-count))
     (? overlap ,number? ,(get-cancel-staff-option 'overlap))
     (? line-thickness ,number? ,(get-cancel-staff-option 'line-thickness))
     (? protrusion ,pair? ,(get-cancel-staff-option 'protrusion))
     )
   #{
     <<
       {
         #music
       }
       \new Voice \absolute {
         \once \override Rest.after-line-breaking =
         #(lambda (grob) (cancel-current-staff grob props))
         g'1*0 \rest
       }
     >>
   #})
