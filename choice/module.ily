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
% Copyright Urs Liska, 2018                                                   %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\version "2.19.80"

% scholarly.choice module

\loadModule scholarly.editorial-markup
\loadModule scholarly.annotate

\include "validate.ily"
\include "choose.ily"
\include "config.ily"

% Merge the annotation attributes from the \choice
% with that of the chosen music expression.
% If an attribute is set in both annotations
% content expression overwrites \choice.
#(define (merge-annotations props music)
   (let*
    ((anchor (get-anchor music))
     (span-annotation (ly:music-property anchor 'span-annotation)))
    (for-each
     (lambda (prop)
       (let*
        ((is-set (assq (car prop) span-annotation)))
        (if (not is-set)
            (set! span-annotation (append span-annotation (list prop))))))
     props)
    (make-input-annotation span-annotation anchor)))

choice =
#(define-music-function (choice-type attrs music)
   (symbol? (ly:context-mod?) choice-music?)
   (let*
    ((props (if attrs (context-mod->props attrs) '()))
     (chosen (choose-element choice-type props music)))
    (merge-annotations props chosen)
    chosen))
