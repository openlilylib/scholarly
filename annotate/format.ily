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
  Helper utilities to format annotation output
%}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% General routines for formatting output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Define string representations for selected ly:music? data types.
% These are used for displaying custom properties.
#(define (format-ly-music music)
   (if (ly:music? music)
       (case  (ly:music-property music 'name)
         ((TimeSignatureMusic)
          (format #f "\\time ~a/~a"
            (ly:music-property music 'numerator)
            (ly:music-property music 'denominator)))
         ((KeyChangeEvent)
          (format #f "Key: ~a" (ly:music-property music 'tonic)))
         (else "(LilyPond Music)"))
       "No music found"))

% Returns a string with a single attribute.
% Simple formatting, not configurable yet
% Uses attribute-labels lookup if available
#(define (format-property-message prop)
   (let
    ((prop-key (car prop))
     (prop-value (cdr prop)))
    (format #f "    ~a: ~a"
      (or (getChildOptionWithFallback '(scholarly annotate attribute-labels) prop-key #f)
          prop-key)
      ; keep that a (cond) expression because there might be more special types to come
      (cond
       ((ly:music? prop-value)
        (format-ly-music prop-value))
       (else prop-value)))))

% Return a list of formatted properties.
% Suppresses attributes in a filter list
% Sorts by the resulting strings
#(define (format-property-messages ann flt)
   (let
    ((accepted (filter (lambda (prop) (not (member (car prop) flt))) ann)))
    (sort
     (map (lambda (prop) (format-property-message prop)) accepted)
     (lambda (a b) (string<? (string-downcase a) (string-downcase b))))))
