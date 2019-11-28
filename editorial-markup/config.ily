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

% Configuration options for scholarly.editorial-markup

% Default colors for the different types of editorialMarkup
\setChildOptions stylesheets.span.span-colors
#`((lemma . ,darkgreen)
   (reading . ,darkblue)
   (addition . ,darkyellow)
   (deletion . ,(x11-color 'DarkRed))
   (restoration . ,darkcyan)
   (original . ,(x11-color 'DarkGoldenrod))
   (regularization . ,(x11-color 'chocolate))
   (sic . ,red)
   (unclear . ,(x11-color 'orange))
   (gap . ,(x11-color 'PaleGoldenrod))
   (correction . ,(x11-color 'RoyalBlue))
   (conjecture . ,blue))

% Validators for the different \editorialMarkup annotations

% Validator function for lemma and reading
#(define validate-variant
   (define-span-validator
    (let ((valid (assq-ref annotation 'source)))
      (if (not valid)
          (set! warning-message "Missing attribute 'source'."))
      valid)))
\setChildOption stylesheets.span.validators lemma #validate-variant
\setChildOption stylesheets.span.validators reading #validate-variant

#(define validate-gap
   (define-span-validator
    (let ((valid (assq-ref annotation 'reason)))
      (if (not valid)
          (set! warning-message "Missing attribute 'reason'."))
      valid)))
\setChildOption stylesheets.span.validators gap #validate-gap

#(define validate-correction
   (define-span-validator
    (let*
     ((type
       (let ((original-type (assq-ref annotation 'type)))
         (if (and original-type (string? original-type))
             (string->symbol original-type)
             original-type)))
      (valid
       (and type
            (memq type '(addition deletion substitution)))))
     (if (not valid)
         (set! warning-message "Missing or invalid attribute 'type'.
Allowed values: 'addition', 'deletion', 'substitution'"))
     valid)))
\setChildOption stylesheets.span.validators correction #validate-correction

