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

% Configuration for the scholarly.choice module

% Validator functions for the built-in choice types.
% Custom functions have to be created using the (define-choice-validator) macro
\registerOption scholarly.choice.choice-type-validators
#`((variants . ,validate-variants)
   (normalization . ,validate-normalization)
   (substitution . ,validate-substitution)
   (emendation . ,validate-emendation))

% Default preferences for the built-in choice types
\registerOption scholarly.choice.preferences
#`((variants . lemma)
   (normalization . regularization)
   (substitution . new)
   (emendation . new))

% Chooser functions for the built-in choice types.
% Custom functions have to be created using the (define-span-chooser) macro
\registerOption scholarly.choice.choosers
#`((variants . ,choose-variants)
   (normalization . ,choose-normalization)
   (substitution . ,choose-substitution)
   (emendation . ,choose-emendation))

% store a single validator function
setChoiceValidator =
#(define-void-function (choice-type validator)(symbol? procedure?)
   (setChildOption
    '(scholarly choice choice-type-validators) choice-type validator))

% store multiple validator functions at once
setChoiceValidators =
#(define-void-function (validators)(validator-list?)
   (for-each
    (lambda (validator)
      (setChildOption '(scholarly choice choice-type-validators)
        (car validator)(cdr validator)))
    validators))

setSpanChooser =
#(define-void-function (choice-type chooser)(symbol? procedure?)
   (setChildOption '(stylesheets choice choosers) choice-type chooser))

setChoicePreference =
#(define-void-function (choice-type preference)(symbol? scheme?)
   (setChildOption '(scholarly choice preferences) choice-type preference))
