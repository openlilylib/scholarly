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
% It is a thin wrapper around stylesheets.span

\loadModule stylesheets.span
\loadModule scholarly.annotate

\include "config.ily"


% Predicate for the type of editorial markup.
% While \span generally can accept arbitrary names
% we have to be more strict here in order to avoid
% ambiguity and especially complications if we should
% ever want to export to MEI.
%
% Also, the attributes module implements checks for
% the interdependency of types and attributes.
#(define (ed-markup-type? obj)
   (and
    (symbol? obj)
    (member obj
      '(lemma           ;; <lem>, preferred reading
         reading        ;; <rdg>, alternative reading from different source
         addition       ;; <add>, addition *in the source*
         deletion       ;; <del>, deletion *in the source*
         restoration    ;; <restore>, restoration of a deleted text *in the source*
         original       ;; <orig>, original (but not erroneous) text
         regularization ;; <reg>, regularized (but not corrected) text
         gap            ;; <gap>, missing material
         sic            ;; <sic>, erroneous text in the source
         unclear        ;; <unclear>, unclear text
         correction     ;; <corr>, editorial emendation
         ))))

% Load data about the different span-types:
% - default styling colors
% - default styling functions (?)
% - rules about allowed combinations
\loadModule scholarly.attributes

% Encode a source finding or editorial decision
% This is a thin wrapper around \span from the stylesheets.span module
% that essentially is limited to a set of predefined span classes
% while providing colors and (some) default styling functions
editorialMarkup =
#(define-music-function (span-class attrs mus)
   (ed-markup-type? (ly:context-mod?) ly:music?)
   (if attrs
       ;; (span invokes the \span music-function from
       ;; stylesheets.span, not the Scheme function
       (tagSpan span-class attrs mus)
       (tagSpan span-class mus)))