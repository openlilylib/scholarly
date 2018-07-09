
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

\version "2.19.22"

\loadModule oll-core.util.consist-to-contexts
\loadModule oll-core.util.grob-location

% Global object storing all annotations
\registerOption scholarly.annotations #'()

% Include implementation
\include "config.ily"
\include "sort.ily"
\include "format.ily"
\include "export.ily"
\include "export-latex.ily"
\include "export-plaintext.ily"
\include "engraver.ily"


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Set default integration in the layout contexts.
%%%% All settings can be overridden in individual scores.

% annotationCollecter is included in all Bottom contexts
\consistToContexts #annotationCollector
#'(ChordNames
   CueVoice
   DrumVoice
   Dynamics
   FiguredBass
   GregorianTranscriptionVoice
   KievanVoice
   Lyrics
   MensuralVoice
   NoteNames
   NullVoice
   PetrucciVoice
   TabVoice
   VaticanaVoice
   Voice)

\layout {
  \context {
    \Score
    % The annotation processor living in the Score context
    % processes the annotations and outputs them to different
    % targets.
    \consists \annotationProcessor
  }
}
