% preamble

% available (default) functions for editorial commands

% todo: this should be expanded to include functions for affecting multiple
% items. for example, music-map would parenthesize multiple items in an
% expression, when  we might rather want to parenthesize the whole section.
% (maybe this isn't a great example though, since a function fo
% (satisfactorily) parenthesize a section doesn't exist yet anyway).


% If more categories are needed than scholarLY
% initially provides, it will accept new ones
% added by end user in this manner:
% \registerOption scholarly.editorial.functions.<category> #`()

\registerOption scholarly.editorial.functions.addition
#`((NoteHead . ,parenthesize)
   (Slur . ,slurDashed))

\registerOption scholarly.editorial.functions.deletion
#`((NoteHead . ,parenthesize)
   (Slur . ,slurDotted))

\registerOption scholarly.editorial.functions.emendation
#`((NoteHead . ,parenthesize)
   (Slur . ,slurDashed))

