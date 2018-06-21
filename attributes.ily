\version "2.19.80"

% Place to store default attributes for choice and edit commands.

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
   (gap . ,(x11-color 'PaleGoldenrod))
   (emendation . ,(x11-color 'RoyalBlue)))
