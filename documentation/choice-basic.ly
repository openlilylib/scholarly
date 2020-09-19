\score {
  \new Staff {
    \clef treble
    \key c \major
    \time 12/8

% excerpt from Grieg Piano sonata op. 7
    \relative c'' {
% 16
      <c c'>4.->\ff <b b'>-> <ais ais'>-> <b b'>->\> | % note the decrescendo 
here

% 17
      s1.\! | % I had to add this measure to get the hairpin printed
    }
  }
}