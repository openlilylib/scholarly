\version "2.19.22"

\include "oll-core/package.ily"

\loadModule scholarly.diplomatic-line-breaks

\markup \vspace #1
{
  s1*2 \mark \default
  s1*2
  \diplomaticLineBreak
  s1
  s2.
    \diplomaticLineBreak
    s4
  s1*2 \mark \default
  s1
}