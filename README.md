# ScholarLY

*ScholarLY* is a toolbox for creating scholarly editions of musical scores
with (mainly) GNU LilyPond.
It belongs to *openLilyLib*, LilyPond's community library system.
Therefore getting and installing *ScholarLY* is automatically handled by
preparing *openLilyLib*.
See its [Wiki page](https://github.com/openlilylib/oll-core/wiki) for details.

As part of *openLilyLib*, this library is released under the
GNU General Public License. See *openLilyLib*'s license for details.

## Overview

*ScholarLY* includes a growing number of features. Currently, `annotate` and `editorial-functions` are implemented. Additionally, an initial version of the *scholarLY* LaTeX package will soon be published.

See the `README`s located in the respective subdirectories for more information on those utilities.

## Getting Started

Prerequisite to using *ScholarLY* is activating *openLilyLib* with

```lilypond
\include "oll-core/package.ily"
```

*openLilyLib* will only be initialized once so it is safe to use this command in multiple
initialization files. ScholarLY is loaded with *openLilyLib*'s `\loadPackage` utility:

```lilypond
\loadPackage \with {
  modules = annotate
} scholarly
```

Inside the command, as the above example shows, we declare which modules should be loaded. Currently, `annotate` and `editorial-functions` are the available modules. `annotate` implicitly loads `editorial-functions`, so it is not necessary to list both, though it is of course possible.

Once *scholarLY* is running, its behaviour can be configured using *openLilyLib*'s
global configuration mechanism. Please refer to the manuals of both for more information.
