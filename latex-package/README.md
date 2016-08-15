# scholarLY latex package

The primary aim of the scholarLY LaTeX package is to produce beautifully typeset annotations for critical editions musical scores engraved with the GNU LilyPond notation software. This package provides the functionality to process the `.inp` file exported from LilyPond using the `scholarLY` library, a project of the openLilyLib organization. It offers the possibility to impliment a (growing) range of custom styles and formats, and simple tools for organizing multiple concurrent modes of customization.

## Installation

**Using TeXlive** :
The ideal way to obtain the package is through your TeX distribution. Updating your entire distro will add `scholarLY` if it isn't already there, and of course it can be individually retrieved through the procedure relevant to your distro.

**Otherwise** :
You can download the package directly from CTAN or `git clone` (https://github.com/openlilylib/scholarly/latex-package). If you already have a copy of the `scholarLY` repository on your system, the latest package version will be contained there in the `latex-package` subdirectory.

Build the package file by running LaTeX on the installer file:
```
latex scholarly.ins
```
This should produce a file called `scholarly.sty` in that same directory. Be sure to point LaTeX to wherever this directory is located, if that isn't already in the search path (which is likely the case if you obtained it any means other than through TeXlive).

## Documentation

To create the package documentation, which includes specific usage instructions and (inline) explanation of the code, run pdflatex on `scholarly.dtx`:
```
pdflatex scholarly.dtx
```
The first pass should generate a message prompting you to run pdflatex on the same file again. This is in order to fully process the table of contents at the beginning of the file.

This should result in the creation of a file `scholarly.pdf` in the same subdirectory as the installer and docstrip files.

## Examples

An example file `annotate.tex` is located in the `usage-examples` subdirectory (NOTE: this might actually change once the package is published to CTAN). The package documentation includes additional snippets with the resulting examples displayed inline.

***

Please report any problems encountered while compiling the package or documentation, or while using the package itself, to the scholarLY issue tracker at https://github.com/openlilylib/scholarly/issues.
