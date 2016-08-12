# scholarLY latex package

The scholarLY LaTeX package provides the functionality to process the `.inp` file exported from LilyPond using the LaTeX typesetting system. It offers the possibility to impliment a (growing) range of custom styles and formats, and simple tools for organizing multiple concurrent modes of customization.

## Obtaining the Package

- The package directory can be obtained directly from CTAN (once published), or through your TeX distribution.
- It can also be cloned with git from the GitHub repository at https://github.com/openlilylib/scholarly/latex-package.

## Installation

- **from TeXlive**
TODO (describe options to either update entire distro, or say to otherwise find it specifically using respective distro procedure).

- **otherwise**
If you have obtained the package directly from CTAN or the github repository, you can build the package file by running LaTeX on the `.ins` file:
```
> latex scholarly.ins
```
This should produce a file called `scholarLY.sty`. Be sure to point LaTeX to wherever this directory is located, if that isn't already in the search path.

## Documentation

To create the package documentation, which includes specific usage instructions and (inline) description of the code, run pdflatex on `scholarly.dtx`:
```
> pdflatex scholarly.dtx
```
The first pass should generate a message prompting you to run pdflatex on the same file again. This is in order to fully process the table of contents at the beginning of the file.

This should result in the creation of a file `scholarly.pdf` in the same subdirectory as the installer and docstrip files.

## Examples

An example file `annotate.tex` is located in the `usage-examples` subdirectory (NOTE: this might actually change once the package is published to CTAN). The package documentation includes additional snippets with the resulting examples displayed inline.

***

Please report any problems encountered while compiling the package or documentation, or while using the package itself, to the scholarLY issue tracker at https://github.com/openlilylib/scholarly/issues.