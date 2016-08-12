# scholarLY latex package

The scholarLY LaTeX package provides the functionality to process the `.inp` file exported from LilyPond using the LaTeX typesetting engine / software.

## obtaining the package

- The package directory can be obtained directly from CTAN (once published), or through your TeX distribution.
- It can also be cloned from the git repository.

## installing

- If you have obtained the package directly from CTAN or the github repository, you can build the package file by running LaTeX on the `.ins` file:
```
> latex scholarly.ins
```
This should produce a file called `scholarLY.sty`.

- To create the documentation, which includes specific usage instructions and (inline) description of the code, run pdflatex on `scholarly.dtx`:
```
> pdflatex scholarly.dtx
```
The first pass should generate a message instructing you to run pdflatex on the same file again. This is in order to fully process the table of contents at the beginning of the file.

This should result in the creation of a file `scholarly.pdf` in the same subdirectory as the installer and docstrip files.