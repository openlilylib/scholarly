# Documentation

This directory contains the sources need to compile scholarLY's documentation.
Prior to building it, you must have [Sphinx](http://sphinx-doc.org) and [LilyPondLexer](https://github.com/jefferyshivers/lilypondlexer) installed on
your system.

Any form of documentation provided by Sphinx is possible with `make <builder>`
where *builder* matches one of the [available methods](http://www.sphinx-doc.org/en/stable/builders.html).
A shorter list is provided by doing `make help` or simply `make` without any arguments.

---

Before building for the first time, run `make scholarly-examples` to compile the
examples with LilyPond. Since the examples actually use the `oll-core` and
`scholarly` libraries, those will have to be included by the `lilypond` command,
either by setting an alias with the include option or by adding the path to
lilypond's main script wherever you've installed it, or by using the `lilyopts`
argument.

```bash
cd scholarly/doc
make -lilyopts "-I path/to/openlilylib/stuff" scholarly-examples
```

Once that is run for the first time with no errors, you shouldn't have to run it
again unless the example source files are changed.

The documents are still compilable without the images. To sweep examples from the
source directory, do:

```
cd scholarly/doc/examples
rm */*.png
```

And to clean images from the build directories, the easiest way is to remove
either the respective subdirectory or the entire build directory. *Sphinx* will
remake them once the documentations are rebuilt.

---

You can find the generated documentations in `doc/build/<builder>`. Sphinx will
create the `build` and each `<builder>` directory if they aren't already there.

```bash
# Open pdf produced by `latexpdf`:
cd build/latex
open scholarly.pdf

cd ../..
# Serve the `html` docs
cd build/html
http-server   # or whatever your preferred method
```

scholarLY is tested with `html`, `epub` and `latex`/`latexpdf` builds.

To report bugs or request enhancements or clarification on any part of this,
please visit the issue tracker of this repository.
