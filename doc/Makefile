
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
SPHINXPROJ    = scholarly
SOURCEDIR     = source
BUILDDIR      = build

# Put it first so that "make" without argument is like "make help".
help:
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

# make the music examples.
# This assumes `lilypond` already includes the path to oll-core and scholarly
lilyopts = 
exAnn = examples/annotate
exEdf = examples/editorial-functions
scholarly-examples:
	@echo "Compiling examples ..."
	@echo "\n~~~ annotate examples ~~~\n"
	lilypond -dclip-systems --format=png -o $(exAnn) $(exAnn)/min.ly
	lilypond -dbackend=eps --format=png --output=$(exAnn) $(exAnn)/footnotes-01.ly
	lilypond -dbackend=eps --format=png --output=$(exAnn) $(exAnn)/footnotes-02.ly
	lilypond -dclip-systems --format=png --output=$(exAnn) $(exAnn)/balloon-text-01.ly
	lilypond -dclip-systems --format=png --output=$(exAnn) $(exAnn)/balloon-text-02.ly
	@echo "\n~~~ editorial-functions examples ~~~\n"
	lilypond -dclip-systems --format=png --output=$(exEdf) $(exEdf)/min.ly
	lilypond -dclip-systems --format=png --output=$(exEdf) $(exEdf)/shorthand.ly
	lilypond -dclip-systems --format=png --output=$(exEdf) $(exEdf)/sections.ly
	@echo "\nRemoving extraneous files ...\n"
	rm examples/*/*.log
	rm examples/*/*.tex
	rm examples/*/*.texi
	rm examples/*/*.eps
	rm examples/*/*.count

.PHONY: help Makefile

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
%: Makefile
	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)
