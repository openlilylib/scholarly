
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
SPHINXPROJ    = scholarly
SOURCEDIR     = source
BUILDDIR      = build

# Put it first so that "make" without argument is like "make help".
help:
	@echo "\033[1m--------------"
	@echo "scholarLY v1.0"
	@echo "--------------\n"
	@echo "\033[0m- Make the doc's music examples with \`make scholarly-examples\`."
	@echo "- Include an extra path to that with \`make lilypath=\"\033[1;34mpath/to/stuff\033[0m\" scholarly-examples\`."
	@echo "\033[0m- Add any other options to that with \`make lilyopts=\"\033[1;34m [options] \033[0m\" scholarly-examples\`.\n"
	@echo "\033[1;32mWith Sphinx installed, follow the instructions below to build the rest of the documentation.\033[0m\n"
	@$(SPHINXBUILD) -M help "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)

# make the music examples.
# This assumes `lilypond` already includes the path to oll-core and scholarly
# Inc path can set with lilypath="/path/" or with other options in lilyopts="[opts]"
lilypath =
lilyopts =
exInd = examples/index
exAnn = examples/annotate
exEdf = examples/editorial-functions
scholarly-examples:
	@echo "\033[1;32m\nCompiling examples ...\033[0m"
	@echo "\033[1;32m\n~~~ index examples ~~~\033[0m"
	@for f in $(exInd)/*.ly ; do \
		echo "\n\033[1mCompiling: $$f\033[0m" ; \
		lilypond -I "$(lilypath)" $(lilyopts) --format=png -o $(exInd) $$f ; \
	done
	@echo "\033[1;32m\n~~~ annotate examples ~~~\033[0m"
	@for f in $(exAnn)/*.ly ; do \
		echo "\n\033[1mCompiling: $$f\033[0m" ; \
		lilypond -I "$(lilypath)" $(lilyopts) --format=png -o $(exAnn) $$f ; \
	done
	@echo "\033[1;32m\n~~~ editorial-functions examples ~~~\033[0m"
	@for f in $(exEdf)/*.ly ; do \
		echo "\n\033[1mCompiling: $$f\033[0m" ; \
		lilypond -I "$(lilypath)" $(lilyopts) --format=png -o $(exEdf) $$f ; \
	done
	@echo "\033[1;32m\n~~~ latex examples ~~~\033[0m"
	lilypond -I "$(lilypath)" $(lilyopts) --format=png -o examples/latex examples/latex/min-lp.ly
	@echo "\033[1m\nRemoving extraneous files ...\n\033[0m"
	@rm examples/*/*.log
	@rm examples/annotate/*.tex
	@rm examples/*/*.texi
	@rm examples/*/*.eps
	@rm examples/*/*.count
	@echo "\033[1;32mSuccess: images are located in ./examples/* and will now be added to the documentation in future builds.\n\033[0m"

.PHONY: help Makefile

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
%: Makefile
	@$(SPHINXBUILD) -M $@ "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS) $(O)
