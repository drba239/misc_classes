SHELL = bash
#include ../../shell_functions.make

docs =	defs_theorems linear_regression

docs_output = $(addsuffix .pdf, $(docs))

all:	$(docs_output)

$(foreach doc, $(docs), $(doc).pdf): %.pdf: %.tex
	pdflatex -shell-escape $<
	pdflatex -shell-escape $<
	pdflatex -shell-escape $<
	rm $(basename $@).log $(basename $@).aux $(basename $@).toc $(basename $@).out || true

#include ../../generic.make