
all: zl.pdf

.DELETE_ON_ERROR: zl.pdf
zl.pdf: zl.tex zl.bib body.tex acknowledge.tex
	-rm *.aux
	pdflatex -file-line-error -halt-on-error zl.tex
	bibtex zl
	pdflatex zl.tex
	pdflatex zl.tex


clean:
	rm -f zl.pdf

clobber:
	rm -f  *.blg *.dvi* *.dw *.ilg \
          *.log *.o *.tmp *.old *~ \#* core
	rm -f *.aux *.bbl *.idx *.ind \
          *.toc *.lof *.lot *.pdf *.ps
	rm -f *.out


