
all: zl.pdf

.DELETE_ON_ERROR: zl.pdf
zl.pdf: zl.tex zl.bib body.tex acknowledge.tex copyright.tex fig.tex
	-rm *.aux
	pdflatex -file-line-error -halt-on-error zl.tex
	bibtex zl
	pdflatex zl.tex
	pdflatex zl.tex

fig.png: fig0.tex fig.tex
	latex fig0.tex
	dvipng -T tight -bg Transparent fig0.dvi -o fig.png

zl.html: zl.pdf fig.png zl-html.cfg
	htlatex zl.tex zl-html,2

clean:
	rm -f  *.blg *.dvi* *.dw *.ilg \
          *.log *.o *.tmp *.old *~ \#* core
	rm -f *.aux *.bbl *.idx *.ind \
          *.toc *.lof *.lot *.pdf *.ps
	rm -f *.out
	rm -f *.4ct *.4tc fig.png *.html *.lg *.xref *.idv *.css
