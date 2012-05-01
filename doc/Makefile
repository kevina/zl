
all: zl.pdf

.DELETE_ON_ERROR: zl.pdf
zl.pdf: zl.tex zl.bib body.tex acknowledge.tex copyright.tex fig.tex\
  parsing-overview.pdf
	-rm *.aux
	pdflatex -file-line-error -halt-on-error zl.tex
	bibtex zl
	pdflatex zl.tex
	pdflatex zl.tex

parsing-overview.png: parsing-overview-fig.tex parsing-overview.pdf
	pdftops -eps parsing-overview.pdf
	latex parsing-overview-fig.tex
	dvipng -T tight -bg Transparent parsing-overview-fig.dvi -o parsing-overview.png

fig.png: fig0.tex fig.tex
	latex fig0.tex
	dvipng -T tight -bg Transparent fig0.dvi -o fig.png

zl.html: zl.pdf parsing-overview.png fig.png zl-html.cfg
	htlatex zl.tex zl-html,2

clean:
	rm -f  *.blg *.dvi* *.dw *.ilg \
          *.log *.o *.tmp *.old *~ \#* core
	rm -f *.aux *.bbl *.idx *.ind \
          *.toc *.lof *.lot zl.pdf *.ps *.eps
	rm -f *.out
	rm -f *.4ct *.4tc *.png *.html *.lg *.xref *.idv *.css
