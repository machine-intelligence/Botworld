Botworld.pdf: Botworld.tex Botworld.bib
	pdflatex Botworld.tex
	bibtex Botworld
	pdflatex Botworld.tex
	pdflatex Botworld.tex

Botworld.tex: Botworld.lhs
	lhs2TeX -o Botworld.tex Botworld.lhs

.PHONY: clean cleanall
clean:
	rm -rf Botworld.tex Botworld.aux Botworld.log Botworld.ptb Botworld.toc Botworld.bbl Botworld.blg Botworld.out

cleanall:
	make clean
	rm -rf Botworld.pdf
