.PHONY: all clean view docker-build

all: main.pdf

main.pdf: main.tex chapters/*.tex appendices/*.tex references.bib
	pdflatex -interaction=nonstopmode main.tex
	bibtex main
	pdflatex -interaction=nonstopmode main.tex
	pdflatex -interaction=nonstopmode main.tex

clean:
	rm -f *.aux *.log *.out *.toc *.lof *.lot *.bbl *.blg
	rm -f chapters/*.aux appendices/*.aux

view: main.pdf
	xdg-open main.pdf 2>/dev/null || open main.pdf 2>/dev/null || echo "PDF ready: main.pdf"

docker-build:
	docker run --rm -v $$(pwd):/work -w /work texlive/texlive:latest make all
