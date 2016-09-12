MAIN=main

all : $(MAIN).pdf $(MAIN).8.pdf

%.pdf : %.tex
	pdflatex $<

%.8.pdf : %.pdf
	pdfjam --nup 2x4 --frame true --noautoscale false --delta '0.2cm 0.3cm' --scale 0.95 $< -o $@

clean :
	rm -f *.ps *.pdf

.PHONY : $(MAIN).tex