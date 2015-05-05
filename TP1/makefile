7114-20151-Churca-Gamarra-TP1.pdf: 7114-20151-Churca-Gamarra-TP1.tex tp1a.out tp1b.out
	pdflatex 7114-20151-Churca-Gamarra-TP1.tex
	pdflatex 7114-20151-Churca-Gamarra-TP1.tex

tp1a.out: tp1a.lp
	echo "take tp1a.lp\ndivert tp1a.out\ngo\nrvrt\nquit" | lingo

tp1b.out: tp1b.lp
	echo "take tp1b.lp\ndivert tp1b.out\ngo\nrvrt\nquit" | lingo

tidy:
	rm -f *.aux *.lof *.log *.out *.toc

clean: tidy
	rm -f *.pdf

.PHONY: clean tidy

