all:
	happy -gca ParKlatte.y
	alex -g LexKlatte.x
	latex DocKlatte.tex; dvips DocKlatte.dvi -o DocKlatte.ps
	ghc --make TestKlatte.hs -o TestKlatte
	ghc --make interpreter.hs -o interpreter
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocKlatte.ps
distclean: clean
	-rm -f DocKlatte.* LexKlatte.* ParKlatte.* LayoutKlatte.* SkelKlatte.* PrintKlatte.* TestKlatte.* AbsKlatte.* TestKlatte ErrM.* SharedString.* Klatte.dtd XMLKlatte.* Makefile*

