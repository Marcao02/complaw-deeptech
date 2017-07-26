all:
	happy -gca ParBurger.y
	alex -g LexBurger.x
	ghc --make TestBurger.hs -o TestBurger

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

distclean: clean
	-rm -f DocBurger.* LexBurger.* ParBurger.* LayoutBurger.* SkelBurger.* PrintBurger.* TestBurger.* AbsBurger.* TestBurger ErrM.* SharedString.* ComposOp.* burger1.dtd XMLBurger.* Makefile*
	
