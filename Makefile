all: sltester brtester bropt brcp

clean:
	$(RM) -f *.o *.hi SetLangParser.hs SetLangLexer.hs

SETLANG=SetLangParser.hs SetLangLexer.hs SetLang.hs
BASERULE=BaseRule.hs RuleParser.hs
GHC=ghc -O3

SetLangLexer.hs: SetLangLexer.x
	alex SetLangLexer.x -o SetLangLexer.hs

SetLangParser.hs: SetLangParser.y
	happy -o SetLangParser.hs SetLangParser.y

sltester: Main.hs $(SETLANG)
	$(GHC) --make -o sltester Main.hs

brtester: BRMain.hs $(SETLANG) $(BASERULE)
	$(GHC) --make -o brtester BRMain.hs

bropt: BROptimizer.hs $(SETLANG) $(BASERULE) BRApprox.hs
	$(GHC) --make -o bropt BROptimizer.hs

brcp: BRCopy.hs $(SETLANG) $(BASERULE) BRCopy.hs
	$(GHC) --make -o brcp BRCopy.hs
