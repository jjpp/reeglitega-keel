all: sltester brtester bropt

clean:
	$(RM) -f *.o *.hi SetLangParser.hs SetLangLexer.hs

SETLANG=SetLangParser.hs SetLangLexer.hs SetLang.hs
GHC=ghc -O3

SetLangLexer.hs: SetLangLexer.x
	alex SetLangLexer.x -o SetLangLexer.hs

SetLangParser.hs: SetLangParser.y
	happy -o SetLangParser.hs SetLangParser.y

sltester: Main.hs $(SETLANG)
	$(GHC) --make -o sltester Main.hs

brtester: BRMain.hs $(SETLANG) BaseRule.hs RuleParser.hs
	$(GHC) --make -o brtester BRMain.hs

bropt: BROptimizer.hs $(SETLANG) BaseRule.hs RuleParser.hs BRApprox.hs
	$(GHC) --make -o bropt BROptimizer.hs

