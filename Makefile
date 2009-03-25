all: sltester brtester bropt brcp brcond brst bror

clean:
	$(RM) -f *.o *.hi SetLangParser.hs SetLangLexer.hs

SETLANG=SetLangParser.hs SetLangLexer.hs SetLang.hs
BASERULE=BaseRule.hs RuleParser.hs
GHC=ghc -O2 -funfolding-use-threshold=16
#-prof -auto-all -caf-all -fforce-recomp

SetLangLexer.hs: SetLangLexer.x
	alex SetLangLexer.x -o SetLangLexer.hs

SetLangParser.hs: SetLangParser.y
	happy -o SetLangParser.hs SetLangParser.y

sltester: Main.hs $(SETLANG)
	$(GHC) --make -o sltester Main.hs

brtester: BRMain.hs $(SETLANG) $(BASERULE)
	$(GHC) --make -o brtester BRMain.hs

brst: BRSet.hs $(SETLANG) $(BASERULE)
	$(GHC) --make -o brst BRSet.hs

bropt: BROptimizer.hs $(SETLANG) $(BASERULE) BRApprox.hs
	$(GHC) --make -o bropt BROptimizer.hs

brcp: BRCp.hs BRCopy.hs $(SETLANG) $(BASERULE)
	$(GHC) --make -o brcp BRCp.hs

brcond: BRByCond.hs BRCopy.hs $(SETLANG) $(BASERULE)
	$(GHC) --make -o brcond BRByCond.hs

bror: BROptRule.hs BRCopy.hs $(SETLANG) $(BASERULE)
	$(GHC) --make -o bror BROptRule.hs
