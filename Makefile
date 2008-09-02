all: sltester

SetLangLexer.hs: SetLangLexer.x
	alex SetLangLexer.x -o SetLangLexer.hs

SetLangParser.hs: SetLangParser.y
	happy -o SetLangParser.hs SetLangParser.y

sltester: Main.hs SetLangParser.hs SetLangLexer.hs SetLang.hs
	ghc --make -o sltester Main.hs

