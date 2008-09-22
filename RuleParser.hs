module RuleParser where

import SetLang;
import SetLangParser;
import BaseRule;
import Text.ParserCombinators.Parsec;
import Text.ParserCombinators.Parsec.Pos;
import System.IO.Unsafe;
import System.IO.UTF8 as UTF8;
import Data.Map;
import Data.Set;
import Debug.Trace;

data ParserState = PS {
		classes :: Classes,
		rules :: [BaseRule],
		start :: String
} deriving (Eq, Show, Read)

emptyParserState = PS { classes = Data.Map.empty, rules = [], start = "" } 

readRuleFile :: FilePath -> String
readRuleFile fn = unsafePerformIO $ UTF8.readFile fn

parseRuleFile fn = runParser rulefile emptyParserState fn (readRuleFile fn)

rulefile :: GenParser Char ParserState (ParserState, Int)
rulefile = do
	rs <- many rulephrase
	eof
	state <- getState
	return (state, (sum rs))

		
word = do
	w <- many $ noneOf " \t\n"
	return w
whitespace = (skipMany1 $ oneOf " \t") <?> "intra word space"

eol = (skipMany1 $ char '\n') <?> "end of line"
setofchars = do x <- word; return $ Data.Set.fromList x


startphrase = do 
		string "start"
		whitespace
		ss <- word
		updateState (\x -> x { start = ss })
		return 0

classphrase = do
		string "class"
		whitespace
		cls <- Text.ParserCombinators.Parsec.upper
		whitespace
		cs <- setofchars
		state <- getState
		updateState $ \z -> z { classes = Data.Map.alter (\x -> Just cs) cls (classes state) }
		return 0

includephrase = do
		string "include"
		whitespace
		fn <- word <?> "file name"
		oldInput <- getInput
		oldPosition <- getPosition
		setInput $ readRuleFile fn
		setPosition $ newPos fn 1 1
		(state, count) <- try rulefile
		setInput oldInput
		setPosition oldPosition
		return count

comment = (many $ noneOf "\n") <?> "comment"

metaphrase = do
		try classphrase
		<|> try includephrase
		<|> startphrase

rulephrase' :: GenParser Char ParserState Int
rulephrase' = do 
		char ':'; metaphrase
		<|> do { char ' '; comment; return 0 }
		<|> do { char '\t'; comment; return 0 }
		<|> singlerule
		<|> do whitespace; return 0

rulephrase = do
	rc <- rulephrase'
	skipMany $ oneOf " \t"
	eol
	return rc

setlang = do
	slexpr <- many $ noneOf "}"
	return $ parseSetLang slexpr

extracondition' = do
		whitespace
		char '{'
		cond <- setlang
		char '}'
		return cond
extracondition defaultExpr = do try extracondition' <|> return defaultExpr

unzero ('0':_) = ""
unzero x = x

singlerule = do
		u <- word; whitespace
		l <- word; whitespace
		lc <- word; whitespace
		rc <- word
		cond <- try (extracondition nopExpression)
		elseCond <- try (extracondition noExpression)
		state <- getState
		updateState (\x -> x { 
			rules = ((BaseRule (unzero u) (unzero l) (unzero lc) (unzero rc) cond elseCond)
				: (rules state)) })
		return 1
		<?> "rule"
	
debug :: (Monad m) => String -> m ()
debug str = do return $ unsafePerformIO $ UTF8.putStrLn str

