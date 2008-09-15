module RuleParser where

import SetLang;
import SetLangParser;
import BaseRule;
import Text.ParserCombinators.Parsec;
import Text.ParserCombinators.Parsec.Pos;
import Data.Set as S;
import Data.Map as M;
import System.IO.Unsafe;

type SetOfChars = Set Char

data ParserState = PS {
		classes :: Map Char SetOfChars,
		rules :: [BaseRule],
		start :: String
} deriving (Eq, Show, Read)

readRuleFile :: FilePath -> String
readRuleFile fn = unsafePerformIO $ readFile fn

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
setofchars = do x <- word; return $ S.fromList x


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
		updateState $ \z -> z { classes = M.alter (\x -> Just cs) cls (classes state) }
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
		return count

metaphrase = do
		try classphrase
		<|> try includephrase
		<|> startphrase

rulephrase' :: GenParser Char ParserState Int
rulephrase' = do 
		char ':'; metaphrase
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
extracondition = do try extracondition' <|> return nopExpression


singlerule = do
		u <- word; whitespace
		l <- word; whitespace
		lc <- word; whitespace
		rc <- word
		cond <- try extracondition
		state <- getState
		updateState (\x -> x { rules = ((BaseRule u l lc rc cond) : (rules state)) })
		return 1
		<?> "rule"
	
debug :: (Monad m) => String -> m ()
debug str = do return $ unsafePerformIO $ putStrLn str

