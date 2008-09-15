module RuleParser where

import SetLang;
import BaseRule;
import Text.ParserCombinators.Parsec;
import Data.Set as S;
import Data.Map as M;
import System.IO.Unsafe;

type SetOfChars = Set Char

data ParserState = PS {
		terminals :: SetOfChars,
		classes :: Map Char SetOfChars,
		rules :: [BaseRule]
} deriving (Eq, Show, Read)

readRuleFile :: FilePath -> String
readRuleFile fn = unsafePerformIO $ readFile fn

rulefile :: GenParser Char ParserState (ParserState, Int)
rulefile = do
	rs <- many1 rulephrase
	eof
	state <- getState
	return (state, (sum rs))

setofchars = do
		x <- manyTill anyChar space
		return $ S.fromList x
		

charsetphrase = do 
		string ":charset"
		skipMany1 space
		cs <- setofchars
		updateState (\x -> x { terminals = cs })
		return 0

classphrase = do
		string ":class"
		skipMany1 space
		cls <- Text.ParserCombinators.Parsec.upper
		skipMany1 space
		cs <- setofchars
		state <- getState
		updateState $ \z -> z { classes = M.alter (\x -> Just cs) cls (classes state) }
		return 0

includephrase = do
		string ":include"
		skipMany1 space
		fn <- manyTill anyChar space <?> "file name"
		oldInput <- getInput
		setInput $ readRuleFile fn
		(state, count) <- try rulefile
		setInput oldInput
		return count

emptyline = do
		skipMany $ oneOf " \t"
		return 0
		<?> "empty line"

rulephrase' :: GenParser Char ParserState Int
rulephrase' = do
		try charsetphrase
		<|> try classphrase
		<|> try includephrase
		<|> try singlerule
		<|> try emptyline

rulephrase = do rc <- rulephrase'; char '\n'; return rc

subrule cs = do
		input <- getInput
		x <- many1 $ oneOf cs
		left <- getInput
		return x

charset state = (S.toList $ terminals state) ++ (keys $ classes state) ++ "012#"

singlerule = do
		st <- getState
		u <- subrule (charset st); skipMany1 space
		l <- subrule (charset st); skipMany1 space
		lc <- subrule (charset st); skipMany1 space
		rc <- subrule (charset st)
		return 1
		<?> "rule"
	
debug :: (Monad m) => String -> m ()
debug str = do return $ unsafePerformIO $ putStrLn str

