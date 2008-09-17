module BaseRule where

import Text.Regex;
import SetLang;
import SetLangParser;
import Control.Monad;
import Data.Set as S;
import Data.Map as M;
import Char;
import System.IO.Unsafe;

type Str = String
type SetOfChars = Set Char
type Classes = Map Char SetOfChars

data BaseRule = BaseRule { upper, lower, precond, postcond :: Str, extra :: Expression }
	deriving (Eq, Show, Read)

uppercond rule = (precond rule) ++ (BaseRule.upper rule) ++ (postcond rule)
lowercond rule = (precond rule) ++ (BaseRule.lower rule) ++ (postcond rule)


data BRState = BRState { cw :: Str, vs :: VarState }
	deriving (Eq, Show)

brapply :: (MonadPlus m) => IsIn -> BRState -> [BaseRule] -> (m BRState)
brapply isIn state rs = msum (Prelude.map (brapply' isIn state) rs)

-- transformWord :: (Monad m) => Str -> BaseRule -> m Str
-- transformWord ws rule = return $ subRegex (mkRegex (uppercond rule)) ws (lowercond rule) 

brapply' :: (MonadPlus m) => IsIn -> BRState -> BaseRule -> m BRState
brapply' isIn state rule = do 
			varstate' <- apply (extra rule) (vs state)
			word' <- transformWord isIn (cw state) rule
			return (BRState word' varstate')

initialBRState = BRState "START" initialState
nopExpression = Expression CTrue [Nop]


type IsIn = Char -> Char -> Bool
isInClasses :: Classes -> IsIn
isInClasses cs c k = if Char.isUpper(k) then S.member c (cs ! k) else k == c

matchHere :: IsIn -> Str -> Maybe Char -> Str -> Bool

matchHere _ [] _ _ = True		-- muster sai otsa
matchHere _ (p:ps) _ [] = False		-- sõna sai otsa, muster mitte

matchHere isIn ('!':p:ps) _ (w:ws) = not (w `isIn` p) && matchHere isIn ps (Just w) ws
matchHere isIn ('1':p1:'1':ps) (Just pw) (w:ws) = 
	(w `isIn` p1) && (pw == w) && matchHere isIn ps (Just w) ws
matchHere isIn ('1':p1:'2':ps) (Just pw) (w:ws) = 
	(w `isIn` p1) && (pw /= w) && matchHere isIn ps (Just w) ws

matchHere isIn (p:ps) _ (w:ws) = (w `isIn` p) && matchHere isIn ps (Just w) ws


findMatch :: (MonadPlus m) => IsIn -> Str -> Str -> m Int
findMatch = findMatch' 0
findMatch' _ isIn [] _ = return 0
findMatch' _ isIn (p:ps) [] = fail "no match"
findMatch' x isIn ps ww@(w:ws) = mplus 
		(findMatch' (x+1) isIn ps ws) 
		(if (matchHere isIn ps Nothing ww) then return x else fail "no match")

transformWord :: (MonadPlus m) => IsIn -> Str -> BaseRule -> m Str
transformWord isIn ws rule = do
			match <- findMatch isIn upper' ws
			return ((take match ws) ++ (transform upper' lower' [] Nothing (drop match ws)))
		where
			upper' = (uppercond rule)
			lower' = (lowercond rule)

-- upper, lower, lastupper, last copied, word
transform :: Str -> Str -> Str -> Maybe Char -> Str -> Str
transform _ [] _ _ ws = ws
transform ('!':us) a b c d = transform us a b c d
transform ('1':us) a b c d = transform us a b c d
transform ('2':us) a b c d = transform us a b c d

transform us ('!':ls) a b c = transform us ls a b c
transform us ('1':ls) a b c = transform us ls a b c
transform us ('2':ls) a b c = transform us ls a b c

transform (u:us) (l:ls) x _ (w:ws) | isUpper(l) && u == l 
	= w : transform us ls (take 2 (u:x)) (Just w) ws
transform us (l:ls) x (Just w') ws | isUpper(l) && l `elem` x 
	= w' : transform (drop 1 us) ls ((take 1 us) ++ (take 1 x)) (Just w') (drop 1 ws)
transform us (l:ls) x _ ws 
	= l : transform (drop 1 us) ls ((take 1 us) ++ (take 1 x)) (Just l) (drop 1 ws)

dbg :: (Monad m) => String -> m ()
dbg str = do return $ unsafePerformIO $ putStrLn str

