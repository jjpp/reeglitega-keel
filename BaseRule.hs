module BaseRule where

import Text.Regex;
import SetLang;
import SetLangParser;
import Control.Monad;
import Data.Set as S;
import Data.Map as M;
import Char;
import System.IO.Unsafe;
import Debug.Trace;

type Str = String
type SetOfChars = Set Char
type Classes = Map Char SetOfChars

data BaseRule = BaseRule { 
				upper, lower, precond, postcond :: Str, 
				matchExpr, nomatchExpr :: Expression,
				ruleName :: String, ruleId :: Int
			}
	deriving (Eq, Show, Read)

uppercond rule = (uppercond' rule) ++ (postcond rule)
lowercond rule = (lowercond' rule) ++ (postcond rule)

uppercond' rule = (precond rule) ++ (BaseRule.upper rule)
lowercond' rule = (precond rule) ++ (BaseRule.lower rule)


data BRState = BRState { cw :: Str, vs :: VarState }
	deriving (Eq, Show)



stopState :: BRState -> Bool
stopState s = "stop" `M.member` (vs s)

unStop s = s { vs = "stop" `M.delete` (vs s) }

brapply :: (MonadPlus m) => IsIn -> BRState -> [BaseRule] -> (m BRState)
brapply _ _ [] = mzero
brapply isIn state (r:rs) = case state' of
			Nothing -> rest
			Just st -> if stopState st
					then return $ unStop st
					else (return st) `mplus` rest
		where rest = brapply isIn state rs
		      state' = brapply' isIn state r
--		      state' = trace ("brapply' " ++ (showState state) ++ " " ++ (show r)) brapply' isIn state r

			

-- msum $ brfilter $ Prelude.map (trace ("state in brapply: " ++ (show state)) brapply' isIn state) rs

-- transformWord :: (Monad m) => Str -> BaseRule -> m Str
-- transformWord ws rule = return $ subRegex (mkRegex (uppercond rule)) ws (lowercond rule) 

brapply' :: (MonadPlus m) => IsIn -> BRState -> BaseRule -> m BRState
brapply' isIn state rule = case (matchState `mplus` nomatchState) of
				(Just _) -> case (transformWord isIn (cw state) rule) of
						(Just newWord) -> brapply'' newWord state matchState rule
						_ -> brapply'' oldWord state nomatchState rule
				otherwise -> fail "no match"
			where 	oldWord = cw state
				matchState = apply (matchExpr rule) (vs state)
				nomatchState = apply (nomatchExpr rule) (vs state)

brapply'' :: (MonadPlus m) => Str -> BRState -> Maybe VarState -> BaseRule -> m BRState
brapply'' word state (Just varstate') rule = do
--			dbg $ "brapply' " ++ (show state) ++ " " ++ (show rule)
			dbg $ "\tstate = " ++ (showState state)
			dbg $ "\trule was = " ++ (show rule)
			dbg $ "\tvarstate = " ++ (showVS varstate')
			dbg $ "\tword' = " ++ (show word)
			return (BRState word varstate')
brapply'' _ _ Nothing _ = fail "No match"

initialBRState = BRState "START" initialState
nopExpression = Expression CTrue []
noExpression = Expression CFalse []


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

matchHere isIn ('1':ps) pw w = matchHere isIn ps pw w -- mingite kohtade peal on poolikud numbrid?
matchHere isIn ('2':ps) pw w = matchHere isIn ps pw w

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
--			dbg $ "transformWord: " ++ (show ws) ++ " " ++ (show rule)
			match <- findMatch isIn upper' ws
--			dbg $ "transformWord: " ++ (show ws) ++ " " ++ (show rule)
			dbg $ "match = " ++ (show match)
			return ((take match ws) ++ (transform upper'' lower'' [] Nothing (drop match ws)))
		where
			upper'' = (uppercond' rule)
			lower'' = (lowercond' rule)
			upper' = (uppercond rule)

--transform' rule a b c d e = trace ("{" ++ rule ++ ": transform " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ " " ++ (show d) ++ " " ++ (show e) ++ "}") transform a b c d e
transform' rule a b c d e = transform a b c d e

-- upper, lower, lastupper, last copied, word
transform :: Str -> Str -> Str -> Maybe Char -> Str -> Str
transform ('!':us) a b c d = transform' "!:us" us a b c d
transform ('1':us) a b c d = transform' "1:us" us a b c d
transform ('2':us) a b c d = transform' "2:us" us a b c d

transform us ('!':ls) a b c = transform' "!:ls" us ls a b c
transform us ('1':ls) a b c = transform' "1:ls" us ls a b c
transform us ('2':ls) a b c = transform' "2:ls" us ls a b c

transform (u:us) [] a b (w:ws) = transform' "out of l" us [] a b ws
transform [] [] _ _ ws = ws

transform (u:us) (l:ls) x _ (w:ws) | isUpper(l) && u == l 
	= w : transform' "same class" us ls (take 2 (u:x)) (Just w) ws
transform us (l:ls) x (Just w') ws | isUpper(l) && l `elem` x 
	= w' : transform' "repeat of last" us ls x (Just w') ws
transform us (l:ls) x _ ws 
	= l : transform' "default" (drop 1 us) ls ((take 1 us) ++ (take 1 x)) (Just l) (drop (ucheck us) ws)

ucheck (x:xs) = 1
ucheck _ = 0

dbg :: (Monad m) => String -> m ()
-- dbg str = do trace str return ()
dbg str = do return ()

showState :: BRState -> String
showState s = "{ " ++ (show (cw s)) ++ " " ++ (showVS (vs s)) ++ "}"
