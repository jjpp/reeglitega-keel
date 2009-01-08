module Main where

import BaseRule
import RuleParser
import Control.Monad
import Debug.Trace
import Data.Set



main = do { 	putStrLn $ "Reegleid: " ++ (show $ length rs);
		putStrLn $ "Tulemusi: " ++ (show $ size x);
		putStrLn $ showstates "\n" x;
		putStrLn $ show (nonterminals ps)
	} where 
		ps = case parseRuleFile "/dev/stdin" of
			Left err -> error (show err)
			Right s -> (fst s)
		rs = reverse $ rules ps
		isIn = isInClasses $ classes ps
		startWord = start ps
		x = generate startWord isIn rs

generate w isIn rs = process (singleton initialBRState { cw = w }) isIn rs empty

process :: Set BRState -> IsIn -> [BaseRule] -> Set BRState -> Set BRState
process initialStates isIn rs seenStates =
	trace ("Processing " ++ (showstates "|" unseenStates)) $
		if Data.Set.null unseenStates then initialStates else process allStates isIn rs initialStates
	where
		allStates = union initialStates newStates
		unseenStates = difference initialStates seenStates
		newStates = unions $ Prelude.map expandState $ toList unseenStates
		expandState state = fromList $ brapply isIn state rs


--s ++ (concat $ map (\state -> process (brapply isIn state rs) isIn rs (seenStates ++ ss)) $ filter (\s -> not (s `elem` seenStates)) ss)

joinStr sep f ss = foldl (\x -> \y -> x ++ sep ++ (f y)) "" ss

showstates sep ss = joinStr sep showState $ toList ss
