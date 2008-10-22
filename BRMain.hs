module Main where

import BaseRule
import RuleParser
import Control.Monad
import Debug.Trace



main = do { 	putStrLn $ "Reegleid: " ++ (show $ length rs);
		putStrLn $ "Tulemusi: " ++ (show $ length x);
		putStrLn $ showstates "\n" x;
		putStrLn $ show (nonterminals ps)
	} where 
		ps = case parseRuleFile "eki.r" of
			Left err -> error (show err)
			Right s -> (fst s)
		rs = reverse $ rules ps
		isIn = isInClasses $ classes ps
		startWord = start ps
		x = generate startWord isIn rs

generate w isIn rs = process [initialBRState { cw = w }] isIn rs []

process :: [BRState] -> IsIn -> [BaseRule] -> [BRState] -> [BRState]
process ss isIn rs seenStates = trace ("Processing " ++ (showstates "|" ss))
	ss ++ (concat $ map (\state -> process (brapply isIn state rs) isIn rs (seenStates ++ ss)) $ filter (\s -> not (s `elem` seenStates)) ss)

joinStr sep f ss = foldl (\x -> \y -> x ++ sep ++ (f y)) "" ss

showstates sep ss = joinStr sep showState ss
