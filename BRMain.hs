module Main where

import BaseRule
import SetLangParser


main = repl (Just initialBRState) Nothing

repl :: Maybe BRState -> Maybe BRState -> IO ()
repl state defaultState =
	case state of
		Nothing -> do
		 	putStrLn "No action."
		 	repl defaultState defaultState
		Just state -> do
			showBRState state
			l <- getLine
			repl (state `brapply` (read l)) (Just state)
	
showBRState state = do
		putStrLn ("'" ++ (word state) ++ "'")
		showState (vs state)

showState = sequence_ . (map showVar) . variablePairs 
showVar (id, val) = putStrLn (id ++ " = " ++ val)

