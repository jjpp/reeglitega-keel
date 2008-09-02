module Main where

import SetLangParser

main = repl (Just initialState) Nothing

repl :: Maybe VarState -> Maybe VarState -> IO ()
repl state defaultState =
	case state of
		Nothing -> do
		 	putStrLn "No action."
		 	repl defaultState defaultState
		Just state -> do
			showState state
			l <- getLine
			repl (state `applySetLang` l) (Just state)
	
showState = sequence_ . (map showVar) . variablePairs 
showVar (id, val) = putStrLn (id ++ " = " ++ val)

