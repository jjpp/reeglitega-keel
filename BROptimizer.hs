module Main where

import BaseRule
import RuleParser
import Control.Monad
import Debug.Trace
import SetLang
import Data.Set as S (toList, empty, fromList, union, size)
import Data.Map as M hiding (map, filter)
import Data.Graph


data StateNode = S [String] VarState



main = do { 	putStrLn $ "Reegleid: " ++ (show $ length rs);
		putStrLn $ "Steps:\n" ++ steps;
		putStrLn $ "Optimisation order: " ++ (show $ map (edgeLabel . stepEdgeByVertex) $ optOrder);
		putStrLn $ "Known vars and states:\n" ++ (concatMap 
			(\(k, v) -> k ++ " = " 
				++ (show $ length v) ++ ": " 
				++ (show $ take 30 v) ++ "\n")
			(map (\(k, v) -> (k, S.toList v)) $ M.toList varsAndStates));
		putStrLn $ "Statespace: " ++ (show $ foldr (*) 1 $ map ((\a -> toInteger a + 1) . S.size . snd) $ M.toList varsAndStates);
		putStrLn $ "Rulegraph:\n" ++ (concatMap
			(\(node, key, keys) -> " " ++ (show key) ++ "/" ++ node ++ " (" 
				++ (show $ length keys) ++ "): "
				++ (show $ take 20 keys) ++ "\n") ruleGraph);
		putStrLn $ "digraph {\n" ++ (concatMap
			(\(node, key, keys) -> concatMap (\target -> (show key) ++ " -> " ++ (show target) ++ "\n") keys) ruleGraph)
			++ "}\n";

				
	} where 
		ps = case parseRuleFile "eki.r" of
			Left err -> error (show err)
			Right s -> (fst s)
		rs = reverse $ rules ps
		isIn = isInClasses $ classes ps
		startWord = start ps
		steps = getSteps steplist
		steplist = uniq $ concatMap gs' rs
		(stepg, stepEdgeByVertex, stepVertexByValue) = graphOfSteps steplist
		optOrder = reverse $ topSort stepg
		varsAndStates = getVarsAndStates rs
		ruleGraph = genRuleGraph rs


genRuleGraph :: [BaseRule] -> [(String, Int, [Int])]
genRuleGraph rs = map (genPossibleTransitions rs) rs
genPossibleTransitions rs r = (ruleName r, ruleId r, idsOfMatchingRules r rs)
idsOfMatchingRules r rs = map ruleId $ filter (\target -> isPossibleTransition r target) rs
isPossibleTransition s t = or $ [ isPossibleTrans' action cond | action <- concat [ posAction, negAction ], cond <- [ posCond, negCond ] ]
	where
		negAction = nonFalseActionOf (nomatchExpr s)
		posAction = nonFalseActionOf (matchExpr s)
		negCond = condOf (nomatchExpr t)
		posCond = condOf (matchExpr t)

nonFalseActionOf c = if (condOf c /= CFalse) then [actionOf c] else []

-- isPossibleTrans' as c = trace ((show as) ++ "; " ++ (show c) ++ " -> " ++ (show $ possibleEval c st') ++ "\n") 
isPossibleTrans' as c = possibleEval c st' 
	where st' = (foldr emptyRun initialState as)

getVarsAndStates rs = foldr updateVarsAndStates M.empty rs 
updateVarsAndStates :: BaseRule -> Map String Domain -> Map String Domain
updateVarsAndStates br m = foldr updateVar m allVars
	where
		allVars = uniq $ (condVars br)
		valueAsList Nothing = S.empty
		valueAsList (Just x) = x
		newValue k x = Just ((valueAsList x) `S.union` (S.fromList $ condValues k br))
		updateVar k m = M.alter (newValue k) k m


edgeLabel (node, key, keys) = node

graphOfSteps sl = graphFromEdges vlist 
	where
		vlist = map vdef $ uniq $ concatMap (\(a, b) -> [a, b]) sl
		vdef node = (node, node, (map snd $ filter (\a -> node == fst a) sl))


rulesWithTarget step rs = filter (\r -> step `elem` (anyExpr exprTargetStep r)) rs

getSteps sl = concatMap (\(a, b) -> (show a) ++ " -> " ++ (show b) ++ "; ") sl

anyExpr :: (MonadPlus m) => (Expression -> m a) -> BaseRule -> m a
anyExpr f r = (f (matchExpr r)) `mplus` (f (nomatchExpr r))

gs' r = anyExpr exprStepEdge r
-- gs' r = exprStepEdge (matchExpr r) ++ exprStepEdge (nomatchExpr r) 
-- gs' r = exprTargetStep (matchExpr r) ++ exprTargetStep (nomatchExpr r) 


exprTargetStep expr = actionValues' "step" expr
exprCondStep x = condValues' "step" x

condVars' (In var value) = [var]
condVars' (Is var value) = [var]
condVars' (Not c) = condVars' c
condVars' (And c1 c2) = (condVars' c1) ++ (condVars' c2)
condVars' (Or c1 c2) = (condVars' c1) ++ (condVars' c2)
condVars' (Xor c1 c2) = (condVars' c1) ++ (condVars' c2)
condVars' (Defined var) = [var]
condVars' _ = []
condVars'' (Expression c _) = condVars' c

condVars br = anyExpr (condVars'') br

condValues' k (In k' values) | k == k' = S.toList values
condValues' k (Is k' value) | k == k' = [value]
condValues' k (Not c) = condValues' k c
condValues' k (And c1 c2) = (condValues' k c1) ++ (condValues' k c2)
condValues' k (Or c1 c2) = (condValues' k c1) ++ (condValues' k c2)
condValues' k (Xor c1 c2) = (condValues' k c1) ++ (condValues' k c2)
condValues' k (Defined k') | k == k' = ["UNDEFINED"]
condValues' _ _ = []

condValues'' k (Expression c _) = condValues' k c
condValues k br = anyExpr (condValues'' k) br


actionValue' k (SetVar k' value) | k == k' = [value]
actionValue' k (UnsetVar k') | k == k' = ["UNDEFINED"];
actionValue' _ _ = []

actionValues' k (Expression _ ss) = concatMap (actionValue' k) ss
actionValues k br = anyExpr (actionValues' k) br


actionVar' (SetVar k _) = [k]
actionVar' (UnsetVar k) = [k]
actionVar' (DeclareVar k _) = [k]
actionVar' _ = []

actionVars' (Expression _ ss) = concatMap actionVar' ss
actionVars br = anyExpr actionVars' br


exprStepEdge expr@(Expression cond _) = 
	case targets of
		[] -> do from <- sources; return (from, from)
		_ -> do from <- sources; to <- targets; return (from, to)
	where
		sources = exprCondStep cond
		targets = exprTargetStep expr


uniqPrepend :: (Eq a) => [a] -> [a] -> [a]
uniqPrepend old new = foldl (\as -> \b -> if b `elem` as then as else b:as) old new
uniq xs = uniqPrepend [] xs



