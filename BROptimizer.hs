module Main where

import BaseRule
import RuleParser
import Control.Monad
import Debug.Trace
import SetLang
import Data.Set (toList)
import Data.Graph





main = do { 	putStrLn $ "Reegleid: " ++ (show $ length rs);
		putStrLn $ "Steps:\n" ++ steps;
		putStrLn $ "Optimisation order: " ++ (show $ map (edgeLabel . stepEdgeByVertex) $ optOrder);
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

exprTargetStep (Expression _ ss) = concat $ 
	map (\x -> case x of 
		SetVar "step" s -> [s]
		_ -> []) ss

exprCondStep (In "step" d) = Data.Set.toList d
exprCondStep (Is "step" v) = [v]
-- mitte päris korrektne..
exprCondStep (Not c) = exprCondStep c
exprCondStep (And c1 c2) = (exprCondStep c1) ++ (exprCondStep c2)
exprCondStep (Or c1 c2) = (exprCondStep c1) ++ (exprCondStep c2)
exprCondStep (Xor c1 c2) = (exprCondStep c1) ++ (exprCondStep c2)
exprCondStep _ = []


exprStepEdge expr@(Expression cond _) = 
	case targets of
		[] -> do from <- sources; return (from, from)
		_ -> do from <- sources; to <- targets; return (from, to)
	where
		sources = exprCondStep cond
		targets = exprTargetStep expr


uniq :: (Eq a) => [a] -> [a]
uniq = foldl (\as -> \b -> if b `elem` as then as else b:as) [] 


