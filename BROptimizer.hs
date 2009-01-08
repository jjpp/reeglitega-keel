module Main where

import BaseRule
import RuleParser
import BRApprox
import Control.Monad
import Debug.Trace
import SetLang
import Data.Set as S (toList, empty, fromList, union, size, singleton, insert, Set, member)
import Data.Map as M hiding (map, filter)
import Data.Graph


data StateNode = S [String] VarState
type BoxId = [Name]
data Box = B { bid :: BoxId, brules :: [BaseRule], targets :: S.Set BoxId }

showBox b = showBoxId (bid b) ++ " (" ++ (show $ length $ brules b) ++ ") -> " ++ (show $ map showBoxId $ S.toList $ targets b)
instance Show Box where
	show x = showBox x

showBoxId bid = unwords bid


boxVars = ["step", "substep", "type", "stem", "target_stem", "stem_transform", "form", "target_form"]


main = do { 	putStrLn $ "Reegleid: " ++ (show $ length rs);
		putStrLn $ "Steps:\n" ++ steps;
		putStrLn $ "Optimisation order: " ++ (show $ map (edgeLabel . stepEdgeByVertex) $ optOrder);
		putStrLn $ "Known vars and states:\n" ++ (concatMap 
			(\(k, v) -> k ++ " = " 
				++ (show $ length v) ++ ": " 
				++ (show $ take 30 v) ++ "\n")
			(map (\(k, v) -> (k, S.toList v)) $ M.toList varsAndStates));
		putStrLn $ "Statespace: " ++ (show $ foldr (*) 1 $ map ((\a -> toInteger a + 1) . S.size . snd) $ M.toList varsAndStates);
		putStrLn $ "Boxspace: " ++ (show $ foldr (*) 1 
			$ map ((\a -> toInteger a + 1) . S.size . snd) 
				$ filter (\(a, b) -> a `elem` boxVars) 
					$ M.toList varsAndStates);
		putStrLn $ "Boxen: " ++ (concatMap (\(x, v) -> (show x) ++ " -> " ++ (show $ length v) ++ "\n") boxen);
		putStrLn $ "boxmap: " ++ (concatMap (\x -> (showBoxId $ bid x) ++ " => " ++ (show $ S.size $ targets x) ++ "\n") boxrels);
		putStrLn $ "digraph rules {\n" ++ (concatMap boxgraph boxrels) ++ "}\n"; 
{-		putStrLn $ "Rulegraph:\n" ++ (concatMap
			(\(node, key, keys) -> " " ++ (show key) ++ "/" ++ node ++ " (" 
				++ (show $ length keys) ++ "): "
				++ (show $ take 20 keys) ++ "\n") ruleGraph); -}
--		putStrLn $ "fs = " ++ (show fs) ++ "\n";
--		putStrLn $ "showfs = " ++ (showfs fs) ++ "\n";
--		putStrLn $ "digraph {\n" ++ (concatMap
--			(\(node, key, keys) -> concatMap (\target -> (show key) ++ " -> " ++ (show target) ++ "\n") keys) ruleGraph)
--			++ "}\n";

				
	} where 
		ps = case parseRuleFile "/dev/stdin" of
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
		initFS = FullState {
			stateById = M.singleton 0 $ initialExtState startWord,
			stateByVS = M.singleton initialState 0,
			updatedStates = S.singleton 0 }
		ruleSet = RS isIn varsAndStates rs
		fs = extCalc ruleSet initFS
		boxen = countBoxen boxVars rs
		boxrels = getBoxRels $ M.fromList $ map (\(n, r) -> (n, B { bid = n, brules = r, targets = S.empty })) boxen

boxgraph b = concatMap edge $ S.toList $ targets b
	where edge t = "\"" ++ (showBoxId $ bid b) ++ "\" -> \"" ++ (showBoxId t) ++ "\"\n"

getBoxRels m = map (\x -> x { targets = getTargets (bid x) x }) $ map snd $ M.toList m
	where
		getTargets i b = foldr (addTarget i) S.empty $ brules b
		addTarget i r s = S.insert (calcTarget i r) s
		calcTarget i r = case (try [] tn) of
					Just x -> x
					Nothing -> error ("Nii ei saa olla, et midagi ei ole: " ++ (show tn))
			where
				tn = calcTarget' i r
				try x [] = if M.member x m then Just x else Nothing
				try x (r:rs) = (try (x ++ [r]) rs) `mplus` (try x rs)
		calcTarget' i r = filter (\x -> x /= []) $ concatMap (check r) boxVars -- :: [Name] 
		check r k = filter (\x -> x /= []) $ map (\x -> if x == "UNDEFINED" then "" else k ++ " = " ++ x) v -- :: [Name]
			where
				v = if avs /= [] then avs else cvs
				cvs = condValues k r
				avs = actionValues k r

countBoxen :: [Name] -> [BaseRule] -> [([Name], [BaseRule])]
countBoxen vs rs = map rulepair $ S.toList nameSets'
	where
		rulepair varstate = ((vs2ns varstate), (applicableRules varstate))
		applicableRules vs = filter (\r -> ((ev r vs matchExpr) `tsor` (ev r vs nomatchExpr)) /= TSFalse) rs
		ev r v f = openEval (condOf $ f r) v
		vs2ns v = map (\(k, v) -> k ++ " = " ++ v) $ M.toList v
		nameSets' = foldr addNameSet S.empty rs
		addNameSet r s = S.insert (varState r) s
		varState r = M.fromList $ [ (k, v) | k <- rvs, v <- condValues k r, v /= "UNDEFINED" ]
		rvs = reverse vs

extIterate' rs fs = tracefs $ extIterate rs fs

extCalc rs fs = if S.size (updatedStates fs') == 0 
			then fs'
			else extCalc rs fs'
		where fs' = extIterate' rs fs

tracefs fs = trace (showfs fs) fs
showfs fs = "\n\n----\n" 
	++ (concatMap (\(k, v) -> (show k) ++ ": " ++ (show v) ++ "\n") $ M.toList $ stateById fs)


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


edgeLabel (node, key, keys) = node

graphOfSteps sl = graphFromEdges vlist 
	where
		vlist = map vdef $ uniq $ concatMap (\(a, b) -> [a, b]) sl
		vdef node = (node, node, (map snd $ filter (\a -> node == fst a) sl))


rulesWithTarget step rs = filter (\r -> step `elem` (anyExpr exprTargetStep r)) rs

getSteps sl = concatMap (\(a, b) -> (show a) ++ " -> " ++ (show b) ++ "; ") sl

gs' r = anyExpr exprStepEdge r
-- gs' r = exprStepEdge (matchExpr r) ++ exprStepEdge (nomatchExpr r) 
-- gs' r = exprTargetStep (matchExpr r) ++ exprTargetStep (nomatchExpr r)

exprTargetStep expr = actionValues' "step" expr
exprCondStep x = condValues' "step" x

exprStepEdge expr@(Expression cond _) = 
	case targets of
		[] -> do from <- sources; return (from, from)
		_ -> do from <- sources; to <- targets; return (from, to)
	where
		sources = exprCondStep cond
		targets = exprTargetStep expr


