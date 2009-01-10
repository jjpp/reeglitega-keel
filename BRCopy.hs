module BRCopy where

import BaseRule
import RuleParser
import SetLang
import Data.Map as M
import Data.Set as S
import System.IO.UTF8 as UTF8
import System (getArgs)
import Control.Monad
import Debug.Trace

copyRules f = do
	args <- getArgs
	copyRules' f $ if length args > 0 then args !! 0 else "/dev/stdin"

copyRules' :: (ParserState -> ParserState) -> String -> IO ()
copyRules' f x = do {
		UTF8.putStrLn $ (showClasses $ classes ps);
		UTF8.putStrLn $ ":start " ++ (unescape reversedNT $ start ps) ++ "\n";
		showRules reversedNT $ rs
	} where 
		ps = f $ case parseRuleFile x of
			Left err -> error (show err)
			Right s -> (fst s)
		reversedNT = M.fromList . (Prelude.map swap) . M.toList $ nonterminals ps
		rs = reverse $ rules ps

swap (a, b) = (b, a)

unescape nt [] = []
unescape nt (x:xs) = (if x > '\xffff'
			then case x `M.lookup` nt of
				Just z -> ("{" ++ z ++ "}")
				Nothing -> [x] 
			else
				[x]) ++ unescape nt xs

zero [] = "0"
zero xs = xs
			

showClasses :: Classes -> String
showClasses cs = concatMap showClass $ M.toList cs
showClass (k, vs) = ":class " ++ [k] ++ " " ++ (S.toList vs) ++ "\n"

showRules nt rs = mapM_ (showRule nt) rs
showRule nt br = UTF8.putStrLn $
		(zero $ unescape nt $ upper br) ++ " " ++
		(zero $ unescape nt $ lower br) ++ " " ++
		(zero $ unescape nt $ precond br) ++ " " ++
		(zero $ unescape nt $ postcond br) ++ " " ++
		(zero $ showExpr $ matchExpr br) ++
		(if (nomatchExpr br /= noExpression) then " " ++ (showExpr $ nomatchExpr br) else "")

showExpr (Expression c as) = "{ " ++ (showCond c) ++ ": " ++ (showAction as) ++ " }"

showCond CFalse = "false"
showCond CTrue = "true"
showCond (Is var value) = var ++ " = " ++ value
showCond (In var values) = var ++ " = " ++ (show values)
showCond (Not c) = "!(" ++ (showCond c) ++ ")"
showCond (Defined v) = "defined(" ++ v ++ ")"
showCond (And e1 e2) = "(" ++ (showCond e1) ++ ") && (" ++ (showCond e2) ++ ")"
showCond (Or e1 e2) = "(" ++ (showCond e1) ++ ") || (" ++ (showCond e2) ++ ")"
showCond (Xor e1 e2) = "(" ++ (showCond e1) ++ ") ^^ (" ++ (showCond e2) ++ ")"


showStmt (SetVar var value) = var ++ " = " ++ value
showStmt (UnsetVar var) = "unset " ++ var
showStmt (Nop) = "nop"

showStmt' st out | out == "" = showStmt st
showStmt' st out | otherwise = showStmt st ++ "; " ++ out

showAction as = foldr showStmt' "" as 


-----------------------------

filterByVars vs ps = ps { rules = applicableRules vs (rules ps) }
	where
		applicableRules vs rs = Prelude.filter (\r -> ((ev r vs matchExpr) `tsor` (ev r vs nomatchExpr)) /= TSFalse) rs
		ev r v f = openEval (condOf $ f r) v

-----------------------------

something (Just _) = True
something Nothing  = False

optimize ps = ps { rules = optimizedRules  }
	where
		rs = reverse $ rules ps

		optimizedRules = reverse $ maybe rs ((flip preapply) rs) (applicableRule rs)
		varsAndStates = getVarsAndStates rs
		applicableRule rs = msum $ Prelude.map (isApplicable rs) rs
		isApplicable rs r = if any (something . applied True r) rs then Just r else Nothing
		applied ww r' r = if (ruleId r') == (ruleId r) then Nothing else
				if "erand" `elem` (actionVars r') then Nothing else 
				if "erand" `elem` (actionVars r) then Nothing else 
				applied' ww r' r
		applied' ww r' r = 
				--trace ("checking brapply': '" ++ (show suffix) ++ "', " ++ (show stateAfter) ++ ", " ++ (show r)) $ 
				if ww 
					then brapply' isIn (BRState { cw = suffix, vs = stateAfter }) r'
					else case apply (matchExpr r) stateAfter of 
						Just newState -> Just (BRState { cw = suffix, vs = newState })
						Nothing -> Nothing 
			where
				suffix = lowercond r
				suffix' = uppercond r'
				stateAfter = case apply (matchExpr r) cs of
						Just x -> x
						Nothing -> error ("mitterakendatav seis:\n" ++ (show r) ++ "\n" ++ (show r') ++ "\n" ++ (show cs))
					where 	cl = condState r initialState $ condVars r
						cs = if length cl == 0 then initialState else cl !! 0
				condState r s (v:vs) = concatMap (\x -> condState r (setVar v x s) vs) $ extCondValues v r
				condState r s [] = if eval (condOf $ matchExpr r) s then [s] else []
				extCondValues v r = if "UNDEFINED" `elem` cv then (S.toList $ varsAndStates ! v) else cv
					where cv = condValues v r
				setVar v x s = run (if x == "UNDEFINED" then (UnsetVar v) else (SetVar v x)) s
				isIn = isInClasses $ classes ps

		possibleApps ww r rs = Prelude.filter (something . applied ww r) rs

-- leia kõik võimalikud rakendamispunktid
-- iga jaoks: kui see on ainus rakendamisvõimalus, siis rakenda ja asenda. 
-- kui mõni veel on, siis rakenda, eemalda stop ja lisa vahetult alles jääva esinemise ette
		preapply r rs = trace ("preapply " ++ (show r) ++ "\naps = " ++ (show $ length aps) ++ "\n\n") $ selfremove $ foldr (preapply' r) rs aps
			where 	aps = possibleApps True r rs
				selfremove = if (something $ applied' False r r) then id else (removeRule r)

		preapply' r' r rs = trace ("preapply' " ++ (show r) ++ " + " ++ (show r') ++ "\n -> " ++ (show merged) ++ "\n") $ 
			if length aps == 1 
				then replaceRule r (merged) rs 
				else insertBefore r (unstop $ merged) rs
			where 	aps = possibleApps False r' rs
				applied = -- trace ("applied': " ++ (show r') ++ " + " ++ (show r)) $ 
					applied' True r' r
				old = uppercond r
				new = case applied of
					Just x -> cw x
					Nothing -> error "Nii ei saa olla, et liidetavad reeglid pole üksteisele rakendatavad"
				tuple' = resplit old new
				merged = mergeRules r r' tuple'

		mergeRules r r' (up, low, left, right) = r { 
								matchExpr = mergeExpr (matchExpr r) (matchExpr r'),
								upper = up,
								lower = low,
								precond = left,
								postcond = right
							}
		mergeExpr e e' = Expression (condOf e) (mergeActions (actionOf e) (actionOf e'))

rawLength xs = rawLength' xs 0
rawLength' [] l = l
rawLength' ('1':xs) l = rawLength' xs l
rawLength' ('2':xs) l = rawLength' xs l
rawLength' ('!':xs) l = rawLength' xs l
rawLength' (x:xs) l = rawLength' xs (l + 1)

mergeActions s s' = -- trace ("mergeActions " ++ (show s) ++ ", " ++ (show s')) $ 
			foldr mergeAction s s'
mergeAction :: Statement -> [Statement] -> [Statement]
mergeAction (Nop) ss = ss
mergeAction z@(SetVar k v) ss = -- trace ("merge, set " ++ k ++ " = " ++ v ++ " (" ++ (show ss) ++ ")") $ 
	(clearAction k ss) ++ [z]
mergeAction z@(UnsetVar k) ss = -- trace ("merge, unset " ++ k ++ " (" ++ (show ss) ++ ")") $
	(clearAction k ss) ++ [z]
clearAction k ss = (Prelude.filter (\x -> case x of (SetVar k' _) -> k' /= k; (UnsetVar k') -> k' /= k; _ -> True) ss)

unstop r = -- trace ("unstop r = " ++ (show r)) $ 
	r { matchExpr = unstopExpr (matchExpr r) }
unstopExpr e = Expression (condOf e) (clearAction "stop" (actionOf e))

replaceRule old new (r:rs) = if (ruleId r == ruleId old) then new:rs else r:(replaceRule old new rs)
replaceRule old new [] = []

removeRule old (r:rs) = if (ruleId r == ruleId old) then rs else r:(removeRule old rs)
removeRule old [] = []

insertBefore old new (r:rs) = -- trace ("insertBefore " ++ (show new)) $ 
	if (ruleId r == ruleId old) then new:r:rs else r:(insertBefore old new rs)
insertBefore old new [] = []
			

resplit :: String -> String -> (String, String, String, String) 
resplit a b | a == b = (a, b, "", "")
resplit a b | otherwise = (upper, lower, left, right) 
	where
		(left, a', b') = commonPrefix a b
		(right, upper, lower) = commonPrefix a' b'

commonPrefix :: String -> String -> (String, String, String)
commonPrefix a b | a == b = (a, "", "")
commonPrefix a b | otherwise = (prefix, revA, revB) 
	where
		prefix = Prelude.map fst $ takeWhile (\(a, b) -> a == b) $ zip a b
		l = length prefix
		revA = reverse $ drop l a
		revB = reverse $ drop l b


