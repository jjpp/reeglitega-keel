module Main where

import BaseRule
import RuleParser
import SetLang
import Data.Map as M
import Data.Set as S
import System.IO.UTF8 as UTF8
import System (getArgs)

main = do
	args <- getArgs
	main' $ if length args > 0 then args !! 0 else "/dev/stdin"


main' x = do {
		UTF8.putStrLn $ (showClasses $ classes ps);
		UTF8.putStrLn $ ":start " ++ (unescape reversedNT $ start ps) ++ "\n";
		showRules reversedNT $ rs
	} where 
		ps = case parseRuleFile x of
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

