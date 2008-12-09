module SetLang where
-- module SetLang where

import qualified Data.Map as Map
import qualified Data.Set as Set

type Name = String
type Value = String
type Domain = Set.Set Value
-- Kõik teadaolevad muutujad
type AllVars = Map.Map Name Domain
-- Kehtivad muutujad
type VarState = Map.Map Name Value


data Expression = Expression Cond [Statement] deriving (Show, Read, Eq)

condOf (Expression c _) = c
actionOf (Expression _ as) = as

data Statement = DeclareVar Name Domain
               | SetVar Name Value
	       | UnsetVar Name
	       | Nop
	deriving (Show, Read, Eq)

-- apply :: Statement -> 

data Cond = In Name Domain
          | Is Name Value
	  | Defined Name
	  | Not Cond
	  | And Cond Cond
	  | Or Cond Cond
	  | Xor Cond Cond
--	  | GtConst Name Value
--	  | GtEConst Name Value
--	  | GtVars Name Name
--	  | GtEVars Name Name
	  | CFalse
	  | CTrue
	deriving (Show, Read, Eq)

eval :: Cond -> VarState -> Bool
eval (Is var value) vars = case (Map.lookup var vars) of
				Just x -> x == value 
				Nothing -> False
eval (In var values) vars = case (Map.lookup var vars) of
				Just x -> x `Set.member` values
				Nothing -> False
eval (Defined var) vars = var `Map.member` vars
eval (Not cond) vars = not $ eval cond vars
eval (And c1 c2) vars = (eval c1 vars) && (eval c2 vars)
eval (Or c1 c2) vars = (eval c1 vars) || (eval c2 vars)
eval (Xor c1 c2) vars = (eval c1 vars) /= (eval c2 vars)
eval CFalse _ = False
eval CTrue _ = True

domainFromStringList ss = Set.fromList ss
variablePairs :: VarState -> [(Name, Value)]
variablePairs = Map.toList


run :: Statement -> VarState -> VarState
run (DeclareVar _ _) x = error "declare is unimplemented"
run (SetVar var value) state = Map.alter (\x -> Just value) var state
run (UnsetVar var) state = Map.delete var state
run (Nop) state = state

apply :: (Monad m) => Expression -> VarState -> m VarState
apply (Expression cond ss) state 
	| eval cond state = return (applyStatements state ss)
	| otherwise       = fail "Dead end."

applyStatements state ss = foldr run state ss

initialState :: VarState
initialState = Map.empty

showVS :: VarState -> String
showVS vs = foldr (\a -> \b -> a ++ "; " ++ b) "" $ map (\x -> (fst x) ++ " = " ++ (snd x)) $ Map.toList vs

undefinedValue = "UNDEFINED"

possibleEval :: Cond -> VarState -> Bool
possibleEval (Is var value) vars = case (Map.lookup var vars) of
				Just x -> x == value 
				Nothing -> True
possibleEval (In var values) vars = case (Map.lookup var vars) of
				Just x -> x `Set.member` values
				Nothing -> True
possibleEval (Defined var) vars = case (Map.lookup var vars) of
				Just x -> x /= undefinedValue
				Nothing -> True 
possibleEval (Not cond) vars = not $ possibleEval cond vars
possibleEval (And c1 c2) vars = (possibleEval c1 vars) && (possibleEval c2 vars)
possibleEval (Or c1 c2) vars = (possibleEval c1 vars) || (possibleEval c2 vars)
possibleEval (Xor c1 c2) vars = (possibleEval c1 vars) /= (possibleEval c2 vars)
possibleEval CFalse _ = False
possibleEval CTrue _ = True

emptyRun :: Statement -> VarState -> VarState
emptyRun (DeclareVar _ _) x = error "declare is unimplemented"
emptyRun (SetVar var value) state = Map.alter (\x -> Just value) var state
emptyRun (UnsetVar var) state = Map.alter (\x -> Just undefinedValue) var state
emptyRun (Nop) state = state


