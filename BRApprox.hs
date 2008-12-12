module BRApprox where

import BaseRule;
import SetLang;
import Data.Set as S;
import Data.Map as M;
import Debug.Trace;

data ExtState = ExtState { esId :: Int, cws :: [Str], extvs :: VarState, targetStates :: Set Int }
	deriving (Eq, Show)

data FullState = FullState { 
		stateById :: Map Int ExtState,
		stateByVS :: Map VarState Int,
		updatedStates :: Set Int
	} deriving (Show)

data RuleSet = RS IsIn AllVars [BaseRule]


resetUpdates fs = fs { updatedStates = S.empty }

extIterate :: RuleSet -> FullState -> FullState
extIterate rs fs = foldr (stateStep rs) (resetUpdates fs) $ S.toList (updatedStates fs)


stateStep :: RuleSet -> Int -> FullState -> FullState
stateStep rs i fs = foldr (stateWordStep rs state i) fs (cws state)
	where state = (stateById fs) ! i

stateWordStep :: RuleSet -> ExtState -> Int -> String -> FullState -> FullState
stateWordStep (RS isIn allVars rules) es i w fs = foldr (updateState i) fs newStates
	where 	brs = BRState w (extvs es)
		newStates = Prelude.map (filterState allVars) $ brapply isIn brs rules :: [BRState]

updateState :: Int -> BRState -> FullState -> FullState
updateState i bs fs = -- trace ("updateState, i = " ++ (show i) ++ ", targetStateId = " ++ (show targetStateId)) $ 
				updateTarget $ updateThis fs
	where	
		targetBefore = (vs bs) `M.lookup` (stateByVS fs)
		targetStateId = -- trace ("target before = " ++ (show targetBefore)) $ 
			case targetBefore of
				Just x -> x
				Nothing -> M.size $ stateByVS fs 
		(targetChanges, targetNow) = case targetBefore of
				Just i' -> updateTargetState $ (stateById fs) ! i'
				Nothing -> (True, ExtState targetStateId [(cw bs)] (vs bs) S.empty)
		updateTargetState es' = if (cw bs) `elem` (cws es') 
						then (False, es') 
						else (True, es' { cws = (cw bs) : (cws es') })
		updateTargetRefs es = -- trace ("inserting target " ++ (show targetStateId) ++ " into " ++ (show $ esId es) ++ ", targetStates = " ++ (show $ targetStates es)) $
			es { targetStates = targetStateId `S.insert` (targetStates es) }
		updateThis fs = -- trace ("updateThis, fs = " ++ (show fs) ++ "\n   i = " ++ (show i)) $
			fs { stateById = M.adjust updateTargetRefs i (stateById fs) }
		updateTarget fs = -- trace ("updateTarget, fs = " ++ (show fs)) $
				if targetChanges
					then fs { 
						stateById = M.insert targetStateId targetNow (stateById fs),
						stateByVS = M.insert (vs bs) targetStateId (stateByVS fs),
						updatedStates = S.insert targetStateId (updatedStates fs) }
					else fs
		

filterState :: AllVars -> BRState -> BRState
filterState av s = s { vs = M.mapWithKey (filterState' av) $ vs s }

filterState' :: AllVars -> Name -> Value -> Value
filterState' av k v = if k `elem` ["form", "giki", "step", "kind"] then v 
		else case M.lookup k av of
			Just v' -> if v `S.member` v' then v else "OTHER"
			Nothing -> v


initialExtState startWord = ExtState 0 [startWord] initialState S.empty

