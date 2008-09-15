module BaseRule where

import Text.Regex;
import SetLang;
import SetLangParser;
import Control.Monad;

type Str = String

data BaseRule = BaseRule { upper, lower, precond, postcond :: Str, extra :: Expression }
	deriving (Eq, Show, Read)

uppercond rule = (precond rule) ++ (BaseRule.upper rule) ++ (postcond rule)
lowercond rule = (precond rule) ++ (BaseRule.lower rule) ++ (postcond rule)


data BRState = BRState { cw :: Str, vs :: VarState }
	deriving (Eq, Show)

brapply :: (MonadPlus m) => BRState -> [BaseRule] -> (m BRState)
brapply state rs = msum (map (brapply' state) rs)

transformWord :: (Monad m) => Str -> BaseRule -> m Str
transformWord ws rule = return $ subRegex (mkRegex (uppercond rule)) ws (lowercond rule) 

brapply' :: (Monad m) => BRState -> BaseRule -> m BRState
brapply' state rule = do 
			varstate' <- apply (extra rule) (vs state)
			word' <- transformWord (cw state) rule
			return (BRState word' varstate')

initialBRState = BRState "START" initialState
nopExpression = Expression CTrue [Nop]


