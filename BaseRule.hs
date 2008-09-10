module BaseRule where

import Text.Regex.Base;
import SetLang;
import Control.Monad;

type Str = String

data BaseRule = BaseRule { upper, lower, precond, postcond :: Str, extra :: Expression }
	deriving (Eq, Show)

data BRState = BRState { word :: Str, vs :: VarState }

brapply :: (MonadPlus m) => BRState -> [BaseRule] -> (m BRState)
brapply state rs = msum (map (brapply' state) rs)

transformWord :: (Monad m) => Str -> BaseRule -> m Str
transformWord ws rule = return ws

brapply' :: (Monad m) => BRState -> BaseRule -> m BRState
brapply' state rule = do 
			word' <- transformWord (word state) rule
			varstate' <- apply (extra rule) (vs state)
			return (BRState word' varstate')

	
