module Main where

import BRCopy
import Data.Map

main = copyRules (filterByVars $ fromList [("type", "10")])
-- main = copyRules (filterByVars $ fromList [("step", "vormid"), ("type", "10"), ("stem", "c0"), ("target_form", "------13")])

