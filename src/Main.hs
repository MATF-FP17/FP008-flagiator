module Main where

import Learning
import ImageProcessing
import HMM
import Data.Number.LogFloat
import Data.Matrix as M
import Data.Map

main :: IO ()
main = do 
         models <- evaluateMapOfModels
	 mapM_ (\(name, hmm) ->  print (generateApproximateModelOutput hmm 25)) $ Data.Map.toList models
	 mapM_ (\(name, hmm) -> drawFromList (generateApproximateModelOutput hmm 25) name) $ Data.Map.toList models
--         print models
