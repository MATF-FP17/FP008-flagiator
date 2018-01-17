module Main where

import Graphics.Image
import Prelude as P
import Learning
import ImageProcessing
import HMM
import Data.Number.LogFloat
import Data.Matrix as M
import Data.Map

--main :: IO ()
main = do
     hmm <- evaluateCountryModel "greece"
     writeModel "greece" hmm
     readModel "greece"
--     mapM_print (generateApproximateModelOutput hmm 9)) $ Data.Map.toList models
--     mapM_ (\(name, hmm) -> drawFromList (generateApproximateModelOutput hmm 9) name) $ Data.Map.toList models
