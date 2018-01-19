module Main where

import Graphics.Image
import Prelude as P
import Learning
import ImageProcessing
import HMM
import Data.Number.LogFloat
import Data.Matrix as M
import Data.Map

main :: IO ()
main = do
       putStrLn "Uneti komandu:"
       command <- getLine
       Learning.exec command
--     learnCountry "trinidad_and_tobago"
--     mapM_vprint (generateApproximateModelOutput hmm 9)) $ Data.Map.toList models
--     mapM_ (\(name, hmm) -> drawFromList (generateApproximateModelOutput hmm 9) name) $ Data.Map.toList models
