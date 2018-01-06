module Main where

import HMM
import Data.Number.LogFloat
import Data.Matrix as M

main :: IO ()
main = do
--	print $ transitionEvaluation kockice testKockice 1 1 0 
--	print $ backwardAlgorithm' kockice testKockice
--	print $ backwardAlgorithm kockice testKockice
--	print $ forwardAlgorithm' kockice testKockice
--	print $ forwardAlgorithm kockice testKockice
	print $ learnBoje learnP'
--	print $ learnBoje learnP
