module Learning where

import Data.Ord
import Data.List
import HMM
import Prelude as P
import ImageProcessing as IP
import Graphics.Image as I 
import Data.Matrix as M
import Data.Number.LogFloat as LF
import System.Directory
import Data.Map as MP
import System.IO

gridSize :: Int
gridSize = 15

learningIterations :: Int
learningIterations = 5


-- HMM created from just one image
prior :: Image VU RGB Double -> HMM
prior image = HMM.fromList (gridSize^2) maxEncodedColor $ listFromGrid $ averagePixelsInGrid image gridSize gridSize

-- Evaluates HMM for given country name
evaluateCountryModel :: String -> IO HMM
evaluateCountryModel countryName = do
            countryContents <- listDirectory dirPath
            images <- mapM (readImageRGB VU) $ fmap (dirPath ++ ) countryContents
            priorImage <- readImageRGB VU (dirPath ++ "0.png")
            let codedPixelsLists = fmap (\x -> listFromGrid (averagePixelsInGrid x gridSize gridSize)) images
            let countryModel = P.foldl baumWelchAlgorithm (prior priorImage) codedPixelsLists
            return countryModel
            where dirPath = "img/" ++ countryName ++ "/"

-- Creates map of pairs - (countryName, hmm)
evaluateMapOfModels :: IO (Map String HMM)
evaluateMapOfModels = do 
                   countryDirs <- listDirectory "img/"
                   models <- mapM evaluateCountryModel countryDirs
                   return $ MP.fromList $ zip countryDirs models
 
-- Writing model in file
writeModel :: String -> HMM -> IO ()                  
writeModel countryName (HMM p q s) = do
                              writeFile ("models/" ++ countryName ++ ".flag") modelRepresentation
                              where modelRepresentation = unlines $ fmap show $ map2D fromLogFloat [M.toList p, M.toList q, s]

-- Reading model of given country                             
readModel :: String -> IO HMM
readModel countryName = do
                        contents <- fmap lines $ readFile ("models/" ++ countryName ++ ".flag")
                        let n = gridSize ^ 2 + 1
                        let m = maxEncodedColor
                        let parsedData = map2D logFloat (fmap read contents :: [[Double]])
                        let model = HMM (M.fromList n n $ parsedData !! 0) (M.fromList n m $ parsedData !! 1) (parsedData !! 2)
                        return model

-- Country model to stdout                        
learnCountry :: String -> IO ()
learnCountry name = do
                    model <- evaluateCountryModel name
                    writeModel name model

-- Writes all learned models in files                       
learning :: IO ()
learning = do
           models <- evaluateMapOfModels
           mapM_ (\(name, hmm) -> writeModel name hmm) $ concat $ P.take learningIterations $ repeat $ MP.toList models
  
-- Draws approximate model output for each country         
drawModels :: IO ()
drawModels = do
             countryNames <- listDirectory "img/"
             mapM_ drawModel countryNames

drawModel :: String -> IO ()
drawModel countryName = do
                        model <- readModel countryName
                        drawFromListOfProbabilities (generateApproximateProbabilities model) countryName
                        drawFromListOfCodes (generateMaximalProbabilities model) countryName

-- Finds the most likely model for given picture           
classify :: String -> IO ()
classify imageName = do
                 countryDirs <- listDirectory "img/"
                 models <- mapM readModel countryDirs
                 let modelNames = zip countryDirs $ models
       
                 image <- readImageRGB VU imageName
                 let sample = listFromGrid $ averagePixelsInGrid image gridSize gridSize
       
                 let probabilities = fmap (\(name, hmm) -> (name, evaluateSample hmm sample)) modelNames
                 
--                 print probabilities
       
                 print (imageName, fst $ maximumBy (comparing snd) probabilities)
                 
classification :: IO ()
classification = do
                 samples <- listDirectory "samples/"
                 mapM_ classify $ fmap ("samples/" ++ ) samples
                 
-- Executes program
exec :: String -> IO ()
exec "classification" = classification
exec "clasify" = do
                 putStrLn "Uneti ime slike:"
                 imageName <- getLine
		 classify imageName
exec "learning" = learning
exec "learn" = do
                 putStrLn "Uneti ime drzave:"
                 countryName <- getLine
                 learnCountry countryName
exec "drawing" = drawModels
exec "draw" = do
                 putStrLn "Uneti ime drzave:"
                 countryName <- getLine
                 drawModel countryName
exec err = putStrLn "Pogresna komanda!"





