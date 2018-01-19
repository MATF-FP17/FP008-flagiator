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
gridSize = 10


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
            where dirPath = "../img/" ++ countryName ++ "/"

-- Creates map of pairs - (countryName, hmm)
evaluateMapOfModels :: IO (Map String HMM)
evaluateMapOfModels = do 
                   countryDirs <- listDirectory "../img/"
                   models <- mapM evaluateCountryModel countryDirs
                   return $ MP.fromList $ zip countryDirs models
 
-- Writing model in file
writeModel :: String -> HMM -> IO ()                  
writeModel countryName (HMM p q s) = do
                              writeFile ("../models/" ++ countryName ++ ".flag") modelRepresentation
                              where modelRepresentation = unlines $ fmap show $ map2D fromLogFloat [M.toList p, M.toList q, s]

-- Reading model of given country                             
readModel :: String -> IO HMM
readModel countryName = do
                        contents <- fmap lines $ readFile ("../models/" ++ countryName ++ ".flag")
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
           mapM_ (\(name, hmm) -> writeModel name hmm) $ MP.toList models
  
-- Draws approximate model output for each country         
drawModels :: IO ()
drawModels = do
             countryDirs <- listDirectory "../img/"
             models <- mapM readModel countryDirs
             let modelNames = zip countryDirs $ models
             mapM_ (\(name, hmm) -> drawFromList (generateApproximateModelOutput hmm (gridSize^2)) name) modelNames
           
-- Finds the most likely model for given picture           
classification :: IO ()
classification = do
                 countryDirs <- listDirectory "../img/"
                 models <- mapM readModel countryDirs
                 let modelNames = zip countryDirs $ models
       
                 putStrLn "Uneti ime slike:"
                 imageName <- getLine
                 image <- readImageRGB VU imageName
                 let sample = listFromGrid $ averagePixelsInGrid image gridSize gridSize
       
                 let probabilities = fmap (\(name, hmm) -> (name, evaluateSample hmm sample)) modelNames
                 
                 print probabilities
       
                 print $ fst $ maximumBy (comparing snd) probabilities
                 
                 
-- Executes program
exec :: String -> IO ()
exec "classify" = classification
exec "learn" = learning
exec "draw" = drawModels
exec err = putStrLn "Pogresna komanda!"




