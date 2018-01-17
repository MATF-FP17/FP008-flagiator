module Learning where

import HMM
import Prelude as P
import ImageProcessing as IP
import Graphics.Image as I 
import Data.Matrix as M
import Data.Number.LogFloat as LF
import System.Directory
import Data.Map
import System.IO

gridSize :: Int
gridSize = 10

prior :: Image VU RGB Double -> HMM
prior image = HMM.fromList (gridSize^2) maxEncodedColor $ listFromGrid $ averagePixelsInGrid image gridSize gridSize

evaluateCountryModel :: String -> IO HMM
evaluateCountryModel countryName = do
            countryContents <- listDirectory dirPath
            images <- mapM (readImageRGB VU) $ fmap (dirPath ++ ) countryContents
            priorImage <- readImageRGB VU (dirPath ++ "0.png")
            let codedPixelsLists = fmap (\x -> listFromGrid (averagePixelsInGrid x gridSize gridSize)) images
            let countryModel = P.foldl baumWelchAlgorithm (prior priorImage) codedPixelsLists
            return countryModel
            where dirPath = "../img/" ++ countryName ++ "/"

-- primeti ovde da sam menjao putanju jer ja kod mene pokrecem program iz roditeljskog direktorijuma

evaluateMapOfModels :: IO (Map String HMM)
evaluateMapOfModels = do 
                   countryDirs <- listDirectory "../img/"
                   models <- mapM evaluateCountryModel countryDirs
                   return $ Data.Map.fromList $ zip countryDirs models
 
writeModel :: String -> HMM -> IO ()                  
writeModel countryName (HMM p q s) = do
                              writeFile (countryName ++ ".flag") modelRepresentation
                              where modelRepresentation = unlines $ fmap show $ map2D fromLogFloat [M.toList p, M.toList q, s] 
						                 
						            
--readModel :: String -> IO HMM
readModel countryName = do
						contents <- fmap lines $ readFile (countryName ++ ".flag")
						let n = gridSize ^ 2 + 1
						let m = maxEncodedColor
						let parsedData = map2D logFloat (fmap read contents :: [[Double]])
						let model = HMM (M.fromList n n $ parsedData !! 0) (M.fromList n m $ parsedData !! 1) (parsedData !! 2)
						print model
						return model
