module Learning where

import HMM
import Prelude as P
import ImageProcessing as IP
import Graphics.Image as I 
import System.Directory
import Data.Map

gridSize :: Int
gridSize = 10

prior :: Image VU RGB Double -> HMM
prior image = HMM.fromList maxEncodedColor $ listFromGrid $ averagePixelsInGrid image gridSize gridSize

evaluateCountryModel :: String -> IO HMM
evaluateCountryModel countryName = do
            countryContents <- listDirectory dirPath
            images <- mapM (readImageRGB VU) $ fmap (dirPath ++ ) countryContents
            priorImage <- readImageRGB VU (dirPath ++ "0.png")
            let codedPixelsLists = fmap (\x -> listFromGrid (averagePixelsInGrid x gridSize gridSize)) images
            let countryModel = P.foldl baumWelchAlgorithm (prior priorImage) codedPixelsLists
            return countryModel
            where dirPath = "img/" ++ countryName ++ "/"

-- primeti ovde da sam menjao putanju jer ja kod mene pokrecem program iz roditeljskog direktorijuma

evaluateMapOfModels :: IO (Map String HMM)
evaluateMapOfModels = do 
                   countryDirs <- listDirectory "img/"
                   models <- mapM evaluateCountryModel countryDirs
                   return $ Data.Map.fromList $ zip countryDirs models
