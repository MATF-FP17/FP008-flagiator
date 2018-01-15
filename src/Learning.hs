module Learning where

import HMM
import Prelude as P
import ImageProcessing as IP
import Graphics.Image as I 
import System.Directory

gridSize :: Int
gridSize = 3

prior :: Image VU RGB Double -> HMM
prior image = HMM.fromList maxEncodedColor $ listFromGrid $ averagePixelsInGrid image gridSize gridSize

process :: IO()
process = do
            serbiaContents <- listDirectory "../img/serbia/"
            images <- mapM (readImageRGB VU) $ fmap ("../img/serbia/" ++ ) serbiaContents
            --print $ fmap (\x -> listFromGrid (averagePixelsInGrid x gridSize gridSize)) images
            priorImage <- readImageRGB VU "../img/serbia/0.png"
            let modelSerbia = foldl baumWelchAlgorithm (prior priorImage) $ fmap (\x -> listFromGrid (averagePixelsInGrid x gridSize gridSize)) images
            print modelSerbia
