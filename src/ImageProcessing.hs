module ImageProcessing where

import Prelude as P
import Graphics.Image as I 
import Graphics.Image.Interface
import Data.List

-- Bijekcija N x N -> N
twoCoordinateBijection :: Integer -> Integer -> Integer
twoCoordinateBijection m n = 2 ^ m * (2 * n + 1) - 1
	      
fstCoordinateInverse :: Integer -> Integer	       
fstCoordinateInverse x 	
        | list == [] = 0
        | otherwise = head list
         where list = [ z | z <- [0..(x + 1)], (x + 1) `mod` 2 ^ (z + 1) /= 0 ]	      	    

sndCoordinateInverse :: Integer -> Integer
sndCoordinateInverse x = ((x + 1) `div` 2 ^ (fstCoordinateInverse x) - 1) `div` 2	    

-- "Kodiranje" uredjene trojke RGB komponenti piksela (koje su tipa Double iz [0,1]), 
-- generisano gorepomenutim kodiranjem uredjenog para, u skup prirodnih brojeva
encodeRGB :: Pixel RGB Double -> Integer
encodeRGB (PixelRGB r g b) = twoCoordinateBijection (twoCoordinateBijection m n) k
                           where m = round $ r * 10
                                 n = round $ g * 10 
                                 k = round $ b * 10

-- "Dekodiranje" prirodnog broja u piksel sa RGB komponentama iz [0,1] zaokruzenim na jednu decimalu
decodeRGB :: Integer -> Pixel RGB Double
decodeRGB x = PixelRGB r g b
          where r = fromInteger (fstCoordinateInverse (fstCoordinateInverse x)) / 10.0
                g = fromInteger (sndCoordinateInverse (fstCoordinateInverse x)) / 10.0 
                b = fromInteger (sndCoordinateInverse x) / 10.0
          
-- Preslikavanje skupa svih RGB piksela sa komponentama iz [0,1] na skup RGB piksela 
-- cija je svaka komponenta zaokruzena na jednu decimalu
mapPixelComponents :: Pixel RGB Double -> Pixel RGB Double
mapPixelComponents (PixelRGB r g b) = PixelRGB (f r) (f g) (f b)
                                        where f = \x -> (fromIntegral (round (x * 10)) / 10)
	
redComponent :: Pixel RGB Double -> Double
redComponent (PixelRGB r g b) = r

greenComponent :: Pixel RGB Double -> Double
greenComponent (PixelRGB r g b) = g

blueComponent :: Pixel RGB Double -> Double
blueComponent (PixelRGB r g b) = b

 
-- F-ja akumulacije koja kao inicijalnu vrednost uzima prvi element liste
foldlWithHeadAsInit :: (a -> a -> a) -> [a] -> a
foldlWithHeadAsInit f list = P.foldl f (head list) (tail list)

-- Mapiranje nad listom listi
map2D :: (a -> b) -> [[a]] -> [[b]]
map2D f listOfLists = P.map (P.map f) listOfLists 

-- Deljenje slike na mrezu dimenzije m x n
makeGrid :: Image VU RGB Double -> Int -> Int -> [[Image VU RGB Double]]
makeGrid image m n = [ [crop (i, j) (pieceRows, pieceCols) image 
                                  | i <- [0, pieceRows .. (m-1) * pieceRows] ]
                                  | j <- [0, pieceCols .. (n-1) * pieceCols] ]
                           where pieceRows =  (rows image) `div` m
                                 pieceCols =  (cols image) `div` n 

-- Spajanje elementa mreze u sliku                             
imageFromGrid :: [[Image VU RGB Double]] -> Image VU RGB Double
imageFromGrid grid = let groupedCols = P.map (foldlWithHeadAsInit topToBottom) grid
                     in  (foldlWithHeadAsInit leftToRight) groupedCols

-- Matrica prosecnih piksela svakom od elemenata mreze dimenzije m x n zaokruzenih na jednu decimalu
averagePixelsInGrid :: Image VU RGB Double -> Int -> Int -> [[Pixel RGB Double]]
averagePixelsInGrid image m n = map2D (mapPixelComponents . calcAveragePixel) (makeGrid image m n)
                          where calcAveragePixel = \x -> I.sum (x / ( fromIntegral $ (rows x) * (cols x)))
                          
-- Lista prosecnih piksela
listFromGrid :: [[Pixel RGB Double]] -> [Pixel RGB Double]
listFromGrid grid = concat grid


-- Mreza dimenzije m x n jednobojnih slicica cija je boja jednaka boji prosecnog piksela (zaokruzenog)
-- na elementu mreze polazne slike                       
gridOfAveragePieces :: Image VU RGB Double -> Int -> Int -> [[Image VU RGB Double]]                       
gridOfAveragePieces image m n = map2D createGridPiece (averagePixelsInGrid image m n)
                          where createGridPiece = \ y -> makeImageR VU (pieceRows, pieceCols) (\ (i, j) -> y)
                                pieceRows =  (rows image) `div` m
                                pieceCols =  (cols image) `div` n 
                                 


exec :: IO ()
exec = do
        putStrLn "Uneti ime slike:"
        imageName <- getLine
        image <- readImageRGB VU imageName
        let imageResized = resize Bilinear Edge (170,341) image
        putStrLn "Uneti dimenziju mreze:"
        inputM <- getLine
        inputN <- getLine
        let m = (P.read inputM :: Int)
        let n = (P.read inputN :: Int)
        let approximation = imageFromGrid (gridOfAveragePieces imageResized m n)
        displayImage approximation
        writeImage "aprox.jpg" approximation

