module ImageProcessing where

import Prelude as P
import Graphics.Image as I 
import Graphics.Image.Interface
import Data.List
import Data.Matrix as M
import Data.Number.LogFloat

-- Konstanta za skaliranje pri kodiranju piksela
colorScalingFactor :: Int
colorScalingFactor = 5

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

encodeCoord :: Double -> Int
encodeCoord p = if m == colorScalingFactor then colorScalingFactor - 1 else m
              where m = floor (p * fromIntegral colorScalingFactor)

decodeCoord :: Int -> Double
decodeCoord m = ((fromIntegral m) + 0.5) / (fromIntegral colorScalingFactor)

maxEncodedColor :: Int
maxEncodedColor = encodeRGB $ PixelRGB 1 1 1

-- "Kodiranje" uredjene trojke RGB komponenti piksela (koje su tipa LogFloat iz [0,1]), 
-- u skup {1..colorScalingFactor}
encodeRGB :: Pixel RGB Double -> Int
encodeRGB (PixelRGB r g b) = m * colorScalingFactor ^ 2 + n * colorScalingFactor + k + 1
                           where m = encodeCoord r
                                 n = encodeCoord g 
                                 k = encodeCoord b

-- "Dekodiranje" prirodnog broja u piksel sa RGB komponentama iz [0,1] 
decodeRGB :: Int -> Pixel RGB Double
decodeRGB y = PixelRGB (decodeCoord r) (decodeCoord g) (decodeCoord b)
                        where r = fromIntegral $ x `div` (colorScalingFactor ^ 2)
                              g = fromIntegral $ (x `div` colorScalingFactor) `mod` colorScalingFactor 
                              b = fromIntegral $ x `mod` colorScalingFactor
                              x = y - 1
          
-- Preslikavanje skupa svih RGB piksela sa komponentama iz [0,1] na skup RGB piksela 
-- cija je svaka komponenta zaokruzena na jednu decimalu
mapPixelComponents :: Pixel RGB Double -> Pixel RGB Double
mapPixelComponents (PixelRGB r g b) = PixelRGB (f r) (f g) (f b)
                                        where f = \x -> (fromIntegral (round (x * 10)) / 10)

      
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
averagePixelsInGrid image m n = map2D calcAveragePixel (makeGrid image m n)
                          where calcAveragePixel = \x -> I.sum (x / ( fromIntegral $ (rows x) * (cols x)))
                          
-- Lista prosecnih piksela
listFromGrid :: [[Pixel RGB Double]] -> [Int]
listFromGrid grid = fmap (fromIntegral.encodeRGB) $ concat grid


-- Rastojanje izmedju dva kodirana piksela
colorDistance :: Int -> Int -> LogFloat
colorDistance pixel1Code pixel2Code = logFloat $ fromIntegral $ ((encodeCoord r1) - (encodeCoord r2)) ^ 2 + ((encodeCoord g1) - (encodeCoord g2)) ^ 2 + ((encodeCoord b1) - (encodeCoord b2)) ^ 2
                                           where (PixelRGB r1 g1 b1) = decodeRGB pixel1Code
                                                 (PixelRGB r2 g2 b2) = decodeRGB pixel2Code

fieldDistance :: Int -> Int -> Int -> LogFloat
fieldDistance x y k
                      | x == y = fromIntegral $ k * k
                      | otherwise = fromIntegral $ k * (abs ((first x) - (first y))) + (abs ((second x) - (second y)))
                      where first a = ((a-1) `div` k) + 1
                            second a = ((a-1) `mod` k) + 1
                      
inverseDistance :: LogFloat -> LogFloat
inverseDistance d = 1/((d+1)^2)

-- Mreza dimenzije m x n jednobojnih slicica cija je boja jednaka boji prosecnog piksela (zaokruzenog)
-- na elementu mreze polazne slike                       
gridOfAveragePieces :: Image VU RGB Double -> Int -> Int -> [[Image VU RGB Double]]                       
gridOfAveragePieces image m n = map2D createGridPiece (averagePixelsInGrid image m n)
                          where createGridPiece = \ y -> makeImageR VU (pieceRows, pieceCols) (\ (i, j) -> y)
                                pieceRows =  (rows image) `div` m
                                pieceCols =  (cols image) `div` n

drawFromList :: [Int] -> String -> IO ()
drawFromList l name = do
        let n = round $ sqrt $ fromIntegral $ length l
        let image = imageFromGrid $ map2D (\ y -> makeImageR VU (100, 100) (\ (i, j) -> y)) $ M.toLists $ M.fromList n n $ fmap decodeRGB l
        displayImage image
        writeImage (name ++ ".jpg") image

exec :: IO ()
exec = do
        putStrLn "Uneti ime slike:"
        imageName <- getLine
        image <- readImageRGB VU imageName
        let imageResized = resize Bilinear Edge (170,341) image
        print $ imageName
        print $ image
        print $ imageResized
        putStrLn "Uneti dimenziju mreze:"
        inputM <- getLine
        inputN <- getLine
        let m = (P.read inputM :: Int)
        let n = (P.read inputN :: Int)
        let approximation = imageFromGrid (gridOfAveragePieces imageResized m n)
        print $ listFromGrid $ averagePixelsInGrid imageResized m n
        displayImage approximation
        writeImage "aprox.jpg" approximation

