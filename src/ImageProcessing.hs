module ImageProcessing where

import Prelude as P
import Graphics.Image as I 
import Graphics.Image.Interface
import Data.List as L
import Data.Matrix as M
import Data.Number.LogFloat

-- Konstanta za skaliranje pri kodiranju piksela
colorScalingFactor :: Int
colorScalingFactor = 7

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

-- Kodiranje jedne RGB komponente u skup {1..colorScalingFactor}
encodeCoord :: Double -> Int
encodeCoord p = if m == colorScalingFactor then colorScalingFactor - 1 else m
              where m = floor (p * fromIntegral colorScalingFactor)

decodeCoord :: Int -> Double
decodeCoord m = ((fromIntegral m) + 0.5) / (fromIntegral colorScalingFactor)

maxEncodedColor :: Int
maxEncodedColor = encodeRGB $ PixelRGB 1 1 1

-- "Kodiranje" uredjene trojke RGB komponenti piksela (koje su tipa Double iz [0,1]), 
-- u prirodan broj
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

-- Matrica prosecnih piksela svakom od elemenata mreze dimenzije m x n 
averagePixelsInGrid :: Image VU RGB Double -> Int -> Int -> [[Pixel RGB Double]]
averagePixelsInGrid image m n = map2D calcAveragePixel (makeGrid image m n)
                          where calcAveragePixel = \x -> I.sum (x / ( fromIntegral $ (rows x) * (cols x)))
                          
-- Lista kodiranih prosecnih piksela
listFromGrid :: [[Pixel RGB Double]] -> [Int]
listFromGrid grid = fmap (fromIntegral.encodeRGB) $ concat grid


-- Rastojanje izmedju dva kodirana piksela
colorDistance :: Int -> Int -> LogFloat
colorDistance pixel1Code pixel2Code = logFloat $ fromIntegral $ ((encodeCoord r1)-(encodeCoord r2))^2 + ((encodeCoord g1) - (encodeCoord g2)) ^ 2 + ((encodeCoord b1) - (encodeCoord b2)) ^ 2
                                           where (PixelRGB r1 g1 b1) = decodeRGB pixel1Code
                                                 (PixelRGB r2 g2 b2) = decodeRGB pixel2Code

fieldDistance' :: Int -> Int -> Int -> LogFloat
fieldDistance' x y k
                      | x == y = fromIntegral $ k * k
                      | otherwise = fromIntegral $ k * (abs ((first x) - (first y))) + (abs ((second x) - (second y)))
                      where first a = ((a-1) `div` k) + 1
                            second a = ((a-1) `mod` k) + 1

fieldDistance :: Int -> Int -> Int -> LogFloat
fieldDistance x y k
                      | x == y = fromIntegral $ k * k
                      | otherwise = fromIntegral $ (abs ((first x) - (first y))) + (abs ((second x) - (second y)))
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

drawFromList :: [Pixel RGB Double] -> String -> String -> IO ()
drawFromList l path name = do
        let n = round $ sqrt $ fromIntegral $ length l
        let image = imageFromGrid $ map2D (\ y -> makeImageR VU (40, 40) (\ (i, j) -> y)) $ M.toLists $ M.fromList n n l
--        displayImage image
        writeImage (path ++ name ++ ".jpg") image

drawFromListOfCodes :: [Int] -> String -> IO ()
drawFromListOfCodes l = drawFromList (fmap decodeRGB l) "drawings/"

drawFromListOfProbabilities :: [[Double]] -> String -> IO ()
drawFromListOfProbabilities l = drawFromList (fmap ponderedColor l) "drawings1/"

ponderedColor :: [Double] -> Pixel RGB Double
ponderedColor l = L.foldl sumColors (PixelRGB 0 0 0) $ P.zipWith scaleColor l $ (fmap decodeRGB [1..])

sumColors :: Pixel RGB Double -> Pixel RGB Double -> Pixel RGB Double
sumColors (PixelRGB r1 g1 b1) (PixelRGB r2 g2 b2) = PixelRGB (r1+r2) (g1+g2) (b1+b2)

scaleColor :: Double -> Pixel RGB Double -> Pixel RGB Double
scaleColor d (PixelRGB r g b) = PixelRGB (d*r) (d*g) (d*b)

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

