module HMM where

import Flags
import Data.Matrix as M
import Prelude as P

-- HMM structure where p is transition probability matrix, q is emission probability matrix and s is matrix of start probabilities 
data HMM = HMM { p :: Matrix Double
     	       , q :: Matrix Double
	       , s :: Matrix Double
	       }
	       deriving Show

-- A constant used as multiplier when creating new model
constantPrecision :: Double
constantPrecision = 0.8

-- -_-
inv :: Int -> Double
inv x = 1/(fromInteger $ toInteger x)

-- A function that creates constant matrix for given dimensions
constMatrix :: Num a => Int -> Int -> a -> Matrix a
constMatrix n m x = matrix n m $ \_ -> x

-- Default constructor
fromMatrices :: Matrix Double -> Matrix Double -> Matrix Double -> HMM
fromMatrices p q s = HMM p q s


-- Constructor that makes default HMM with given max values for hidden and emitted states
-- All matrices are uniform
defaultHMM :: Int -> Int -> HMM
defaultHMM n m = HMM (constMatrix n n $ inv n) 
	      	  (constMatrix m n $ inv n)
		  (constMatrix 1 n $ inv n)


-- Constructor that makes a priori model from first example
-- Emision matrix is uniform
-- Transition matrix represents scaled counted transitions
-- n = m
fromList :: Int  -> [Int] -> HMM
fromList n l@(h:t) = HMM (scale $ transitions n l)
	     	       	   (constMatrix n n $ inv n) 
			   (setElem constantPrecision (1, h) $ scaleMatrix (1 - constantPrecision) (constMatrix 1 n 1))

scale :: Matrix Double -> Matrix Double
scale m = fromLists $ map (\l -> map (/(sum l)) l) (toLists m)

-- Counts transitions in list
transitions :: Int -> [Int] -> Matrix Double
transitions n l = foldl increse (M.fromList n n $ repeat 1) $ zip l $ tail l

increse :: Num a => Matrix a -> (Int, Int) -> Matrix a
increse m (x, y) = setElem ((getElem x y m) + 1) (x, y) m













-- mozda moze lepse
getSequence :: HMM -> [Int] -> [Int]
getSequence model ys = snd $ maks1 $ viterbiAlgorithm model ys

viterbiAlgorithm :: HMM -> [Int] -> [(Double, [Int])]
viterbiAlgorithm model@(HMM p q s) ys = foldr (viterbiIteration model) (zip (toList s) [[1], [2]]) ys

viterbiIteration ::  HMM -> Int -> [(Double, [Int])] -> [(Double, [Int])]
viterbiIteration model@(HMM p q s) y l = [maks model l j y | j <- [1..n]] where n = nrows s

maks :: HMM -> [(Double, [Int])] -> Int -> Int -> (Double, [Int])
maks (HMM p q s) l j y = maks1 [((fst vs) * p!(i, j) * q!(j, y), j:(snd vs)) | (i, vs) <- zip [1..n] l] where n = nrows s

maks1 :: [(Double, [Int])] -> (Double, [Int])
maks1 = maks2 (-1, [])

maks2 :: (Double, [Int]) -> [(Double, [Int])] -> (Double, [Int])
maks2 m [] = m
maks2 m (h:t)
  | fst h > fst m = maks2 h t
  | otherwise = maks2 m t
-- mozda moze lepse





















test :: [Int] -> [Int]
test x = getSequence (HMM (constMatrix 2 2 (1/2)) 
			  (fromLists [[1/4, 1/12, 1/4, 1/12, 1/4, 1/12], [1/6|_<-[1..6]]]) 
			  (M.fromList 2 1 [1/2, 1/2])
		     ) x

test1 :: [Int] -> [Int]
test1 x = getSequence (HMM.fromList 6 [1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 4, 3, 2, 1]) x


