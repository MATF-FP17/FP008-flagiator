module HMM where

import Data.Matrix as M
import Data.Number.LogFloat as LF
import Prelude as P

-- HMM structure where p is transition probability matrix, q is emission probability matrix and s is matrix of initial probabilities 
data HMM = HMM { p :: Matrix LogFloat
     	       , q :: Matrix LogFloat
	       , s :: [LogFloat]
	       }
	       deriving Show

-- Function that returns matrix of transition probabilities
transitionMatrix :: HMM -> Matrix LogFloat
transitionMatrix (HMM p q s) = p

-- Function that returns matrix of emition probabilities
emissionMatrix :: HMM -> Matrix LogFloat
emissionMatrix (HMM p q s) = q

-- Function that returns matrix of initial probabilities
initialMatrix :: HMM -> [LogFloat]
initialMatrix (HMM p q s) = s

-- Function that returns number of hidden states
hiddenStates :: HMM -> Int
hiddenStates (HMM p q s) = nrows q
n = hiddenStates

-- Function that returns number of emitted states
emittedStates :: HMM -> Int
emittedStates (HMM p q s) = ncols q
m = emittedStates

-- A constant used as multiplier when creating new model
constantPrecision :: LogFloat
constantPrecision = logFloat 0.8

nearlyEye :: Int -> Matrix LogFloat
nearlyEye n = matrix n n $ \(i, j) -> if i == j then constantPrecision else (1-constantPrecision)/((fromInteger $ toInteger n) - 1)

-- -_-
inv :: Int -> LogFloat
inv x = 1/(fromInteger $ toInteger x)

-- A function that creates constant matrix for given dimensions
constMatrix :: Num a => Int -> Int -> a -> Matrix a
constMatrix n m x = matrix n m $ \_ -> x

-- Default constructor
fromMatrices :: Matrix LogFloat -> Matrix LogFloat -> [LogFloat] -> HMM
fromMatrices p q s = HMM p q s


-- Constructor that makes default HMM with given max values for hidden and emitted states
-- All matrices are uniform
defaultHMM :: Int -> Int -> HMM
defaultHMM n m = HMM (constMatrix n n $ inv n) 
			(constMatrix n m $ inv n)
			(toList $ constMatrix 1 n $ inv n)


-- Constructor that makes a priori model from first example
-- Emision matrix is uniform
-- Transition matrix represents scaled counted transitions
-- n = m
fromList :: Int  -> [Int] -> HMM
fromList n l@(h:t) = HMM (scaleTransitions $ countTransitions n l)
				(constMatrix n n $ inv n) 
				(toList $ setElem constantPrecision (1, h) $ scaleMatrix (1 - constantPrecision) (constMatrix 1 n 1))


-- Scales transition matrix to sum of 1
scaleTransitions :: Matrix LogFloat -> Matrix LogFloat
scaleTransitions m = fromLists $ map (\l -> map (/(LF.sum l)) l) (toLists m)

-- Counts transitions in list
countTransitions :: Int -> [Int] -> Matrix LogFloat
countTransitions n l = foldl increse (M.fromList n n $ repeat 1) $ zip l $ tail l

increse :: Num a => Matrix a -> (Int, Int) -> Matrix a
increse m (x, y) = setElem ((getElem x y m) + 1) (x, y) m




-- Next iteration in forward algorithm
forwardIteration :: HMM -> [LogFloat] -> Int -> [LogFloat]
forwardIteration model@(HMM p q s) l y = [LF.sum $ map (\(li, i) -> li * p!(i, j) * q!(j, y)) $ zip l [1..] | j <- [1..(n model)]]

-- Returns matrix of forward iterations prolonged on a list of elements
forwardAlgorithm :: HMM -> [Int] -> [LogFloat]
forwardAlgorithm model@(HMM p q s) ys = foldl (forwardIteration model) s ys

-- Next iteration in backward algorithm
backwardIteration :: HMM -> Int -> [LogFloat] -> [LogFloat]
backwardIteration model@(HMM p q s) y l = [LF.sum $ map (\(lj, j) -> lj * p!(i, j) * q!(j, y)) $ zip l [1..] | i <- [1..(n model)]]

-- Returns matrix of backward iterations prolonged on a list of elements
backwardAlgorithm :: HMM -> [Int] -> [LogFloat]
backwardAlgorithm model ys = foldr (backwardIteration model) (take (n model) $ repeat 1) ys

-- Evaluates transition probability using sums of forward algorithm and backward algorithm results
transitionEvaluation :: HMM -> [Int] -> Int -> Int -> Int -> LogFloat
transitionEvaluation model@(HMM p q s) ys i j t = let 
							y1 = take t ys
							y2 = drop (t-1) ys
							alpha = forwardAlgorithm model y1
							beta = backwardAlgorithm model y2
						  in alpha!!(i-1) * beta!!(j-1) * p!(i, j) * q!(j, ys!!(t-1)) -- / (LF.sum $ forwardAlgorithm model ys)

-- Gives the next iteration of HMM matrix of probabilities for new input (by definition)
learnP :: HMM -> [Int] -> Matrix LogFloat
learnP model ys = scaleTransitions $ fromLists $ [[LF.sum [transitionEvaluation model ys i j t | t <- [1..(length ys)]] | j <- [1..(n model)]] | i <- [1..(n model)]]

-- EVERYTHING OPTIMIZED (access to the element of matrix is done in constant time)
forwardAlgorithm' :: HMM -> [Int] -> Matrix LogFloat
forwardAlgorithm' model@(HMM p q s) ys = fromLists $ reverse $ foldl (\m y -> (forwardIteration model (m!!0) y):m) [s] ys

backwardAlgorithm' :: HMM -> [Int] -> Matrix LogFloat
backwardAlgorithm' model ys = fromLists $ foldr (\y m -> (backwardIteration model y (m!!0)):m) [take (n model) $ repeat 1] ys

-- Gives the next iteration of HMM matrix of probabilities for new input (optimized)
learnP' :: HMM -> [Int] -> Matrix LogFloat
learnP' model@(HMM p q s) ys = let
				alphas = transpose $ submatrix 1 (length ys) 1 (n model) $ forwardAlgorithm' model ys
				betas = submatrix 1 (length ys) 1 (n model) $ backwardAlgorithm' model ys
				qs = transpose $ fromLists $ zipWith (\l qq -> (map (\x -> qq!!(x-1)) l)) (take (n model) $ repeat ys) (toLists q)
				in scaleTransitions $ scalarMatrixProduct p $ alphas * (scalarMatrixProduct betas qs)

--baumWelchAlgorithm :: HMM -> [Int] -> HMM
--baumWelchAlgorithm model ys = HMM (learnP model ys) (learnQ model ys) s


scalarMatrixProduct :: Num a => Matrix a -> Matrix a -> Matrix a
scalarMatrixProduct x y = matrix (nrows x) (ncols x) (\(i, j) -> x!(i, j) * y!(i, j)) 

-- TESTS KOCKICE

testKockice :: [Int]
testKockice = [(1*k) `mod` 6 + 1 | k <- [1..1]]

kockice :: HMM
kockice = HMM (nearlyEye 2) (fromLists [[1/6 | _ <- [1..6]], [1/4, 1/12, 1/4, 1/12, 1/4, 1/12]]) [0.5, 0.5]

kockice2 :: HMM
kockice2 = HMM (nearlyEye 2) (fromLists [[1/6 | _ <- [1..6]], [1/3-0.0001, 0.0001, 1/3-0.0001, 0.0001, 1/3-0.0001, 0.0001]]) [0.5, 0.5]

forwardKockice :: Matrix LogFloat
forwardKockice = forwardAlgorithm' kockice testKockice

backwardKockice :: Matrix LogFloat
backwardKockice = backwardAlgorithm' kockice testKockice

learnKockice :: (HMM -> [Int] -> a) -> a
learnKockice f = f kockice testKockice

-- TESTS kxk

k :: (Num a) => a
k = 200

testBoje :: [Int]
testBoje = [1..50]

boje :: HMM
boje = (HMM (nearlyEye k) (nearlyEye k) [1/k | _ <- [1..k]])

learnBoje :: (HMM -> [Int] -> a) -> a
learnBoje f = f boje testBoje





-- mozda moze lepse
getSequence :: HMM -> [Int] -> [Int]
getSequence model ys = snd $ maks1 $ viterbiAlgorithm model ys

viterbiAlgorithm :: HMM -> [Int] -> [(LogFloat, [Int])]
viterbiAlgorithm model@(HMM p q s) ys = foldr (viterbiIteration model) (zip s [[1], [2]]) ys

viterbiIteration ::  HMM -> Int -> [(LogFloat, [Int])] -> [(LogFloat, [Int])]
viterbiIteration model@(HMM p q s) y l = [maks model l j y | j <- [1..(n model)]]

maks :: HMM -> [(LogFloat, [Int])] -> Int -> Int -> (LogFloat, [Int])
maks model@(HMM p q s) l j y = maks1 [((fst vs) * p!(i, j) * q!(j, y), j:(snd vs)) | (i, vs) <- zip [1..(n model)] l]

maks1 :: [(LogFloat, [Int])] -> (LogFloat, [Int])
maks1 = maks2 (-1, [])

maks2 :: (LogFloat, [Int]) -> [(LogFloat, [Int])] -> (LogFloat, [Int])
maks2 m [] = m
maks2 m (h:t)
  | fst h > fst m = maks2 h t
  | otherwise = maks2 m t
-- mozda moze lepse

testViterbi :: [Int] -> [Int]
testViterbi x = getSequence (HMM (constMatrix 2 2 (1/2)) 
			  (fromLists [[1/4, 1/12, 1/4, 1/12, 1/4, 1/12], [1/6|_<-[1..6]]]) 
			  [1/2, 1/2]
		     ) x

testViterbi1 :: [Int] -> [Int]
testViterbi1 x = getSequence (HMM.fromList 6 [1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 4, 3, 2, 1]) x
