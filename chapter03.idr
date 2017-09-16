module Main

import Data.Vect

-- 2.1. Define your version of `length`.
length' : List a -> Nat
length' [] = 0
length' (x :: xs) = S (length' xs)

-- 2.2. Define your version of `reverse`.
reverse' : List a -> List a
reverse' [] = []
reverse' (x :: xs) = reverse' xs ++ [x]

-- 2.3. Define your version of `map` for `List`.
mapList : (a -> b) -> List a -> List b
mapList f [] = []
mapList f (x :: xs) = f x :: mapList f xs

-- 2.4. Define your version of `map` for `Vect`.
mapVect : (a -> b) -> Vect n a -> Vect n b
mapVect f [] = []
mapVect f (x :: xs) = f x :: mapVect f xs

-- 3.1. Implement `transposeMat` using `zipWith`.
transMatrix : Vect m (Vect n a) -> Vect n (Vect m a)
transMatrix [] = replicate _ []
transMatrix (x :: xs) = zipWith (::) x (transMatrix xs)

-- 3.2. Implement `addMatrix`.
addMatrix
   : Num a
  => Vect n (Vect m a)
  -> Vect n (Vect m a)
  -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys)
  = zipWith (+) x y :: addMatrix xs ys

||| Computes a sum of corresponding products of numbers from two vectors.
dotProduct : Num a => Vect n a -> Vect n a -> a
dotProduct xs ys =
  sum $ zipWith (*) xs ys

||| Constructs a new n*p matrix given n*m and m*p matrices and combining function.
||| A value at a given row/column of a new matrix is constructed by passing
||| corresponding row of first matrix and corresponding row of second matrix to the
||| combining function.
zipMatrixWith
  : (Vect m a -> Vect m a -> b)
 -> Vect n (Vect m a)
 -> Vect m (Vect p a)
 -> Vect n (Vect p b)
zipMatrixWith f mn np =
    map (\n => map (\p => f n p) pn) mn
  where
    pn = transMatrix np

-- 3.3. Implement `multMatrix`.
multMatrix
   : Num a
  => Vect n (Vect m a)
  -> Vect m (Vect p a)
  -> Vect n (Vect p a)  
multMatrix = zipMatrixWith dotProduct







