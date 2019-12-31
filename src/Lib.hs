module Lib 
( splitCode
, splitCodeWith
) where

import System.Random

type Code = Int

encode :: String -> Int
encode = undefined

polynomial :: Num a => [a] -> a -> a
polynomial coefficients x =
    sum $ zipWith (*) coefficients (iterate (*x) 1)

splitCodeWith :: [Int] -> Code -> [(Int, Code)]
splitCodeWith coefficients code =
    let poly = polynomial (code : coefficients)
    in [(x, poly x) | x<-[1..]]

splitCode :: Code -> Int -> IO [(Int, Code)]
splitCode code threshold = do
    gen <- newStdGen
    let lower = 10 ^ (length (show code) - 1)
        upper = 10 ^ length (show code)
        coefficients = take (threshold - 1) (randomRs (lower, upper) gen)
    return $ splitCodeWith coefficients code
