module Lib 
( splitCode
, splitCodeWith
, Scheme (..)
) where

import System.Random

type Code = Int

data Scheme = Scheme
    { threshold :: Int
    , p :: Code
    , fragments :: [(Int, Code)]
    } deriving (Show, Eq)

encode :: String -> Int
encode = undefined

polynomial :: Num a => [a] -> a -> a
polynomial coefficients x =
    sum $ zipWith (*) coefficients (iterate (*x) 1)

splitCodeWith :: [Int] -> Code -> Scheme
splitCodeWith coefficients code =
    let poly = polynomial (code : coefficients)
    in Scheme (length coefficients + 1) 1 [(x, poly x) | x<-[1..]]

splitCode :: Code -> Int -> IO Scheme
splitCode code threshold = do
    gen <- newStdGen
    let lower = 10 ^ (length (show code) - 1)
        upper = 10 ^ length (show code)
        coefficients = take (threshold - 1) (randomRs (lower, upper) gen)
    return $ splitCodeWith coefficients code
