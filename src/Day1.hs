module Main where

import Text.Read (readMaybe)

parseInts :: [String] -> [Maybe Integer]
parseInts = map readMaybe

main :: IO ()
main = do
        raw <- readFile "inputs/day1.txt"
        let ls = lines raw
        let amts = parseInts ls
        let target = Just 2020
        let solution1 = take 1 [fmap product can | a <- amts, b <- amts, let can = sequence [a,b], fmap sum can == target]
        print solution1
        let solution2 = take 1 [fmap product can | a <- amts, b <- amts, c <- amts, let can = sequence [a,b,c], fmap sum can == target]
        print solution2
