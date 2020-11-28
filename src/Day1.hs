module Main where

import Text.Read (readMaybe)
import System.Exit (die)
import Data.List (tails)

solution1 :: [Int] -> Int
solution1 xs = head [ a * b | (a, b) <- two xs, a + b == 2020 ]

solution2 :: [Int] -> Int
solution2 xs = head [ a * b * c | (a, b, c) <- three xs, a + b + c == 2020]


combinations :: Int -> [Int] -> [[Int]]
combinations 0 _  = [[]]
combinations _ [] = [[]]
combinations n xs = filter (\x -> length x == n) [ y:ys | (y:rest) <- tails xs, ys <- combinations (n-1) rest]

two :: [Int] -> [(Int, Int)]
two xs = [(a,b) | a:rest <- tails xs, b <- rest]

three :: [Int] -> [(Int, Int, Int)]
three xs = [(a,b,c) | (a:resta) <- tails xs, (b:restb) <- resta, c <- head restb]

main :: IO ()
main = do
        raw <- readFile "inputs/day1.txt"
        let ls = lines raw
        -- let amts = parseInts ls
        case traverse readMaybe ls of
                Just xs -> do
                        print $ solution1 xs
                        print $ solution2 xs
                Nothing -> die "Parse error"
        -- let target = Just 2020
        -- let solution1 = take 1 [fmap product can | a <- amts, b <- amts, let can = sequence [a,b], fmap sum can == target]
        -- print solution1
        -- let solution2 = take 1 [fmap product can | a <- amts, b <- amts, c <- amts, let can = sequence [a,b,c], fmap sum can == target]
        -- print solution2
