module Main where

parseInts :: [String] -> [Int]
parseInts = map read

main :: IO ()
main = do
        raw <- readFile "inputs/day1.txt"
        let ls = lines raw
        let amts = parseInts ls
        let solution1 = head [a * b | a <- amts, b <- amts, a + b == 2020]
        print solution1
        let solution2 = head [a * b * c | a <- amts, b <- amts, c <- amts, a + b + c == 2020]
        print solution2
