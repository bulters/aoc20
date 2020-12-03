{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.List.Split as S (splitOneOf)
import Data.Algebra.Boolean (xor)

data Password = Password Policy String deriving (Show)
data Policy = Policy Int Int Char deriving (Show)

parseInput :: [String] -> [Password]
parseInput ss = map parsePassword ss

parseInt :: String -> Int
parseInt = read

parsePassword :: String -> Password
parsePassword s = Password pol pass where
                        parts = S.splitOneOf ": -" s 
                        min = parseInt (parts !! 0) 
                        max = parseInt (parts !! 1) 
                        char = head $ parts !! 2
                        pol = Policy min max char
                        pass = parts !! 4

validPassword :: Password -> Bool
validPassword (Password (Policy mi ma c) p) = cc >= mi && cc <= ma where
                                               cc = length $ filter (== c) p

complexValidPassword :: Password -> Bool
complexValidPassword (Password (Policy p1 p2 c) p) = ((p !! (p1-1)) == c) `xor` ((p !! (p2-1)) == c)

main :: IO ()
main = do
        raw <- readFile "inputs/day2.txt"
        let ls = lines raw
        let passwords = parseInput ls
        let validPasswords = filter validPassword passwords
        print $ length validPasswords
        let complexValidPasswords = filter complexValidPassword passwords
        print $ length complexValidPasswords
