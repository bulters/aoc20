module Main where

type Field = [[Bool]]
type Coord = (Int, Int)
type Slope = (Int, Int)

parseField :: String -> Field
parseField i = map parseLine ls where
               ls = lines i

parseLine :: String -> [Bool]
parseLine l = map (\x -> x == '#' ) l

getXY :: Field -> Int -> Coord -> Bool
getXY f w (x, y) = (f !! y) !! ax where
                    ax = mod x w

step :: Coord -> Slope -> Coord
step (x, y) (dx, dy) = (x + dx, y + dy)

slideDown :: Field -> Int -> Int -> Slope -> Int
-- slideDown f s w h = True
slideDown f w h s = length $ filter id trees where
                        posGen = scanl step (0,0) (repeat s)
                        trees = map (getXY f w) $ takeWhile (\(_,y) -> y < h) posGen

main :: IO ()
main = do
        raw <- readFile "inputs/day3.txt"
        let field = parseField raw
        let width = length $ field !! 0
        let height = length field
        let step = (3, 1)
        let solution1 = slideDown field width height step
        print solution1
        let slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]
        let trees = map (slideDown field width height) slopes
        let solution2 = product trees
        print solution2
