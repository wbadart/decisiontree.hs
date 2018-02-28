{- Main.hs
 -
 - Driver program to test DecisionTree.
 -
 - Will Badart
 - created: FEB 2018
 -}

import System.Environment (getArgs)
import System.IO (
    BufferMode(NoBuffering), getLine, hSetBuffering, readFile, stdout)

import DecisionTree (classify)
import Entropy (informationGain)


splitBy :: (Char -> Bool) -> String -> [String]
splitBy p = foldr f [[]]
    where f c l@(x:xs) | p c = []:l
                       | otherwise = (c:x):xs


main = do
    args <- getArgs
    let fname = if not $ null args
                then head args else "weather.csv"

    putStrLn $ "Using training set from " ++ fname
    hSetBuffering stdout NoBuffering

    contents <- readFile "weather.csv"
    let split = splitBy (==',')
        rows = map split $ lines contents

    let loop = do
        putStr "instance> "
        line <- getLine

        let tup = split line
        print $ classify rows informationGain tup

        loop

    loop
