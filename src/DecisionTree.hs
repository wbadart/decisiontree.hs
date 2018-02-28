{- DecisionTree.hs
 -
 - Simple decision tree implementation for Haskell.
 -
 - Will Badart
 - created: FEB 2018
 -}

module DecisionTree (classify) where

import Data.Function (on)
import Data.List (find, maximumBy, nub)
import qualified Data.Map as M
import qualified Data.Set as S


type Test = [String] -> Bool
type Metric = [[String]] -> [Test] -> Float


labelCounts :: [[String]] -> M.Map String Int
labelCounts data_ =
    M.fromListWith (\_ a -> a + 1) $ zip (map last data_) (repeat 0)


prediction :: [[String]] -> String
prediction data_ =
    let counts = labelCounts data_
    in fst $ maximumBy (compare `on` snd) $ M.toList counts


mktests :: [[String]] -> Int -> [Test]
mktests data_ idx =
    let feature_vals = nub $ map (!!idx) data_
        mktest v = (==v) . (!!idx)
    in map mktest feature_vals


bestFeature :: [[String]] -> Metric -> Int
bestFeature data_ criterion =
    let features = [0..(length $ data_ !! 0) - 2]  -- -2 to exclude label
        thisIG = criterion data_
        branches = mktests data_
        ftByIG = [(ft, thisIG $ branches ft) | ft <- features]
    in fst $ maximumBy (compare `on` snd) ftByIG


classify :: [[String]] -> Metric -> [String] -> Maybe String
classify data_ criterion tup
    | (length $ nub $ map last data_) <= 1 = Just (prediction data_)
    | otherwise = let ftIdx = bestFeature data_ criterion
                  in case find ($tup) $ mktests data_ ftIdx
                          of Just p -> classify (filter p data_) criterion tup
                             Nothing -> Nothing
