{- DecisionTree.hs
 -
 - Simple decision tree implementation for Haskell.
 -
 - Will Badart
 - created: FEB 2018
 -}

module DecisionTree (classify, mktests) where

import Data.Function (on)
import Data.List (find, maximumBy, nub)

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

import Entropy (Criterion, Test)

uniq :: Ord a => [a] -> [a]
uniq = S.toList . S.fromList

labelCounts :: [[String]] -> Map String Int
labelCounts = M.fromListWith (+) . (`zip` repeat 1) . map last
    -- M.fromListWith (\_ a -> a + 1) $ zip (map last data_) (repeat 0)

prediction :: [[String]] -> String
prediction = fst . maximumBy (compare `on` snd) . M.toList . labelCounts
    -- let counts = labelCounts data_
    -- in fst $ maximumBy (compare `on` snd) $ M.toList counts

mktests :: [[String]] -> Int -> [Test]
mktests data_ idx =
    let feature_vals = uniq $ map (!!idx) data_
        mktest v     = (==v) . (!!idx)
    in map mktest feature_vals


bestFeature :: [[String]] -> Criterion -> Int
bestFeature data_ criterion =
    let features = [0..length (head data_) - 2]  -- -2 to exclude label
        metric   = criterion data_
        branches = mktests data_
    in maximumBy (compare `on` metric . branches) features

classify :: [[String]] -> Criterion -> [String] -> Maybe String
classify data_ criterion tup
    | length (nub $ map last data_) <= 1 = Just (prediction data_)
    | otherwise =
        let ftIdx = bestFeature data_ criterion
        in case find ($tup) $ mktests data_ ftIdx of
            Just p  -> classify (filter p data_) criterion tup
            Nothing -> Nothing
