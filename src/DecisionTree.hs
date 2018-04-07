{--
 - DecisionTree.hs
 -
 - Simple decision tree implementation for Haskell.
 -
 - Will Badart
 - created: FEB 2018
 -}

module DecisionTree (classify) where

import Data.Function (on)
import Data.List (find, maximumBy)
import Data.Map (Map)
import qualified Data.Map as M

import Entropy (Criterion, Test, uniq)

labelCounts :: [[String]] -> Map String Int
labelCounts = M.fromListWith (+) . (`zip` repeat 1) . map last

prediction :: [[String]] -> String
prediction = fst . maximumBy (compare `on` snd) . M.toList . labelCounts

mktests :: Int -> [[String]] -> [Test]
mktests idx = map (\v -> (==v) . (!!idx)) . uniq . map (!!idx)

bestFeature :: Criterion -> [[String]] -> Int
bestFeature criterion data_ =
    let features = [0..length (head data_) - 2]  -- -2 to exclude label
        branches = (`mktests` data_)
    in  maximumBy (compare `on` criterion data_ . branches) features

classify :: Criterion -> [[String]] -> [String] -> Maybe String
classify criterion data_ tup
    | length (uniq $ map last data_) <= 1 = Just (prediction data_)
    | otherwise =
        let ftIdx = bestFeature criterion data_
        in case find ($tup) $ mktests ftIdx data_ of
            Just p  -> classify criterion (filter p data_) tup
            Nothing -> Nothing
