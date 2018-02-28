{- Entropy.hs
 -
 - Calculate the entropy on a dataset given a
 - specific branching scheme.
 -
 - Will Badart
 - created: FEB 2018
 -}

module Entropy
( conditionalEntropy
, entropy
, informationGain
) where

import Data.List (genericLength, nub)


-- =======
-- Helpers
-- =======

hasLabel :: String -> [String] -> Bool
hasLabel label = ((==label) . last)


countBy :: Num i => (a -> Bool) -> [a] -> i
countBy p = genericLength . filter p


labels :: [[String]] -> [String]
labels = nub . map last


lg :: Floating i => i -> i
lg = logBase 2


-- ============
-- Calculations
-- ============

entropy :: [[String]] -> Float
entropy data_ =
    let ct l = countBy (hasLabel l) data_
        probs = [ct l / (genericLength data_) | l <- labels data_]
    in negate $ sum [p * (lg p) | p <- probs]


conditionalEntropy :: [[String]] -> [([String] -> Bool)] -> Float
conditionalEntropy data_ branches =
    let ct p = countBy p data_
        freqs p = [ct (\tup -> p tup && hasLabel l tup) | l <- labels data_]
        zipper f c = if f /= 0 then (f / c) * (lg (f / c)) else 0
        fracs p = zipWith zipper (freqs p) (repeat $ ct p)
        total p = negate $ sum $ fracs p
        ent e p = e + (ct p / genericLength data_) * (total p)
    in foldl ent 0 branches


informationGain :: [[String]] -> [([String] -> Bool)] -> Float
informationGain data_ branches =
    entropy data_ - conditionalEntropy data_ branches
