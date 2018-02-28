{- Entropy.hs
 -
 - Calculate the entropy on a dataset given a
 - specific branching scheme.
 -
 - Will Badart
 - created: FEB 2018
 -}

module Entropy
( informationGain
, gainRatio
, Criterion
, Test
) where

import Data.List (foldl', genericLength, nub)


type Test = [String] -> Bool
type Criterion = [[String]] -> [Test] -> Float


-- =======
-- Helpers
-- =======

hasLabel :: String -> [String] -> Bool
hasLabel label = (==label) . last

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
        probs = [ct l / genericLength data_ | l <- labels data_]
    in negate $ sum [p * lg p | p <- probs]

conditionalEntropy :: [[String]] -> [[String] -> Bool] -> Float
conditionalEntropy data_ branches =
    let ct p = countBy p data_
        freqs p = [ct (\tup -> p tup && hasLabel l tup) | l <- labels data_]
        fracs p = [if f /= 0 then (f / c) * lg (f / c) else 0
                    | (f, c) <- zip (freqs p) (repeat $ ct p)]
        total p = negate $ sum $ fracs p
        ent p = (ct p / genericLength data_) * total p
    in sum $ map ent branches

informationGain :: Criterion
informationGain data_ branches =
    entropy data_ - conditionalEntropy data_ branches

splitInfo :: Criterion
splitInfo data_ branches =
    let bits c = (c / genericLength data_) * lg (c / genericLength data_)
    in negate $ sum $ map (bits . (`countBy` data_)) branches

gainRatio :: Criterion
gainRatio data_ branches =
    informationGain data_ branches / splitInfo data_ branches
