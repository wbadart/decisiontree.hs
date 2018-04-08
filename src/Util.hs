{--
 - Util.hs
 -
 - Miscellaneous helpers for use throughout the program.
 -
 - Will Badart
 - created: APR 2018
 -}

module Util
( uniq
, enumerate
) where

import qualified Data.Set as S

uniq :: Ord a => [a] -> [a]
uniq = S.toList . S.fromList

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

skipIdx :: Int -> [a] -> [a]
skipIdx k = foldr skip [] . enumerate
  where skip (i, x) acc = if i == k then acc else x:acc
