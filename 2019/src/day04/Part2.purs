module Day04.Part2 where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Data.Array (all, any, concat, filter, foldr, length, uncons, zip, (..))
import Data.HashMap as HM
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)

splitNum :: Int -> Array Int-> Array Int
splitNum x y =
    let digit = mod x 10
        remainder = (x - digit) / 10
    in
        if x == 0 then y
        else concat [splitNum remainder y, [digit], y]

isEqualOrIncreasing :: Array Int -> Boolean
isEqualOrIncreasing xs = case uncons xs of
            Just {head: y, tail: ys} -> 
                let pairs = zip xs ys
                    isAtLeastEqual =  all (\ (Tuple a b) -> a <= b) pairs
                    consecutiveEqual = map fst $ filter (\ (Tuple a b) -> a == b) pairs
                    -- can clean this up to use one of the maybe functions that operates on maybe instead of case matching
                    matchHowMany = foldr (\ k obj -> HM.alter (\ mVal -> case mVal of 
                        Just val -> Just (val + 1)
                        Nothing -> Just 1) k obj) HM.empty consecutiveEqual
                    -- check that there is at least one number with only a single entry indicating a pair
                    atLeastJustAPair = any (\ num -> num == 1) (HM.values matchHowMany)
                in
                    isAtLeastEqual && atLeastJustAPair
            Nothing -> false

main :: Effect Unit
main =  
  let nums = filter (\ num -> isEqualOrIncreasing $ splitNum num []) $ 136760 .. 595730 
  in
    log $ show $ length nums
  
