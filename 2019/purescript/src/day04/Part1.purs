module Day04.Part1 where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))

import Data.Array (all, any, concat, filter, length, uncons, zip, (..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

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
                    consecutiveEqual = any (\ (Tuple a b) -> a == b) pairs
                in
                    isAtLeastEqual && consecutiveEqual
            Nothing -> false

main :: Effect Unit
main =  
  let nums = filter (\ num -> isEqualOrIncreasing $ splitNum num []) $ 136760 .. 595730 
  in
    log $ show $ length nums
  
