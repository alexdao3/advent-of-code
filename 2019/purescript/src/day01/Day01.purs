module Day01 where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))

import Data.Maybe (Maybe)
import Data.Foldable (sum)
import Data.Int (fromString, decimal, toStringAs)
import Data.String (split, Pattern(Pattern))
import Data.Traversable (traverseDefault)

inputFile :: String
inputFile = "src/day01/input.txt"

splitTextFileLines :: String -> Array String
splitTextFileLines fileString = split (Pattern "\n") fileString

algoPartOne :: Int -> Int
algoPartOne num = num / 3 - 2

convertToNums :: Array String -> Maybe (Array Int)
convertToNums = traverseDefault fromString 

applyAlgo :: (Int -> Int) -> Array Int -> Int
applyAlgo algo = sum <<< map algo

backToStringBase10 :: Maybe Int -> Maybe String
backToStringBase10 = map (toStringAs decimal) 

algoPartTwo :: Int -> Int
algoPartTwo num = 
  let fuelRequired = algoPartOne num 
  in if fuelRequired < 0 then 0 else algoPartTwo fuelRequired + fuelRequired

getSum :: (Array Int -> Int) -> Array Int -> Int
getSum fn xs = fn xs

main :: Effect Unit
main =  
  let algo = algoPartTwo
      solution = map (applyAlgo algo) <<< convertToNums <<< splitTextFileLines <$> readTextFile UTF8 inputFile
  in (log <<< show) =<< solution
  
