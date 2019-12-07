module Main where

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
inputFile = "src/input.txt"

splitTextFileLines :: String -> Array String
splitTextFileLines fileString = split (Pattern "\n") fileString

applyAlgo :: Int -> Int
applyAlgo num = num / 3 - 2

convertToNums :: Array String -> Maybe (Array Int)
convertToNums = traverseDefault fromString 

getPartOneSum :: Array Int -> Int
getPartOneSum = sum <<< map applyAlgo

backToStringBase10 :: Maybe Int -> Maybe String
backToStringBase10 = map (toStringAs decimal) 

-- applyAlgoPartTwo :: (map <<< map )

main :: Effect Unit
main =  
  let something = backToStringBase10 <<< map getPartOneSum <<< convertToNums <<< splitTextFileLines <$> readTextFile UTF8 inputFile
  in (log <<< show) =<< something
  