module Day02.Part1 where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))

import Data.Array (drop, index, length, snoc, take, updateAt)
import Data.Maybe (Maybe(..))
import Data.Int (fromString)
import Data.String (split, Pattern(Pattern))
import Data.Traversable (traverseDefault)

inputFile :: String
inputFile = "src/day02/input.txt"

runOps :: Int -> Array Int -> Array Int ->  Maybe (Array Int)
runOps takeNum [code, xIndex, yIndex, updateIndex] fullProgram  = 
    if code == 99 then (Just fullProgram)
    else do
        xValue <- index fullProgram xIndex
        yValue <- index fullProgram yIndex
        let calcXY = if code == 1 then xValue + yValue
                        else xValue * yValue
        newProgram <- updateAt updateIndex calcXY fullProgram 
        runOps (takeNum + 4) (take 4 (drop takeNum fullProgram))  newProgram 
runOps _ takeNum fullProgram  = 
    pure fullProgram

main :: Effect Unit
main =  do
  text <- readTextFile UTF8 inputFile
  -- listNums :: Maybe (Array Int)
  let listNums = traverseDefault fromString <<< (split (Pattern ",")) $ text  
      something = (\ nums -> runOps 4 (take 4 nums) nums) =<< listNums
  log $ show something
  
