module Day02.Part2 where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))

import Control.MonadZero (guard)
import Data.Array (drop, find, filter, index, length, snoc, take, updateAt, zip, (..))
import Data.Maybe (Maybe(..))
import Data.Int (fromString)
import Data.String (split, Pattern(Pattern))
import Data.Tuple (Tuple(..))
import Data.Traversable (traverseDefault)
import Prim.Row (Cons)

inputFile :: String
inputFile = "src/day02/input.txt"

runOps :: Int -> Array Int -> Array Int ->  Maybe (Array Int)
runOps takeNum [code, xIndex, yIndex, updateIndex] fullProgram  = 
    if code == 99 then (Just fullProgram)
    else do
        xValue <- index fullProgram xIndex
        yValue <- index fullProgram yIndex
        -- lazy handling. should check code == 2 for multiplication 
        let calcXY = if code == 1 then xValue + yValue
                        else xValue * yValue
        newProgram <- updateAt updateIndex calcXY fullProgram 
        runOps (takeNum + 4) (take 4 (drop takeNum fullProgram))  newProgram 
runOps _ takeNum fullProgram  = 
    pure fullProgram

allCombinations ::  Array (Tuple Int Int)
allCombinations = do
    a <- 0 .. 99
    b <- 0 .. 99
    pure $ Tuple a b
    
checkCombination :: Tuple Int Int -> Array Int -> Boolean
checkCombination (Tuple noun verb) program =
    -- newProgram :: Maybe (Array Int)
    let 
        a = do
            withNounAndVerb <- updateAt 1 noun =<< updateAt 2 verb program
            terminatedProgram <- runOps 4 (take 4 withNounAndVerb) withNounAndVerb 
            index terminatedProgram 0
    in 
        case a of
            Just x -> x == 19690720
            Nothing -> false

main :: Effect Unit
main =  do
  text <- readTextFile UTF8 inputFile
  let listNums = traverseDefault fromString <<< (split (Pattern ",")) $ text  
      vals = (\ nums -> find (\ combo -> checkCombination combo nums ) allCombinations) =<< listNums
  log $ show vals
  
