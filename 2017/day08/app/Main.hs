module Main where

import System.Environment
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.List (maximumBy)

-- break down one line
-- need to break the 5 tokens into meaning: char, increase/decrease, howMuch, condition
-- returns an array of instructions
-- needs to take each instruction and action on the hashmap
data Direction = Increase | Decrease deriving Show
data Instruction = Instruction {
                    getRegister :: String,
                    getDirection :: Direction,
                    howMuch :: Int,
                    condition :: Bool
                  } | NotInstruction deriving Show
data Operator = GT | LT | GTE | LTE | EQ | NEQ

lookUpValue :: String -> HM.HashMap String Int -> Int
lookUpValue key map = fromMaybe 0 $ HM.lookup key map

parseDirection :: String -> Direction
parseDirection direction = if direction == "inc" then Increase else Decrease

parseCondition :: HM.HashMap String Int -> [String] -> Bool
parseCondition map [_, register, operator, number] =
  case operator of
    "<"  -> lookUpValue register map < read number
    "<=" -> lookUpValue register map <= read number
    ">"  -> lookUpValue register map > read number
    ">=" -> lookUpValue register map >= read number
    "==" -> lookUpValue register map == read number
    "!=" -> lookUpValue register map /= read number

splitLine :: [String] -> HM.HashMap String Int -> Instruction
splitLine (register:direction:howMuch:condition) hashMapRegister  =
  Instruction register (parseDirection direction) (read howMuch) (parseCondition hashMapRegister condition)
splitLine _ _ = NotInstruction

createInstructions :: [[String]] -> [HM.HashMap String Int -> Instruction]
createInstructions instructions =
  map splitLine instructions

runInstruction :: (HM.HashMap String Int -> Instruction) -> HM.HashMap String Int -> HM.HashMap String Int
runInstruction f hashMapRegister =
  case (f hashMapRegister) of
    Instruction register Increase amount True -> updateRegister register amount hashMapRegister
    Instruction register Decrease amount True -> updateRegister register (negate amount) hashMapRegister
    _ -> hashMapRegister

updateRegister :: String -> Int -> HM.HashMap String Int -> HM.HashMap String Int
updateRegister key amount hashMapRegister =
  case HM.lookup key hashMapRegister of
    Nothing -> HM.insert key amount hashMapRegister
    Just _  -> HM.adjust ((+) amount) key hashMapRegister

runInstructions :: [HM.HashMap String Int -> Instruction] -> HM.HashMap String Int
runInstructions thunks =
  runInstructions' thunks HM.empty
  where
    runInstructions' (f:[]) hashMapRegister = runInstruction f hashMapRegister
    runInstructions' (f:fs) hashMapRegister = runInstructions' fs (runInstruction f hashMapRegister)

getMaxRegister :: HM.HashMap String Int -> (String, Int)
getMaxRegister hashMapRegister =
  maximumBy (\(_, val) (_, val') -> compare val val') $ HM.toList hashMapRegister

getInstr (Instruction _ _ amount _) = amount
getInstr _ = 0

main :: IO ()
main = do
  (fileName:args) <- getArgs
  file <- readFile fileName
  let xs = getMaxRegister . runInstructions . createInstructions . map words . lines $ file
  print (show xs)

