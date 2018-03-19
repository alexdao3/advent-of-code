module Main where

import System.Environment

type Count = Int
type Multiplier = Int
type Stack = String
type NextChar = Char
type GarbageCount = Int
data State = OpenGroup | OpenGarbage | Negate deriving Eq
data NextState = NextState String Count Multiplier GarbageCount

deriveState :: Char -> State
deriveState '{' = OpenGroup
deriveState '<' = OpenGarbage
deriveState '!' = Negate

handleOpenGroup :: Char -> NextState -> NextState
handleOpenGroup '<' (NextState stack count multiplier gc) =
  NextState (stack ++ "<") count multiplier gc
handleOpenGroup '!' (NextState stack count multiplier gc) =
  NextState (stack ++ "!") count multiplier gc
handleOpenGroup '{' (NextState stack count multiplier gc) =
  NextState (stack ++ "{") count (multiplier + 1) gc
handleOpenGroup '}' (NextState stack count multiplier gc) =
  NextState (init stack) (multiplier + count) (multiplier - 1) gc
handleOpenGroup _ nextState = nextState

handleGarbage :: Char -> NextState -> NextState
handleGarbage '!' (NextState stack count multiplier gc) =
  NextState (stack ++ "!") count multiplier gc
handleGarbage '>' (NextState stack count multiplier gc) =
  NextState (init stack) count multiplier gc
handleGarbage _ (NextState stack count multiplier gc) = NextState stack count multiplier $ gc + 1

advance :: String -> (Count, GarbageCount)
advance (x:xs) =
  advance' xs (NextState "{" 0 1 0)
  where
    advance' [] (NextState _ count _ gc) = (count, gc)
    advance' (x:xs) nextState = advance' xs $ handleNextState x nextState

handleNextState :: Char -> NextState -> NextState
handleNextState char (NextState stack count multiplier gc)
  -- What is the way to there is bind a variable to a pattern during pattern matching
  | stack /= "" && (deriveState $ last stack) == OpenGroup     = handleOpenGroup char (NextState stack count multiplier gc)
  | stack /= "" && (deriveState $ last stack) == OpenGarbage   = handleGarbage char (NextState stack count multiplier gc)
  | stack /= "" && (deriveState $ last stack) == Negate        = NextState (init stack) count multiplier gc
  | otherwise                                                  = NextState stack count multiplier gc

main :: IO ()
main = do
  (file:_) <- getArgs
  contents <- readFile file
  print $ advance contents
