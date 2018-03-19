module Main where

import System.Environment

type Count = Int
type Multiplier = Int
type Stack = String
type NextChar = Char
data State = OpenGroup | OpenGarbage | Negate deriving Eq
data NextState = NextState String Count Multiplier

deriveState :: Char -> State
deriveState '{' = OpenGroup
deriveState '<' = OpenGarbage
deriveState '!' = Negate

handleOpenGroup :: Char -> NextState -> NextState
handleOpenGroup '<' (NextState stack count multiplier) =
  NextState (stack ++ "<") count multiplier
handleOpenGroup '!' (NextState stack count multiplier) =
  NextState (stack ++ "!") count multiplier
handleOpenGroup '{' (NextState stack count multiplier) =
  NextState (stack ++ "{") count (multiplier + 1)
handleOpenGroup '}' (NextState stack count multiplier) =
  NextState (init stack) (multiplier + count) (multiplier - 1)
handleOpenGroup _ nextState = nextState

handleGarbage :: Char -> NextState -> NextState
handleGarbage '!' (NextState stack count multiplier) =
  NextState (stack ++ "!") count multiplier
handleGarbage '>' (NextState stack count multiplier) =
  NextState (init stack) count multiplier
handleGarbage _ nextState = nextState

advance :: String -> Int
advance (x:xs) =
  advance' xs (NextState "{" 0 1)
  where
    advance' [] (NextState _ count _) = count
    advance' (x:xs) nextState = advance' xs $ handleNextState x nextState

handleNextState :: Char -> NextState -> NextState
handleNextState char (NextState stack count multiplier)
  -- What is the way to there is bind a variable to a pattern during pattern matching
  | stack /= "" && (deriveState $ last stack) == OpenGroup     = handleOpenGroup char (NextState stack count multiplier)
  | stack /= "" && (deriveState $ last stack) == OpenGarbage   = handleGarbage char (NextState stack count multiplier)
  | stack /= "" && (deriveState $ last stack) == Negate        = NextState (init stack) count multiplier
  | otherwise                                   = NextState stack count multiplier

main :: IO ()
main = do
  (file:_) <- getArgs
  contents <- readFile file
  print $ advance contents
