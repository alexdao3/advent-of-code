module Day06.Part1 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Data.Array (foldr, length, uncons, (:))
import Data.Array as Arr
import Data.HashMap as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set, empty, fromFoldable, difference, insert, member)
import Data.String (split, Pattern(..))
import Data.Tuple (Tuple(..), fst, snd)

inputFile :: String
inputFile = "src/day06/input.txt"

-- Newtypes
newtype Orbit
  = Orbit String

instance showOrbit :: Show Orbit where
  show (Orbit str) = str

derive instance ordOrbit :: Ord Orbit

derive instance eqOrbit :: Eq Orbit

-- Type Aliases
type Orbits
  = Array (Tuple String String)

splitOrbits :: String -> Orbits
splitOrbits str =
  let
    lines = split (Pattern "\n") str

    orbits = map (split (Pattern ")")) lines
  in
    map
      ( \orbit -> case orbit of
          [ orbited, orbiting ] -> Tuple orbiting orbited
          _ -> Tuple "" ""
      )
      orbits

getOutermostOrbits :: Orbits -> Set String
getOutermostOrbits xs =
  let
    orbiting = fromFoldable $ map fst xs

    orbited = fromFoldable $ map snd xs
  in
    difference orbiting orbited

buildPath :: Orbits -> String -> Array String
buildPath orbits orbiting =
  let
    orbitsMap = Map.fromFoldable orbits

    nextOrbit = Map.lookup orbiting orbitsMap
  in
    case nextOrbit of
      Just orbited -> orbiting : (buildPath orbits orbited)
      Nothing -> [ orbiting ]

calcOrbitsPerPath :: Array String -> Set String -> Tuple (Set String) Int
calcOrbitsPerPath xs xset = case uncons xs of
  Just { head: y, tail: ys } ->
    if member y xset then
      Tuple xset 0
    else
      add (length ys) <$> (calcOrbitsPerPath ys (insert y xset))
  Nothing -> Tuple xset 0

sumOrbits :: Array String -> Tuple (Set String) Int -> Tuple (Set String) Int
sumOrbits path (Tuple visited num) = add num <$> calcOrbitsPerPath path visited

visitedOrbits :: Array String -> Set String -> Set String
visitedOrbits path visited = case uncons path of
  Just { head: x, tail: xs } ->
    if member x visited then
      visitedOrbits xs visited
    else
      visitedOrbits xs (insert x visited)
  Nothing -> visited

main :: Effect Unit
main = do
  text <- readTextFile UTF8 inputFile
  let
    orbits = splitOrbits text

    outermost = getOutermostOrbits orbits

    paths = map (buildPath orbits) $ Arr.fromFoldable outermost

    totalOrbits = foldr sumOrbits (Tuple empty 0) paths
  log $ show totalOrbits
