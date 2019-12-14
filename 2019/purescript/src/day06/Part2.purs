module Day06.Part2 where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))
import Data.Array ((:))
import Data.HashMap as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set, fromFoldable, intersection, difference, size)
import Data.String (split, Pattern(..))
import Data.Tuple (Tuple(..), fst, snd, swap)

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

reverseOrbits :: Orbits -> Orbits
reverseOrbits orbits = map swap orbits

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

main :: Effect Unit
main = do
  text <- readTextFile UTF8 inputFile
  let
    orbits = splitOrbits text

    outermost = getOutermostOrbits orbits

    youPath = fromFoldable $ buildPath orbits "YOU"

    sanPath = fromFoldable $ buildPath orbits "SAN"

    sharedPathSize = size $ intersection youPath sanPath

    -- take combined length of both paths
    -- subtract 2x the shared path (at some point, the path from YOU intersects with the path from SAN, since both terminate at COM)
    -- leaves the combined length of the distinct paths (subtract 2 to remove YOU and SAN orbits)
    diff = (size youPath) + (size sanPath) - (2 * sharedPathSize) - 2
  log $ show diff
