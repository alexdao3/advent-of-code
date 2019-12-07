module Day02 where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Node.FS.Sync (readTextFile)
import Node.Encoding (Encoding(UTF8))

-- import Data.Maybe (Maybe)
-- import Data.Foldable (sum)
-- import Data.Int (fromString, decimal, toStringAs)
-- import Data.String (split, Pattern(Pattern))
-- import Data.Traversable (traverseDefault)

inputFile :: String
inputFile = "src/day02/input.txt"

main :: Effect Unit
main =  
  log =<< readTextFile UTF8 inputFile
  
