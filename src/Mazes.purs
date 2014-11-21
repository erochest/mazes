module Mazes where


import Prelude
import Control.Monad
import Control.Monad.Eff.Random
import Data.Graph
import Debug.Trace


main = do
    print "Hello, world!"
    random >>= print <<< ((++) "Random!! ") <<< show
