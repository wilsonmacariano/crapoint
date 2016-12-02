module Something where

import Control.Monad.Trans.State
import System.Random

rollDie :: State StdGen Int
rollDie = state $ randomR (1, 6)

{-
rollDie :: State StdGen Int
rollDie = do generator <- get
             let (value, newGenerator) = randomR (1,6) generator
             put newGenerator
             return value
-}
