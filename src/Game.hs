module Game where

import Control.Monad (forever)
import System.Exit (exitSuccess)
import Data.Semigroup
import Data.Monoid (mempty)

data Point = Point { x :: Integer
                   , y :: Integer }
             deriving (Eq, Show)

sumPoints a b = Point (x a + x b) (y a + y b)

instance Semigroup Point where
  (<>) = sumPoints

instance Monoid Point where
  mempty = Point 0 0
  mappend = (<>)

movePoint :: Game -> Char -> IO Game
movePoint (Game p) c =
  return
  $ Game
  $ case c of
      'a' -> p <> Point (-1) 0
      's' -> p <> Point 0 (-1)
      'd' -> p <> Point 1 0
      'w' -> p <> Point 0 1

data Game = Game Point deriving Show

runGame :: Game -> IO ()
runGame game = forever $ do
  putStrLn (show game)
  putStrLn "Move you point!"
  c <- getChar
  exitWhenInvalid c
  (movePoint game c) >>= runGame

exitWhenInvalid :: Char -> IO ()
exitWhenInvalid c
  | notElem c "wasd" = exitSuccess
  | otherwise = return ()

main :: IO ()
main = do
  runGame (Game $ Point 0 0)
