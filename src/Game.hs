module Game where

import Data.Semigroup
import Control.Monad (forever)
import System.Exit (exitSuccess)

data Point = Point { x :: Integer
                   , y :: Integer }
             deriving (Eq, Show)

sumPoints a b = Point (x a + x b) (y a + y b)

instance Semigroup Point where
  (<>) = sumPoints

instance Monoid Point where
  mempty = Point 0 0
  mappend = (<>)

movePoint :: Char -> Maybe Point
movePoint c
  | c == 'a' = Just $ mempty <> Point (-1) 0
  | c == 's' = Just $ mempty <> Point 0 (-1)
  | c == 'd' = Just $ mempty <> Point 1 0
  | c == 'w' = Just $ mempty <> Point 0 1
  | otherwise = Nothing

doMovePoint :: Char -> IO ()
doMovePoint = putStrLn
              . show
              . movePoint

something :: Char -> IO ()
something a
  | elem a "wasd" = doMovePoint a
  | otherwise = exitSuccess

main :: IO ()
main = forever $ do
  line <- getChar
  something line
