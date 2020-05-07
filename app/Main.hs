module Main where

import           Lib                            ( getGag )
-- import           Types                          ( caption )

main :: IO ()
main = do
  a <- getGag "m.9gag.com/gag/agAy16W"
  case a of
    Right h -> print h
    p       -> print p
