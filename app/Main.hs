module Main where

import           Lib                            ( getGag )
-- import           Types                          ( caption )

main :: IO ()
main = do
  --a <- getGag "m.9gag.com/gag/agAy16W"
  a <- getGag "vk.com/wall-52537634_1543772"
  case a of
    Right h -> print h
    p       -> print p
