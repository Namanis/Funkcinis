module Main where

import Lib ( run, Destination(Post), Source(Get) )
import Data.List ()
import Lib (Step(Action, Delete, Terminate))

main :: IO ()
main = run 'O' $
    replicate 9 (Action (Get 0, Post 200)) ++
    [Delete 0, Delete 200, Terminate]
