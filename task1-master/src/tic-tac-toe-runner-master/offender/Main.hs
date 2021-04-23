module Main where

import Data.List (repeat, take)
import Lib (Destination (Post), Source (Get, None), Step (Action, Delete, Terminate), run)

main :: IO ()
main = run 'X' $
    Delete 200 :
    (take 9 (
        (Action (None, Post 0)) :
        repeat (Action (Get 200, Post 0)))
    ) ++ [Delete 0, Delete 200, Terminate]