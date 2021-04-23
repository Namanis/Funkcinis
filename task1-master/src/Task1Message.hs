module Task1Message
where

-- ┌       ┐
-- │ X O X │
-- │ X O O │
-- │ X X O │
-- └       ┘
-- seed: -6542337934167652300
-- encoding: BenArr
-- from: LIL
-- to: ARR

size :: Int
size = 3

message :: String
message = "lll1:v1:X1:x1:0el1:v1:O1:x1:1el1:v1:X1:x1:2eell1:v1:X1:x1:0el1:v1:O1:x1:1el1:v1:O1:x1:2eell1:v1:X1:x1:0el1:v1:X1:x1:1el1:v1:O1:x1:2eee"

type From = [[(Int, Char)]]
type To = ([Int], [Int], [Char])

expectedFrom :: From
expectedFrom = [[(0, 'X'), (1, 'O'), (2, 'X')], [(0, 'X'), (1, 'O'), (2, 'O')], [(0, 'X'), (1, 'X'), (2, 'O')]]

expectedTo :: To
expectedTo = ([0, 1, 2, 0, 1, 2, 0, 1, 2], [0, 0, 0, 1, 1, 1, 2, 2, 2], ['X', 'O', 'X', 'X', 'O', 'O', 'X', 'X', 'O'])
