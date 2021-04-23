module Task1

where

import Data.List as L 
import Data.Char as C

import Task1Message


parseChar :: String -> (Char, String) --parseChar
parseChar ('l':'1' :':':'v':'1':':': pos :t) = 
    case t of
        ('1':r) -> (pos, t)
        otherwise -> error "Can't read char"
parseChar _ = error "Invalid message"


parseInt :: String -> (Int, String)  --parseInt
parseInt ('1' :':':'x':'1':':': pos :t) =
    case t of
        ('e':r) -> (read (pos:[]), r)
        otherwise -> error "Can't read int"
parseInt _ = error "Invalid message"



parseIntTuple :: String -> ((Int, Char), String)  --Combine Int and Char into tuple
parseIntTuple str =
    ((i, c), r2)
    where
        (c, r1) = parseChar str
        (i, r2) = parseInt r1

parseListOfTuples :: String -> ([(Int, Char)], String)   --Make list of tuples
parseListOfTuples ('l':r) = parseListOfTuples' r []
parseListOfTuples _ = error "Invalid string"


parseListOfTuples' :: String -> [(Int, Char)] -> ([(Int, Char)], String)
parseListOfTuples' ('e':r) acc =
    (acc, r)
parseListOfTuples' s acc =
    let
        ((c, i), left) = parseIntTuple s
    in
        parseListOfTuples' left (acc ++ (c,i):[])


parseListOfLists :: String -> [[(Int, Char)]]    --Make list of lists of tuples
parseListOfLists ('l':r) = parseListOfLists' r []
parseListOfLists _ = error "Invalid string"

parseListOfLists' :: String -> [[(Int, Char)]] -> [[(Int, Char)]]
parseListOfLists' ('e':r) acc = (L.reverse acc)
parseListOfLists' s acc =
    let
        (l, left) = parseListOfTuples s
    in
        parseListOfLists' left (l : acc)

parse :: Int    -- ^ Size of the matrix (number of columns or rows)
      -> String -- ^ Encoded message
      -> From   -- ^ Parsed data structure
parse size message = parseListOfLists message


convert :: Int  -- ^ Size of the matrix (number of columns or rows)
        -> From -- ^ Parsed matrix
        -> To   -- ^ Converted matrix 
convert size from = produceFinalTo from 

convertListInt :: [(Int, Char)] -> [Int]   --[(0, 'X'), (1, 'O')] -> [0,1]
convertListInt list = map fst list

convertListOfListsInt :: [[(Int, Char)]] -> [Int]  --[[(0, 'X'), (1, 'O')], [(1, 'X')]] -> [0,1,1]
convertListOfListsInt lists =  concat $ map (\x -> (convertListInt x)) lists


convertListChar :: [(Int, Char)] -> [Char]  --[(0, 'X'), (1, 'O')] -> "XO"
convertListChar list = map snd list


convertListOfListsChar :: [[(Int, Char)]] -> [Char]   --[[(0, 'X'), (1, 'O')], [(1, 'X')]] -> "XOX"
convertListOfListsChar lists = concat $ map (\x -> (convertListChar x)) lists

convertPos :: Int -> [(Int, Char)] -> [Int] --[(0, 'X'), (1, 'O')] -> [0,0]
convertPos i ints = map  (\x -> i) ints
{-
convertListPos :: [[(Int, Char)]] -> [Int]
<<<<<<< HEAD
convertListPos lists = concat ( map (\x -> convertPos(head (elemIndices x lists)) x) lists) 
=======
convertListPos lists = concat $ map (\x -> convertPos(head $ elemIndices x lists) x) lists
>>>>>>> d06e262 (Initial commit)


produceFinalTo :: From -> To
produceFinalTo from =
    (convertListOfListsInt from, convertListPos from, convertListOfListsChar from)
<<<<<<< HEAD




=======
-}



