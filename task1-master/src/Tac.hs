module Tac where

import Task2Message
import Task3Parse
import Data.Char as C
import Data.List as L
import System.Environment
import System.IO 
import System.Exit

--type To = [(Int, Int, Char)]

type Grid = [[Player]]

data Player = O | B | X
            deriving (Eq, Ord, Show)

data Tree a = Node a [Tree a]
        deriving Show


putGrid :: Grid -> IO ()
putGrid =
    putStrLn . unlines . concat . interleave bar . map showRow
        where bar = [replicate ((size*4)-1) '-'] 

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
        where
            beside = foldr1 (zipWith (++))
            bar = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

generateBoard :: Grid
generateBoard = [[B,B,B],[B,B,B],[B,B,B]]


convertPlayer :: Char -> Player
convertPlayer a = 
        case a of
            'X' ->  X
            'O' ->  O
            'B' ->  B
            otherwise -> error "Bad value"

{-convertPlayerBack :: Player -> Char
convertPlayerBack a = 
        case a of
            X ->  'X'
            O ->  'O'
            B ->  'B'
            otherwise -> error "Bad value"-}


newBoard :: To -> Grid
newBoard tupleArr = newBoard' tupleArr generateBoard

newBoard' :: To -> Grid -> Grid
newBoard' tupleArr acc      | null tupleArr = acc
                            | otherwise =
                                let 
                                    (x, y, s) = head tupleArr
                                    val = x+y+(y*2)
                                    pos = convertPlayer s
                                in
                                    newBoard' (tail tupleArr) $ concat (move acc val pos)

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

move:: Grid -> Int -> Player -> [Grid]
move g i p =
    if valid g i then [chop size (xs ++ [p] ++ ys)] else []
    where (xs,B:ys) = splitAt i (concat g)


chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

empty :: Grid
empty = replicate size (replicate size B)

getNat :: String -> IO Int
getNat prompt = do 
                    putStr prompt
                    xs <- getLine
                    if xs /= [] && all isDigit xs then
                        return (read xs)
                    else
                        do 
                            putStrLn "Error: Invalid Number"
                            getNat prompt

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

from :: (Int, To) -> (To)
from (_, tupArr) = tupArr

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    pl <- getArgs
--    print pl

    msg <- getLine

    let tupArr = from(checkDupOrOrder msg)
    let board = newBoard tupArr

    play board O

    -- ---*--- It is I, the Tic Tac Toe bot ---*---
   {- s <- getArgs
    msg <- getLine
    case msg of
        "*" -> do 
            hPutStrLn stderr "My move: (2,0,X)"
            putStrLn "[\"last\",[[\"data\",[2,0,\"X\"]]]]"
            exitSuccess
        _ -> do 
            let pair = checkDupOrOrder msg
            case pair of
                (100,[]) -> exitWith $ ExitFailure 100
                (101,[]) -> exitWith $ ExitFailure 101
                (5, m)   -> exitWith $ ExitFailure 999-}

--[(0,1,'X'),(2,0,'O'),(0,0,'X'),(1,2,'O'),(0,2,'X')]

playGame :: To -> String -> Player -> IO()
playGame tupArr msg player = do
    let board = newBoard tupArr
   -- playerChar <- convertPlayerBack player

    watchGrid tupArr
     {-   if wins O board then putStrLn "Player O wins!\n"
        else if wins X board then putStrLn "Player X wins!\n"
        else putStrLn "It's a draw!\n"-}
    if won board || length tupArr == 9 then 
        exitWith $ ExitFailure 20


   
    else exitSuccess



checkDupOrOrder :: String -> (Int, To)
checkDupOrOrder s =
    case parse 0 s of
            Left _ -> (100,[])
            Right val -> case convert 0 val of 
                            Left _ -> (101,[])
                            Right m -> (5, m)

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
        where
            line = all (== p)
            rows = g
            cols = transpose g
            dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool
won g = wins O g || wins X g

winningTable :: To -> Bool
winningTable tupArr = won (newBoard tupArr)


watchGrid :: To -> IO ()
watchGrid tupArr = putGrid (newBoard tupArr)

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)= Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 12

minimax :: Tree Grid -> Player -> Tree (Grid, Player)
minimax (Node g []) _
    | wins O g = Node (g, O) []
    | wins X g = Node (g, X) []
    | otherwise = Node (g, B) []
minimax (Node g ts) p
    | turn g == p = Node (g, maximum ps) ts'
    | turn g /= p = Node (g, minimum ps) ts'
            where
                ts' = map (`minimax` p )ts
                ps = [p | Node (_,p) _ <- ts']

bestmove :: Grid -> Player -> Grid
bestmove [[B,B,B],[B,B,B],[B,B,B]] X = [[X,B,B],[B,B,B],[B,B,B]]
bestmove [[B,B,B],[B,B,B],[B,B,B]] O = [[O,B,B],[B,B,B],[B,B,B]]
bestmove g p = head [g' | Node (g', p') _ <- ts, p' == best]
        where
            tree = prune depth (gametree g p)
            Node (_, best) ts = minimax tree p

turn :: Grid -> Player
turn g = if os <= xs then O else X
        where
            os = length (filter (== O) ps)
            xs = length (filter (== X) ps)
            ps = concat g

moves :: Grid -> Player -> [Grid]
moves g p
    | won g = []
    | full g = []
    | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

full :: Grid -> Bool
full = all (/= B) . concat

next :: Player -> Player
next O = X
next B = B
next X = O

showBest :: To -> Player -> Grid
showBest tupArr p = bestmove (newBoard tupArr) p


play :: Grid -> Player -> IO ()
play g p = do
        
        putGrid g
        play' g p 

play' :: Grid -> Player -> IO ()
play' g p
    | wins O g = putStrLn "Player O wins!\n"
    | wins X g = putStrLn "Player X wins!\n"
    | full g = do
            putStrLn "It's a draw!\n"
            exitWith $ ExitFailure 12
    
    | p == O = do
            i <- getNat (prompt p)
            case move g i p of
                [] -> do
                    putStrLn "Error: Invalid move"
                    play' g p 
                [g'] -> play g' (next p)
    | p == X = do 
            putStr "Player X is thinking... "
            (play $! (bestmove g p)) (next p)

doingMove :: Grid -> Player -> IO ()
doingMove g p = do
        putStr (prompt p)
        (play $! (bestmove g p)) (next p)

--testing
--"l4:lastll4:datali0ei1e1:Oeee4:prevl4:lastll4:datali1ei2e1:Xeeeee"
--[(0,1,'O'),(1,2,'X')]

--"l4:lastll4:datali0ei1e1:Xeeee"
--[(0,1,'X')]