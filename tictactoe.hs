{-
  Skeleton-code available here, from the IN5630 UiO course:
  https://www.uio.no/studier/emner/matnat/ifi/IN5630/v24/exercises/week-01/TicTacToe.hs
-}

import Data.List
import System.IO

data Player = Cross | Nought
            deriving (Show, Eq)

data Cell = Move Player | Empty
          deriving (Show, Eq)

type Row = [Cell]
type Board = [Row]

makeBoard :: Cell -> Board
makeBoard c = take 3 emptyRows
  where emptyRows = repeat emptyRow
        emptyRow = take 3 (repeat c)

type Position = (Int, Int)

-- You should probably take a good look at this function until you
-- understand it. Keep in mind that it does not check whether the move
-- is valid.
move :: Position -> Board -> Player -> Board
move (x, y) board player = rowsBefore ++ (changed : rowsAfter)
  where (rowsBefore, toBeChanged : rowsAfter) = splitAt x board
        changed = cellsBefore ++ (newCell : cellsAfter)
        (cellsBefore, old : cellsAfter) = splitAt y toBeChanged
        newCell = Move player

-- Type for representing the state of a game. That is, which should
-- take the next move, and what does the board look like.
type GameState = (Player, Board)

startState :: GameState
startState = (Cross, makeBoard Empty)

makeMove :: Position -> GameState -> GameState
makeMove (x, y) (Cross, b)  = (Nought, move (x,y) b Cross)
makeMove (x, y) (Nought, b) = (Cross, move (x,y) b Nought)


-- checks how many Empty on board.
validMove :: Position -> GameState -> Bool
validMove x (p, b) = (length $ filter (== Empty) $ concat $ concat $ makeMove x (p, b)) /=  (length $ filter (== Empty) $ concat $ concat $ (p,b))

-- same function as above
validMove' :: Position -> GameState -> Bool
validMove' (x, y) (p, b) = case b !! x !! y of
  Empty -> True
  _     -> False

allMoves :: [Position]
allMoves = [ (x, y) | x <- [0 .. 2], y <- [0 .. 2] ]

allValidMoves :: GameState -> [Position]
allValidMoves (p, b) = [x | x <- allMoves, (validMove x (p, b)) == True]

-- The type for representing game trees.  Each node have a game state
-- and a list of *valid* moves leading to (sub-) game trees
data GameTree = Node GameState [(Position, GameTree)] deriving Show

-- Grow the game tree starting in a given state
makeTree :: GameState -> GameTree
makeTree (p, b) = Node (p, b) [(b', makeTree (makeMove b' (p, b))) | b' <- allValidMoves (p, b)]


allNodes :: GameTree -> [GameState]
allNodes (Node gs subs) = gs : concatMap (allNodes . snd) subs


some = unlines ["XOX", "OO-", "XOO"] -- win O
full = unlines ["XOX", "OXX", "XOO"] -- draw
emp = unlines ["---", "---", "---"]

readBoard :: String -> Board
readBoard = map readRow . lines
    where readRow = map readCell

readCell :: Char -> Cell
readCell '-' = Empty
readCell 'X' = Move Cross
readCell 'O' = Move Nought

showBoard :: Board -> String
showBoard = unlines . map showRow
    where showRow = map showCell

showCell :: Cell -> Char
showCell Empty = '-'  
showCell (Move Cross) = 'X' 
showCell (Move Nought) = 'O'

wins :: GameState -> Bool
wins (p, b) = any line (rows ++ cols ++ diags)
              where
                line = all (== Move p)
                rows = b
                cols = transpose b
                diags = [diag b, diag (reverse b)]
                  where
                    diag b = [b !! n !! n | n <- [0..2]]



-- WIP


-- todo: implement minimax and interface to make it playable vs computer

{-

minimax :: GameTree -> GameState
minimax (Node b [])
  | wins Nought b = Node (b, Nought) []
  | wins Cross b  = Node (b, Cross) []
  | otherwise     = Node (b, Empty) []
minimax (Node b ts)
  | turn b == Nought = ...
  | turn b == Cross = ...


bestMove :: GameState -> Board
-- bestMove (p, b)


prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move in the form (x, y) in [0,2]: "

sti :: String -> (Int, Int)
sti str = read str :: (Int, Int)

play :: GameState -> IO ()
play (p, b)
 | wins (Cross, b)  = putStrLn "Player X wins.\n"
 | wins (Nought, b) = putStrLn "Player O wins.\n"
 | allValidMoves (p, b) == [] = putStrLn "Tie.\n"
 | p == Cross = do (x, y) <- sti (prompt p)
                   case validMove (x, y) (p, b) of
                    False -> putStrLn $ "Invalid move: " ++ show (x, y)
                             play (p, b)
                    True  -> play (makeMove (x, y) (p, b))
 | p == Nought = do putStrLn $ "Player " ++ show p ++ "is making its move..."
                    (play $! (p, bestMove (p, b)))
                    readBoard bestMove ++ "\n"


main :: IO ()
main = do hSetBuffering stdout NoBuffering
          play startState


-}