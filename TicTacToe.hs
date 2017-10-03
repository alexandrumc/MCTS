{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, TupleSections #-}

module TicTacToe where

import MCTS
import GameState

import System.Random
import Data.Maybe
import Data.List
{-
    Tipul celulelor (1-9)
-}
type Cell = Int

{-
    Tipul jucătorilor
-}
data Player = X | O
    deriving (Eq, Enum, Show)

{-
    Întoarce celălalt jucător.
-}
otherPlayer :: Player -> Player
otherPlayer X = O
otherPlayer O = X

{-
    *** TODO ***

    Tipul stării jocului. Ar trebui să conțină informații despre tablă
    și despre jucătorul care urmează să mute.
-}
data Board = MyBoard (Player, [(Cell, Maybe Player)])
    deriving Eq
bd :: Board
bd = MyBoard (X, [(1, (Just X)), (2, (Just O)), (3, (Just X)), (4, Nothing)])
{-
    *** TODO ***

    Întoarce lista conținuturilor celulelor, unde celule libere
    sunt reprezentate de `Nothing`.

    Ordinea celulelor este următoarea:

    789
    456
    123
-}
boardConfiguration :: Board -> [Maybe Player]
boardConfiguration (MyBoard (player, board)) = map (\x -> snd x) board

{-
    *** TODO ***

    Întoarce jucătorul care urmează să mute.
-}
boardPlayer :: Board -> Player
boardPlayer (MyBoard (player, _)) = player

{-
    *** TODO ***

    Instanțiați clasa `Show` cu tipul `Board`.
-}
instance Show Board where
    show b@(MyBoard (player, board)) = show player  ++ " " ++ show (boardConfiguration b) 

{-
    *** TODO ***

    Instanțiați clasa `GameBoard` cu tipurile `Board` și `Cell`.
-}

customFilter :: (Cell, Maybe Board) -> Bool
customFilter (_, Nothing) = False
customFilter (_, Just x)  = True

isFull :: [(Cell, Maybe Player)] -> Bool
isFull board 
    |   (elemIndex Nothing lst) == Nothing  = True
    |   otherwise                           = False
    where 
        lst = map (\x -> snd x) board


 

isWin_help :: [Maybe Player] -> Bool
isWin_help lst
    | (((filter (\x -> x == (Just X)) lst) == []) && ((head lst) /= Nothing) && ((elemIndex Nothing lst) == Nothing)) 
        || (((filter (\x -> x == (Just O)) lst) == []) && (head lst /= Nothing) && ((elemIndex Nothing lst) == Nothing))  = True
    | otherwise                                                                                                           = False 

isWin :: Board -> Bool
isWin board
    |   (isWin_help line1) || (isWin_help line2) || (isWin_help line3) || (isWin_help col1)
         || (isWin_help col2) || (isWin_help col3) || (isWin_help diag1) || (isWin_help diag2)    = True
    |   otherwise                                                                                 = False
    where
        lst = boardConfiguration board
        line1 = take 3 lst
        line2 = take 3 (drop 3 lst)
        line3 = drop 6 lst
        col1 = [lst!!0] ++ [lst!!3] ++ [lst!!6]
        col2 = [lst!!1] ++ [lst!!4] ++ [lst!!7]
        col3 = [lst!!2] ++ [lst!!5] ++ [lst!!8]
        diag1 = [lst!!0] ++ [lst!!4] ++ [lst!!8]
        diag2 = [lst!!2] ++ [lst!!4] ++ [lst!!6]

instance GameState Board Cell where
    -- playerIndex :: Board -> Int
    playerIndex (MyBoard (X, _)) = 0
    playerIndex (MyBoard (O, _)) = 1

    -- maxPlayers :: Board -> Int
    maxPlayers board = 2 

    -- successors :: Board -> [(Cell, Board)]
    successors board@(MyBoard (player, bd)) = map (\z -> ((fst z), (fromJust $ snd z))) (filter (\x -> customFilter x) (map (\y -> ((fst y), (place (fst y) board))) bd))

    -- outcome :: Board -> Outcome
    outcome bd@(MyBoard (player, board))
        | (isWin bd)                                = (Win 50) 
        | (isFull board) && ((isWin bd) == False)   = (Draw 10)
        |  otherwise                                = Ongoing

{-
    *** TODO ***

    Tabla inițială de joc. X mută primul.
-}
initialBoard :: Board
initialBoard = MyBoard (X, [(1, Nothing), (2, Nothing), (3, Nothing), (4, Nothing),
                        (5, Nothing), (6, Nothing), (7, Nothing), (8, Nothing), (9, Nothing)])

{-
    *** TODO ***

    Mută în celula dată ca parametru, în funcție de jucătorul aflat la rând,
    și schimbă jucătorul curent.

    Ordinea celulelor este explicată la funcția `boardConfiguration`.
-}
place :: Cell -> Board -> Maybe Board
place cell board@(MyBoard(player, bd)) 
    | cell < 1 || cell > 9         = Nothing
    | (snd extract) /= Nothing     = Nothing
    | otherwise                    = Just (MyBoard((otherPlayer player), ((left ++ [(cell, (Just player))]) ++ right)))
    where 
          extract = bd!!(cell-1)
          left = take (cell-1) bd 
          right = drop (cell) bd 

{-
    *** TODO ***

    Alege o mutare pornind din starea curentă.

    Utilizați `choose` din modulul `MCTS`, cu un număr dorit de iterații
    ale algoritmului.

    Pentru a juca contra calculatorului, rulați din modulul `Interactive`:

    > humanVsAI step
-}
step :: Board -> StdGen -> (Cell, Board)
step board rand = choose 100 board rand
