{-# LANGUAGE LambdaCase #-}

module Lib where

import Safe

data Token = X | O deriving (Show, Eq)

type Cell = Maybe Token

data Board = Board Cell Cell Cell Cell Cell Cell Cell Cell Cell

data Exception = CellTaken | InvalidCell deriving Show

data Input = Move Int | Undo

emptyBoard :: Board
emptyBoard = Board Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

initTTT :: IO ()
initTTT = do
    putStrLn $ drawBoard emptyBoard
    ticTacToe [emptyBoard] X

negateToken :: Token -> Token
negateToken X = O
negateToken _ = X

parseInput :: String -> Maybe Input
parseInput = \case
    "0" -> Just $ Move 0
    "1" -> Just $ Move 1
    "2" -> Just $ Move 2
    "3" -> Just $ Move 3
    "4" -> Just $ Move 4
    "5" -> Just $ Move 5
    "6" -> Just $ Move 6
    "7" -> Just $ Move 7
    "8" -> Just $ Move 8
    "9" -> Just $ Move 9
    "u" -> Just $ Undo
    "undo" -> Just $ Undo
    _ -> Nothing

ticTacToe :: [Board] -> Token -> IO ()
ticTacToe b p = do
    putStrLn $ "You are " ++ show p ++ ". Please input cell bitch."
    input <- parseInput <$> getLine
    let first:rest = b

    case input of
        Just Undo -> do
            case length b of
                1 -> do
                    putStrLn "Can't undo on first move"
                    putStrLn $ drawBoard $ head b
                    ticTacToe b p
                _ -> do
                    putStrLn $ drawBoard $ head rest
                    ticTacToe (tail b) $ negateToken p
        Just (Move x) -> either
            (\e -> do
                print e
                putStrLn $ drawBoard $ first
                ticTacToe b p
            )
            (\b' -> case checkBoard b' of
                    Just t -> do
                        putStrLn $ drawBoard b'
                        putStrLn $ show t ++ " wins bitch."
                    _ -> do
                        putStrLn $ drawBoard b'
                        ticTacToe (b' : b) $ negateToken p
            )
            (occupyWallStreet first p x)
        _ -> do
            putStrLn "Must be a number between 1 and 9 bitch."
            putStrLn $ drawBoard first
            ticTacToe b p

allThree :: Cell -> Cell -> Cell -> Bool
allThree (Just a) (Just b) (Just c) = (a == b) && (b == c)
allThree _ _ _ = False

checkBoard :: Board -> Maybe Token
checkBoard (Board a b c _ _ _ _ _ _) | allThree a b c = a
checkBoard (Board _ _ _ a b c _ _ _) | allThree a b c = a
checkBoard (Board _ _ _ _ _ _ a b c) | allThree a b c = a
checkBoard (Board a _ _ b _ _ c _ _) | allThree a b c = a
checkBoard (Board _ a _ _ b _ _ c _) | allThree a b c = a
checkBoard (Board _ _ a _ _ b _ _ c) | allThree a b c = a
checkBoard (Board a _ _ _ b _ _ _ c) | allThree a b c = a
checkBoard (Board _ _ a _ b _ c _ _) | allThree a b c = a
checkBoard _ = Nothing

occupyWallStreet :: Board -> Token -> Int -> Either Exception Board
occupyWallStreet (Board (Just _) _ _ _ _ _ _ _ _) _ 1 = Left CellTaken
occupyWallStreet (Board _ c2 c3 c4 c5 c6 c7 c8 c9) p 1 =
    Right $ Board (Just p) c2 c3 c4 c5 c6 c7 c8 c9

occupyWallStreet (Board _ (Just _) _ _ _ _ _ _ _) _ 2 = Left CellTaken
occupyWallStreet (Board c1 _ c3 c4 c5 c6 c7 c8 c9) p 2 =
    Right $ Board c1 (Just p) c3 c4 c5 c6 c7 c8 c9

occupyWallStreet (Board _ _ (Just _) _ _ _ _ _ _) _ 3 = Left CellTaken
occupyWallStreet (Board c1 c2 _ c4 c5 c6 c7 c8 c9) p 3 =
    Right $ Board c1 c2 (Just p) c4 c5 c6 c7 c8 c9

occupyWallStreet (Board _ _ _ (Just _) _ _ _ _ _) _ 4 = Left CellTaken
occupyWallStreet (Board c1 c2 c3 _ c5 c6 c7 c8 c9) p 4 =
    Right $ Board c1 c2 c3 (Just p) c5 c6 c7 c8 c9

occupyWallStreet (Board _ _ _ _ (Just _) _ _ _ _) _ 5 = Left CellTaken
occupyWallStreet (Board c1 c2 c3 c4 _ c6 c7 c8 c9) p 5 =
    Right $ Board c1 c2 c3 c4 (Just p) c6 c7 c8 c9

occupyWallStreet (Board _ _ _ _ _ (Just _) _ _ _) _ 6 = Left CellTaken
occupyWallStreet (Board c1 c2 c3 c4 c5 _ c7 c8 c9) p 6 =
    Right $ Board c1 c2 c3 c4 c5 (Just p) c7 c8 c9

occupyWallStreet (Board _ _ _ _ _ _ (Just _) _ _) _ 7 = Left CellTaken
occupyWallStreet (Board c1 c2 c3 c4 c5 c6 _ c8 c9) p 7 =
    Right $ Board c1 c2 c3 c4 c5 c6 (Just p) c8 c9

occupyWallStreet (Board _ _ _ _ _ _ _ (Just _) _) _ 8 = Left CellTaken
occupyWallStreet (Board c1 c2 c3 c4 c5 c6 c7 _ c9) p 8 =
    Right $ Board c1 c2 c3 c4 c5 c6 c7 (Just p) c9

occupyWallStreet (Board _ _ _ _ _ _ _ _ (Just _)) _ 9 = Left CellTaken
occupyWallStreet (Board c1 c2 c3 c4 c5 c6 c7 c8 _) p 9 =
    Right $ Board c1 c2 c3 c4 c5 c6 c7 c8 (Just p)

occupyWallStreet _ _ _ = Left InvalidCell

drawBoard :: Board -> String
drawBoard (Board c1 c2 c3 c4 c5 c6 c7 c8 c9) =
    drawRow c1 c2 c3 ++ "-+-+-\n" ++ drawRow c4 c5 c6 ++ "-+-+-\n" ++ drawRow c7 c8 c9

drawRow :: Cell -> Cell -> Cell -> String
drawRow c1 c2 c3 = drawCell c1 ++ "|" ++ drawCell c2 ++ "|" ++ drawCell c3 ++ "\n"

drawCell :: Cell -> String
drawCell = maybe " " show
