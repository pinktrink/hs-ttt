module Lib where

import Safe

data Token = X | O deriving (Show, Eq)

type Cell = Maybe Token

data Board = Board Cell Cell Cell Cell Cell Cell Cell Cell Cell

data Exception = CellTaken | InvalidCell deriving Show

emptyBoard :: Board
emptyBoard = Board Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

initTTT :: IO ()
initTTT = do
    putStrLn $ drawBoard emptyBoard
    ticTacToe emptyBoard X

negateToken :: Token -> Token
negateToken X = O
negateToken _ = X

ticTacToe :: Board -> Token -> IO ()
ticTacToe b p = do
    putStrLn $ "You are " ++ show p ++ ". Please input cell bitch."
    input <- readMay <$> getLine
    case input of
        Just x -> either
            (\e -> do
                print e
                putStrLn (drawBoard b)
                ticTacToe b p
            )
            (\b' -> case checkBoard b' of
                    Just t -> do
                        putStrLn $ drawBoard b'
                        putStrLn $ show t ++ " wins bitch."
                    _ -> do
                        putStrLn $ drawBoard b'
                        ticTacToe b' $ negateToken p
            )
            (occupyWallStreet b p x)
        _ -> do
            putStrLn "Must be a number between 1 and 9 bitch."
            putStrLn $ drawBoard b
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
