{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}


module Lib where

import Control.Applicative
import Data.Map as Map
import Data.List.Split
import Data.List


data Token = X | O deriving (Show, Eq)


data Cell
    = C1 | C2 | C3
    | C4 | C5 | C6
    | C7 | C8 | C9
    deriving (Eq, Ord, Enum, Bounded)


type Board = Map Cell Token


data Input = Move Cell | Undo


initTTT :: IO ()
initTTT = do
    putStrLn $ drawBoard mempty
    ticTacToe [mempty] X


parseInput :: String -> Maybe Input
parseInput = \case
   "1" -> Just $ Move C1; "2" -> Just $ Move C2; "3" -> Just $ Move C3
   "4" -> Just $ Move C4; "5" -> Just $ Move C5; "6" -> Just $ Move C6
   "7" -> Just $ Move C7; "8" -> Just $ Move C8; "9" -> Just $ Move C9
   "u" -> Just Undo; "undo" -> Just Undo
   _ -> Nothing


ticTacToe :: [Board] -> Token -> IO ()
ticTacToe bs p = do
    putStrLn $ "You are " ++ show p ++ ". Please input cell bitch."
    input <- parseInput <$> getLine
    maybe (error' "must be a number between 0 and 9 bitch") tick input
  where
    printBoard = putStrLn . drawBoard

    tick = \case
        Move x -> case Map.lookup x (head bs) of
            Just _ -> error' "Cell Taken"
            _ -> let b' = Map.insert x p $ head bs
                in maybe (next b') (winner b') $ checkBoard b'
        Undo | length bs == 1 -> do
            putStrLn "Cannot undo on empty board"
            ticTacToe bs p
        _ -> do
            printBoard $ head $ tail bs
            ticTacToe (tail bs) negated

    next b' = do
        printBoard b'
        ticTacToe (b':bs) negated

    winner b' t = do
        printBoard b'
        putStrLn $ show t ++ " wins bitch."

    error' msg = do
        putStrLn msg
        printBoard $ head bs
        ticTacToe bs p

    negated = case p of X -> O; O -> X


checkBoard :: Board -> Maybe Token
checkBoard b =
        check C1 C2 C3
    <|> check C4 C5 C6
    <|> check C7 C8 C9
    <|> check C1 C4 C7
    <|> check C2 C5 C8
    <|> check C3 C6 C9
    <|> check C1 C5 C9
    <|> check C3 C5 C7
  where
    check x y z = do
        x' <- Map.lookup x b
        y' <- Map.lookup y b
        z' <- Map.lookup z b
        if x' == y' && x' == z'
        then Just x' else Nothing


drawBoard :: Board -> String
drawBoard b = intercalate "-+-+-\n"
    . fmap ((++ "\n") . intercalate "|") . chunksOf 3
    $ maybe " " show . flip Map.lookup b <$> [C1 ..]
