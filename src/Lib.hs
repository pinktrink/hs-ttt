{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Lib where

import Control.Applicative
import Data.Map as Map
import Data.Maybe
import Data.List.Split
import Data.List


data Token = X | O deriving (Show, Eq)


data Cell
    = C1 | C2 | C3
    | C4 | C5 | C6
    | C7 | C8 | C9
    deriving (Eq, Ord, Enum, Bounded)


type Board = Map Cell Token


initTTT :: IO ()
initTTT = do
    putStrLn $ drawBoard mempty
    ticTacToe mempty X


parseCell :: String -> Maybe Cell
parseCell = \case
   "1" -> Just C1; "2" -> Just C2; "3" -> Just C3
   "4" -> Just C4; "5" -> Just C5; "6" -> Just C6
   "7" -> Just C7; "8" -> Just C8; "9" -> Just C9
   _ -> Nothing


ticTacToe :: Board -> Token -> IO ()
ticTacToe b p = do
    putStrLn $ "You are " ++ show p ++ ". Please input cell bitch."
    cell <- parseCell <$> getLine
    maybe (error "must be a number between 0 and 9 bitch") tick cell
  where
    printBoard = putStrLn . drawBoard

    tick x = case Map.lookup x b of
        Just _ -> error "Cell Taken"
        _ -> let b' = Map.insert x p b
             in maybe (next b') (winner b') $ checkBoard b'

    next b' = do
        printBoard b'
        ticTacToe b' (case p of X -> O; O -> X)

    winner b' t = do
        printBoard b'
        putStrLn $ show t ++ " wins bitch."

    error msg = do
        putStrLn msg
        printBoard b
        ticTacToe b p


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
