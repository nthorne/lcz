{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import System.IO


-- Returns true if the first String argument is a substring of the
-- second String argument.
substring :: String -> String -> Bool
substring xs [] = False
substring xs ys
  | prefix xs ys = True
  | substring xs (tail ys) = True
  | otherwise = False
  where
    prefix [] ys = True
    prefix (x:xs) [] = False
    prefix (x:xs) (y:ys) = (x == y) && prefix xs ys


-- TODO: Add proper patterns and formatting here
match :: String -> String
match xs
  | substring " E " xs = "ERROR " ++ xs
  | substring "AND" xs = "!!! " ++ xs
  | otherwise = xs


someFunc :: IO ()
someFunc = do
  -- Set utf8 encoding for stdout and stdin; otherwise,
  -- we'll get exceptions on utf8 characters.
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  -- split stdin from interact into [String], apply
  -- match to each line, and then join the lines.
  interact (unlines . map match . lines)
