{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( colorizeInput
    ) where

import System.IO


-- color definitions
red = "\x1b[31m"
green = "\x1b[32m"
yellow = "\x1b[33m"
blue = "\x1b[34m"
magenta = "\x1b[35m"
cyan = "\x1b[36m"
reset = "\x1b[0m"


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
    prefix (x:xs) (y:ys)
      | x == y = prefix xs ys
      | otherwise = False


-- If the supplied String argument matches one of the guard arms,
-- then we prefix the input with an ANSI color code of our choice,
-- and suffix it with the reset ANSI code.
match :: String -> String
match xs
  | substring " D " xs = green  ++ xs ++ reset
  | substring " I " xs = blue   ++ xs ++ reset
  | substring " E " xs = yellow ++ xs ++ reset
  | substring " W " xs = red    ++ xs ++ reset
  | otherwise = xs


-- Read lines from stdin, and if we encounter a substring match
-- with a substring of our choice, then we colorize the input line
-- using ANSI color codes before writing it to stdout.
colorizeInput :: IO ()
colorizeInput = do
  -- Set utf8 encoding for stdout and stdin; otherwise,
  -- we'll get exceptions on utf8 characters.
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  -- split stdin from interact into [String], apply
  -- match to each line, and then join the lines.
  interact (unlines . map match . lines)
