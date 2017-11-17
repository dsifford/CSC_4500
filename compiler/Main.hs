import Data.Char
import System.IO

import Lib

debug = True

main :: IO ()
main = do
  file <- readFile "input.txt"
  let tokens = tokenize file
  let isValid = validate tokens
  let printValidity f =
        if f
          then putStrLn "File is valid"
          else putStrLn "File is invalid"
  if debug
    then do
      putStrLn $ unlines $ map show tokens
      printValidity isValid
    else printValidity isValid
