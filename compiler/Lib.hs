module Lib
  ( validate
  , tokenize
  ) where

import Data.Char (isAlphaNum, isLetter, isNumber)

{-# ANN module "HLint: ignore Use camelCase" #-}

newtype Identifier =
  Identifier String
  deriving (Show, Eq)

newtype IntLit =
  IntLit Int
  deriving (Show, Eq)

newtype Operator =
  Operator Char
  deriving (Show, Eq)

newtype Separator =
  Separator Char
  deriving (Show, Eq)

data Token
  = T_Ident Identifier
  | T_Int IntLit
  | T_Operator Operator
  | T_Sep Separator
  deriving (Show, Eq)

data State
  = Q0
  | Q1
  | Q2
  deriving (Show)

data Stack
  = Program
  | StmtList
  | Stmt
  | Assign
  | Expr
  deriving (Show)

identifier :: String -> Maybe (Identifier, String)
identifier s@(x:xs)
  | isLetter x || x == '_' =
    let ident = takeWhile isAlphaNum s
        remaining = drop (length ident) s
    in if length ident < 11
         then Just (Identifier ident, remaining)
         else Nothing
  | otherwise = Nothing
identifier [] = Nothing

intLit :: String -> Maybe (IntLit, String)
intLit s@(x:xs)
  | isNumber x =
    let int = takeWhile isNumber s
        remaining = drop (length int) s
    in if length int < 11
         then Just (IntLit (read int :: Int), remaining)
         else Nothing
  | otherwise = Nothing
intLit [] = Nothing

operator :: Char -> Maybe Operator
operator x =
  if x `elem` "+-*/="
    then Just (Operator x)
    else Nothing

separator :: Char -> Maybe Separator
separator x =
  if x `elem` "\n\r\v\f;"
    then Just (Separator x)
    else Nothing

-- Helper that takes a builder and an input string and returns whether
-- or not the input satisfies the builder's type
is :: (a -> Maybe b) -> a -> Bool
is f a =
  case f a of
    Just _  -> True
    Nothing -> False

tokenize :: String -> [Token]
tokenize tokens@(x:xs)
  | is identifier tokens =
    case identifier tokens of
      Just (ident, str) -> T_Ident ident : tokenize str
      Nothing           -> tokenize xs
  | is intLit tokens =
    case intLit tokens of
      Just (int, str) -> T_Int int : tokenize str
      Nothing         -> tokenize xs
  | is operator x =
    case operator x of
      Just o  -> T_Operator o : tokenize xs
      Nothing -> tokenize xs
  | is separator x =
    case separator x of
      Just s  -> T_Sep s : tokenize xs
      Nothing -> tokenize xs
  | otherwise = tokenize xs
tokenize [] = []

term :: [Token] -> Maybe ([Token], [Token])
term x =
  let t = term' x []
      remaining = drop (length t) x
  in if not (null t)
       then Just (t, remaining)
       else Nothing

term' :: [Token] -> [Token] -> [Token]
term' (a:b:xs) y =
  case (a, b) of
    (T_Ident _, T_Operator (Operator '*')) -> term' xs (y ++ [a, b])
    (T_Ident _, T_Operator (Operator '/')) -> term' xs (y ++ [a, b])
    (T_Int _, T_Operator (Operator '*'))   -> term' xs (y ++ [a, b])
    (T_Int _, T_Operator (Operator '/'))   -> term' xs (y ++ [a, b])
    (T_Ident _, _)                         -> y ++ [a]
    (T_Int _, _)                           -> y ++ [a]
    _                                      -> y
term' [T_Ident a] y = y ++ [T_Ident a]
term' [T_Int a] y = y ++ [T_Int a]

expression :: [Token] -> Maybe ([Token], [Token])
expression x =
  let expr = expression' x []
      remaining = drop (length expr) x
  in if not (null expr)
       then Just (expr, remaining)
       else Nothing

expression' :: [Token] -> [Token] -> [Token]
expression' x y =
  case term x of
    Just (t, remaining) ->
      case remaining of
        (T_Operator (Operator '+'):xs) ->
          expression' xs (y ++ t ++ [T_Operator (Operator '+')])
        (T_Operator (Operator '-'):xs) ->
          expression' xs (y ++ t ++ [T_Operator (Operator '-')])
        _ -> y ++ t
    Nothing -> y

validate :: [Token] -> Bool
validate x = v Q0 x []

v :: State -> [Token] -> [Stack] -> Bool
v Q0 input [] = v Q1 input [Program]
-- Program
v Q1 input@(x:xs) [Program] = v Q1 input [StmtList, Program]
v Q1 [] [Program] = v Q1 [] []
-- Statement List
v Q1 input@(x:xs) (StmtList:ys) = v Q1 input (Stmt : StmtList : ys)
v Q1 [] (StmtList:ys) = v Q1 [] ys
-- Statement
v Q1 (T_Sep x:xs) (Stmt:ys) = v Q1 xs ys
v Q1 input@(x:xs) (Stmt:ys) = v Q1 input (Assign : Stmt : ys)
-- Assignment
v Q1 (T_Ident x:T_Operator (Operator '='):xs) (Assign:ys) = v Q1 xs (Expr : ys)
-- Expression
v Q1 input (Expr:ys) =
  case expression input of
    Just (expr, remaining) -> v Q1 remaining ys
    Nothing                -> False
-- Transition
v Q1 [] [] = v Q2 [] []
-- Accepting State
v Q2 _ _ = True
-- Everything Else
v _ _ _ = False
