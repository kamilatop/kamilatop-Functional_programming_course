module BaseStructureFix 
  ( Operation(..)   
  , ExecutionError(..)
  , ExecutionResult(..)
  , Stack
  , ArrayStore
  , emptyStack
  , push
  , pop
  , formatExecutionError  
  , createArray
  , arrayStore
  , arrayFetch
  , arrayModify
  ) where

import Control.Exception (Exception(..))
import qualified Data.Map as Map

data Operation
  = OpInt Int      
  | OpAdd          
  | OpSub          
  | OpMul          
  | OpDiv          
  | OpBeginUntil   
  | OpCreateArray Int  
  | OpArrayStore       
  | OpArrayFetch       
  | OpArrayModify      
  deriving (Show, Eq)

data ExecutionError
  = StackUnderflow
  | DivisionByZero
  | UnknownOperation String
  | ArrayIndexOutOfBounds
  | ArrayNotInitialized
  deriving (Show, Eq)

instance Exception ExecutionError

type Stack = [Int]
type ArrayStore = Map.Map Int [Int]

data ExecutionResult = ExecutionResult
  { stack  :: Stack
  , arrays :: ArrayStore
  , output :: String
  } deriving (Show, Eq)

formatExecutionError :: ExecutionError -> String
formatExecutionError StackUnderflow         = "Ошибка: Стек пуст"
formatExecutionError DivisionByZero         = "Ошибка: Деление на ноль"
formatExecutionError (UnknownOperation op)  = "Ошибка: Неизвестная операция: " ++ op
formatExecutionError ArrayIndexOutOfBounds  = "Ошибка: Выход за границы массива"
formatExecutionError ArrayNotInitialized    = "Ошибка: Массив не инициализирован"

emptyStack :: Stack
emptyStack = []

push :: Int -> Stack -> Stack
push x st = x : st

pop :: Stack -> Either ExecutionError (Int, Stack)
pop [] = Left StackUnderflow
pop (x:xs) = Right (x, xs)

createArray :: Int -> ArrayStore -> Either ExecutionError ArrayStore
createArray size arrays
  | size <= 0 = Left ArrayIndexOutOfBounds
  | otherwise = Right $ Map.insert (Map.size arrays) (replicate size 0) arrays

arrayStore :: Int -> Int -> Int -> ArrayStore -> Either ExecutionError ArrayStore
arrayStore arrayIndex elementIndex value arrays =
  case Map.lookup arrayIndex arrays of
    Nothing -> Left ArrayNotInitialized
    Just arr ->
      if elementIndex < 0 || elementIndex >= length arr
        then Left ArrayIndexOutOfBounds
        else Right $ Map.insert arrayIndex (take elementIndex arr ++ [value] ++ drop (elementIndex + 1) arr) arrays

arrayFetch :: Int -> Int -> ArrayStore -> Either ExecutionError Int
arrayFetch arrayIndex elementIndex arrays =
  case Map.lookup arrayIndex arrays of
    Nothing -> Left ArrayNotInitialized
    Just arr ->
      if elementIndex < 0 || elementIndex >= length arr
        then Left ArrayIndexOutOfBounds
        else Right $ arr !! elementIndex

arrayModify :: Int -> Int -> (Int -> Int) -> ArrayStore -> Either ExecutionError ArrayStore
arrayModify arrayIndex elementIndex f arrays =
  case Map.lookup arrayIndex arrays of
    Nothing -> Left ArrayNotInitialized
    Just arr ->
      if elementIndex < 0 || elementIndex >= length arr
        then Left ArrayIndexOutOfBounds
        else Right $ Map.insert arrayIndex (take elementIndex arr ++ [f (arr !! elementIndex)] ++ drop (elementIndex + 1) arr) arrays