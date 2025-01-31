module BaseStructureFix
  ( Operation(..)
  , ExecutionError(..)
  , Stack
  , ArrayStore
  , Dictionary
  , emptyStack
  , push
  , pop
  , formatExecutionError  
  , createArray
  , arrayStore
  , arrayFetch
  , arrayModify
  , executeLoop
  , executeMultiExitLoop
  , executeCase
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
  | OpColon String      
  | OpSemicolon         
  | OpCall String       
  | OpLeave             
  | OpLoop              
  | OpCase       
  | OpOf         
  | OpEndof      
  | OpEndcase    
  deriving (Show, Eq)

data ExecutionError
  = StackUnderflow
  | DivisionByZero
  | UnknownOperation String
  | ArrayIndexOutOfBounds
  | ArrayNotInitialized
  | WordNotDefined String  
  deriving (Show, Eq)

instance Exception ExecutionError

type Stack = [Int]
type ArrayStore = Map.Map Int [Int]
type Dictionary = Map.Map String [Operation]

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

formatExecutionError :: ExecutionError -> String
formatExecutionError StackUnderflow         = "Error: Stack is empty"
formatExecutionError DivisionByZero         = "Error: Division by zero"
formatExecutionError (UnknownOperation op)  = "Error: Unknown operation: " ++ op
formatExecutionError ArrayIndexOutOfBounds  = "Error: Array index out of bounds"
formatExecutionError ArrayNotInitialized    = "Error: Array not initialized"
formatExecutionError (WordNotDefined name)  = "Error: Word '" ++ name ++ "' not defined"

-- Multi-exit loop execution
executeMultiExitLoop :: Stack -> [Operation] -> Int -> Either ExecutionError Stack
executeMultiExitLoop st ops counter = do
  let newCounter = counter + 1
  if newCounter > 10
    then Right st
    else executeMultiExitLoop (push newCounter st) ops newCounter

-- Execute LEAVE
executeLeave :: Stack -> Either ExecutionError Stack
executeLeave [] = Left StackUnderflow
executeLeave st = Right st

-- Case execution
executeCase :: Stack -> [Operation] -> Either ExecutionError Stack
executeCase st (OpCase : rest) = executeCase st rest
executeCase st (OpOf : rest) = executeCase st rest
executeCase st (OpEndof : rest) = executeCase st rest
executeCase st (OpEndcase : rest) = Right st
executeCase st _ = Left (UnknownOperation "Unknown operation in CASE statement")
