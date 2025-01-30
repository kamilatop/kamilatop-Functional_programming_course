module BaseStructureFix 
  ( Operation(..)   
  , ExecutionError(..)
  , ExecutionResult(..)
  , Stack
  , emptyStack
  , push
  , pop
  , formatExecutionError  
  ) where

import Control.Exception (Exception(..))

-- | Определение команд, которые можно интерпретировать:
data Operation
  = OpInt Int      -- ^ Положить целое число на стек
  | OpAdd          -- ^ Операция сложения
  | OpSub          -- ^ Операция вычитания
  | OpMul          -- ^ Операция умножения
  | OpDiv          -- ^ Операция деления
  | OpBeginUntil   -- ^ Цикл BEGIN ... UNTIL
  deriving (Show, Eq)

-- | Определение возможных ошибок исполнения:
data ExecutionError
  = StackUnderflow
  | DivisionByZero
  | UnknownOperation String
  deriving (Show, Eq)

instance Exception ExecutionError

-- | Определение типа стека
type Stack = [Int]

data ExecutionResult = ExecutionResult
  { stack  :: Stack
  , output :: String
  } deriving (Show, Eq)

-- | Функция для форматирования ошибок
formatExecutionError :: ExecutionError -> String
formatExecutionError StackUnderflow         = "Ошибка: Стек пуст"
formatExecutionError DivisionByZero         = "Ошибка: Деление на ноль"
formatExecutionError (UnknownOperation op)  = "Ошибка: Неизвестная операция: " ++ op

-- | Операции со стеком
emptyStack :: Stack
emptyStack = []

push :: Int -> Stack -> Stack
push x st = x : st

pop :: Stack -> Either ExecutionError (Int, Stack)
pop [] = Left StackUnderflow
pop (x:xs) = Right (x, xs)
