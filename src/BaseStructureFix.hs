-- Файл: src/BaseStructureFix.hs
-- Описание:
-- Базовый модуль, в котором определяются:
-- 1) Тип команд (Operation),
-- 2) Тип ошибок исполнения (ExecutionError),
-- 3) Тип результата выполнения (ExecutionResult),
-- 4) Определение стека (Stack) и основные операции с ним (push/pop).

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
  = StackUnderflow             -- ^ При попытке выполнить pop из пустого стека
  | DivisionByZero             -- ^ При попытке деления на ноль
  | UnknownOperation String    -- ^ При наличии неизвестной операции
  deriving (Show, Eq)

-- | Делаем ошибку экземпляром класса Exception
instance Exception ExecutionError

-- | Функция для форматирования ошибок и отображения их пользователю
formatExecutionError :: ExecutionError -> String
formatExecutionError StackUnderflow         = "Ошибка: Стек пуст"
formatExecutionError DivisionByZero         = "Ошибка: Деление на ноль"
formatExecutionError (UnknownOperation op)  = "Ошибка: Неизвестная операция: " ++ op

-- | Определение типа стека как списка целых чисел
type Stack = [Int]

-- | Результат выполнения программы: 
--   здесь можно хранить конечный стек и выходные данные (строку).
data ExecutionResult = ExecutionResult
  { stack  :: Stack
  , output :: String
  } deriving (Show, Eq)

-- | Пустой стек
emptyStack :: Stack
emptyStack = []

-- | Положить элемент в стек (push)
push :: Int -> Stack -> Stack
push x st = x : st

-- | Извлечь элемент из стека (pop)
pop :: Stack -> Either ExecutionError (Int, Stack)
pop [] = Left StackUnderflow
pop (x:xs) = Right (x, xs)
