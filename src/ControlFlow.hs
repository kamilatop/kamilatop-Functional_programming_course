-- Файл: src/ControlFlow.hs
-- Описание:
-- Модуль, содержащий функции для выполнения условных операторов (if-then-else) и циклов (do-while)
-- с использованием стека и операций.

module ControlFlow where

import BaseStructureFix

-- | Условный оператор IF-THEN-ELSE:
-- Если верхний элемент стека не равен 0, выполняем блок thenBranch, иначе elseBranch.
ifThenElse :: Stack -> [Operation] -> [Operation] -> Either ExecutionError Stack
ifThenElse (cond:restStack) thenBranch elseBranch =
  if cond /= 0
    then executeProgram thenBranch restStack  -- Если условие истинно, выполняем thenBranch
    else executeProgram elseBranch restStack   -- Если условие ложно, выполняем elseBranch
ifThenElse _ _ _ = Left StackUnderflow  -- Если стек пуст, возвращаем ошибку

-- | Цикл DO I LOOP:
-- Повторяем выполнение операций с увеличением переменной от start до end.
doLoop :: Int -> Int -> [Operation] -> Stack -> Either ExecutionError Stack
doLoop start end ops st
  | start >= end = Right st  -- Если достигнут предел, возвращаем текущий стек
  | otherwise = 
      case executeProgram ops st of
        Left err      -> Left err  -- Если возникла ошибка в процессе выполнения, возвращаем ее
        Right newSt   -> doLoop (start + 1) end ops newSt  -- Иначе повторяем с обновленным стеком

-- | Функция для выполнения программы (списка операций) на текущем стеке.
executeProgram :: [Operation] -> Stack -> Either ExecutionError Stack
executeProgram [] st = Right st  -- Если список операций пуст, возвращаем текущий стек
executeProgram (op:ops) st = 
  case op of
    -- Операция: положить целое число на стек
    OpInt n -> executeProgram ops (push n st)

    -- Операция: сложение
    OpAdd -> 
      case pop st of
        Left err         -> Left err  -- Ошибка, если стек пуст
        Right (a, st1) ->
          case pop st1 of
            Left err         -> Left err  -- Ошибка, если стек пуст после первого pop
            Right (b, st2) -> executeProgram ops (push (b + a) st2)  -- Сложение и продолжение

    -- Операция: вычитание (b - a)
    OpSub ->
      case pop st of
        Left err         -> Left err
        Right (a, st1) ->
          case pop st1 of
            Left err         -> Left err
            Right (b, st2) -> executeProgram ops (push (b - a) st2)  -- Вычитание и продолжение

    -- Операция: умножение
    OpMul ->
      case pop st of
        Left err         -> Left err
        Right (a, st1) ->
          case pop st1 of
            Left err         -> Left err
            Right (b, st2) -> executeProgram ops (push (b * a) st2)  -- Умножение и продолжение

    -- Операция: деление (b ÷ a), с проверкой деления на ноль
    OpDiv ->
      case pop st of
        Left err         -> Left err
        Right (a, st1) ->
          case pop st1 of
            Left err         -> Left err
            Right (b, st2) ->
              if a == 0
                then Left DivisionByZero  -- Ошибка, если деление на ноль
                else executeProgram ops (push (b `div` a) st2)  -- Деление и продолжение
