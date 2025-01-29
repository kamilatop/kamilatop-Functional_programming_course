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

-- | Цикл BEGIN ... UNTIL:
-- Повторяем выполнение операций до тех пор, пока условие не станет истинным.
beginUntil :: [Operation] -> Stack -> (Stack -> Bool) -> Either ExecutionError Stack
beginUntil ops stack condition = 
  let loop stack' = 
        if condition stack' 
          then Right stack'  -- Если условие истинно, возвращаем текущий стек
          else 
            case executeProgram ops stack' of
              Left err      -> Left err  -- Если ошибка при выполнении операций
              Right newSt   -> loop newSt  -- Иначе продолжаем до тех пор, пока условие не станет истинным
  in loop stack  -- Начинаем цикл с первоначального состояния стека

-- | Функция для выполнения программы (списка операций) на текущем стеке.
-- Функция выполняет все операции и возвращает новый стек, если операции выполнены без ошибок.
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
            Right (b, st2) -> executeProgram ops (push (b + a) st2)

    -- Операция: вычитание (b - a)
    OpSub -> 
      case pop st of
        Left err         -> Left err
        Right (a, st1) ->
          case pop st1 of
            Left err         -> Left err
            Right (b, st2) -> executeProgram ops (push (b - a) st2)

    -- Операция: умножение
    OpMul -> 
      case pop st of
        Left err         -> Left err
        Right (a, st1) ->
          case pop st1 of
            Left err         -> Left err
            Right (b, st2) -> executeProgram ops (push (b * a) st2)

    -- Операция: деление (b ÷ a), с проверкой деления на ноль
    OpDiv -> 
      case pop st of
        Left err         -> Left err
        Right (a, st1) ->
          case pop st1 of
            Left err         -> Left err
            Right (b, st2) -> 
              if a == 0 
                then Left DivisionByZero  -- Ошибка при делении на ноль
                else executeProgram ops (push (b `div` a) st2)
