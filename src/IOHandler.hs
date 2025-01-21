-- Файл: src/IOHandler.hs
-- Описание:
-- Модуль, предоставляющий функции для ввода и вывода в контексте работы со стеком.

module IOHandler where

import BaseStructureFix (Stack, ExecutionError(..), pop, push)

-- | Функция для вывода одного символа.
emit :: Char -> Stack -> (Stack, String)
emit c st = (st, [c])  -- Возвращает стек без изменений и сам символ

-- | Функция для вывода верхнего элемента стека.
printTop :: Stack -> Either ExecutionError (Stack, String)
printTop (x:xs) = Right (xs, show x ++ " ")  -- Если стек не пуст, выводим верхний элемент
printTop _      = Left StackUnderflow  -- Если стек пуст, возвращаем ошибку

-- | Функция для ввода числа от пользователя.
inputNumber :: IO Int
inputNumber = do
  putStr "Введите число: "  -- Запрашиваем ввод
  readLn  -- Читаем введенное число и возвращаем его
