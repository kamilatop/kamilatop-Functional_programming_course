-- Файл: app/Main.hs
-- Описание:
-- Пример исполняемой программы на языке Haskell, в которой
-- определяется начальное состояние стека, список операций,
-- а затем вызывается функция doLoop для повторного выполнения операций.

module Main where

import BaseStructureFix (Operation(..), formatExecutionError, Stack)
import ControlFlow (doLoop)
import System.IO (putStrLn)

main :: IO ()
main = do
  -- Изначальное состояние стека: содержит два числа (0 и 0).
  let stack = [0, 0]

  -- Список операций, которые будут выполняться:
  -- 1) Поместить число 1 на стек (OpInt 1)
  -- 2) Выполнить сложение верхних двух чисел (OpAdd)
  let ops   = [OpInt 1, OpAdd]

  -- Запускаем цикл doLoop, который выполнит "ops" три раза,
  -- начиная со значения 0 и заканчивая 3 (не включая 3).
  case doLoop 0 3 ops stack of
    -- Если ошибок при выполнении не произошло, 
    -- выводим итоговое состояние стека:
    Right resultStack -> putStrLn $ "Результат: " ++ show resultStack
    -- Если во время выполнения произошла ошибка (например, пустой стек),
    -- то выводим сообщение об ошибке:
    Left err          -> putStrLn $ "Ошибка: " ++ formatExecutionError err
