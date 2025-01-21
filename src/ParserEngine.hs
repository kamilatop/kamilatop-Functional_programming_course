-- Файл: src/ParserEngine.hs
-- Описание:
-- Модуль, который предоставляет функции для парсинга строк в операции.

module ParserEngine where

import BaseStructureFix
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

-- | Тип ошибки лексического анализа
newtype LexicalError = LexicalError String
  deriving (Show, Eq)

-- | Функция для преобразования строки в операцию
parseOperation :: String -> Either LexicalError Operation
parseOperation "+"  = Right OpAdd        -- Операция сложения
parseOperation "-"  = Right OpSub        -- Операция вычитания
parseOperation "*"  = Right OpMul        -- Операция умножения
parseOperation "/"  = Right OpDiv        -- Операция деления
parseOperation str
  | all (`elem` "0123456789") str = Right (OpInt (read str))  -- Если строка состоит только из цифр, создаем операцию с числом
  | otherwise = Left (LexicalError ("Неизвестный токен: " ++ str))  -- В противном случае, ошибка с нераспознанным токеном

-- | Функция для преобразования строки в список операций
parseProgram :: String -> Either LexicalError [Operation]
parseProgram = traverse parseOperation . words  -- Преобразуем каждое слово в операцию с помощью `traverse`
