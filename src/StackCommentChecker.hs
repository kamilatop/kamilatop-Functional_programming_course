module StackCommentChecker (checkStackComments, StackCommentMode(..), StackCommentError(..)) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

-- | Определение режима проверки комментариев
data StackCommentMode = StrictMode | WarningMode deriving (Show, Eq)

-- | Определение ошибки проверки комментариев
data StackCommentError
  = InvalidSyntax String  -- Ошибка синтаксиса
  | Mismatch Int Int Int Int  -- Несоответствие (ожидаемый вход, ожидаемый выход, фактический вход, фактический выход)
  deriving (Show, Eq)

type Parser = Parsec Void String

-- | Парсер для комментариев вида ( a b -- c d )
stackCommentParser :: Parser ([String], [String])
stackCommentParser = do
  _ <- char '(' *> space
  inputs <- many (some alphaNumChar <* space)
  _ <- string "--" <* space
  outputs <- many (some alphaNumChar <* space)
  _ <- char ')'
  return (inputs, outputs)

-- | Функция проверки соответствия комментариев и реального стека.
checkStackComments :: StackCommentMode -> String -> Int -> Int -> Either StackCommentError ()
checkStackComments mode comment actualIn actualOut =
  case parse stackCommentParser "" comment of
    Left _ -> case mode of
      StrictMode  -> Left (InvalidSyntax "Некорректный синтаксис комментария стека")
      WarningMode -> Right ()  -- В режиме предупреждения просто игнорируем ошибку
    Right (expectedIn, expectedOut) ->
      let expectedInCount = length expectedIn
          expectedOutCount = length expectedOut
      in if expectedInCount == actualIn && expectedOutCount == actualOut
         then Right ()
         else case mode of
           StrictMode  -> Left (Mismatch expectedInCount expectedOutCount actualIn actualOut)
           WarningMode -> Right ()  -- Просто игнорируем несоответствие
