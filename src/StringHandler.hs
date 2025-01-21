module StringHandler where

import BaseStructureFix
import Data.List (isPrefixOf, isSuffixOf)

-- | Печать строки как есть
printString :: String -> Stack -> (Stack, String)
printString str st = (st, str)

-- | Пример парсинга строки, поддерживающей синтаксис: ." ... "
parseString :: String -> Either String String
parseString input =
  if ".\"" `isPrefixOf` input && "\"" `isSuffixOf` input
    then Right (init (drop 2 input)) -- Удаляем ." с начала и " с конца
    else Left "Неверный формат строки"
