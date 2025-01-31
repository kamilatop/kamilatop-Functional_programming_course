module ParserEngine where

import BaseStructureFix
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

newtype LexicalError = LexicalError String
  deriving (Show, Eq)

parseOperation :: String -> Either LexicalError Operation
parseOperation "+"  = Right OpAdd
parseOperation "-"  = Right OpSub
parseOperation "*"  = Right OpMul
parseOperation "/"  = Right OpDiv
parseOperation ":"  = Right (OpColon "")
parseOperation ";"  = Right OpSemicolon
parseOperation str
  | all (`elem` "0123456789") str = Right (OpInt (read str))
  | otherwise = Left (LexicalError ("Неизвестный токен: " ++ str))

parseProgram :: String -> Either LexicalError [Operation]
parseProgram = traverse parseOperation . words