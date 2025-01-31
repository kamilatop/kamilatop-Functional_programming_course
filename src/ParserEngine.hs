module ParserEngine where

import BaseStructureFix
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

type Parser = Parsec Void String

parseOperation :: String -> Either LexicalError Operation
parseOperation "LEAVE" = Right OpLeave
parseOperation "+LOOP" = Right OpLoop
parseOperation "CASE" = Right OpCase
parseOperation "OF" = Right OpOf
parseOperation "ENDOF" = Right OpEndof
parseOperation "ENDCASE" = Right OpEndcase
parseOperation "+"  = Right OpAdd
parseOperation "-"  = Right OpSub
parseOperation "*"  = Right OpMul
parseOperation "/"  = Right OpDiv
parseOperation ":"  = Right (OpColon "")
parseOperation ";"  = Right OpSemicolon
parseOperation str
  | all (`elem` "0123456789") str = Right (OpInt (read str))
  | otherwise = Left (LexicalError ("Unknown token: " ++ str))

parseProgram :: String -> Either LexicalError [Operation]
parseProgram = traverse parseOperation . words
