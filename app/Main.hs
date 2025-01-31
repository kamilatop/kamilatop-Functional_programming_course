module Main where
  
import BaseStructureFix (Operation(..), Stack, formatExecutionError)
import ControlFlow (executeProgram)
import qualified Data.Map as Map
import System.IO (putStrLn)

main :: IO ()
main = do
  let initialDict = Map.empty
  let program = 
        [ OpColon "double"
        , OpInt 2
        , OpMul
        , OpSemicolon
        , OpInt 5
        , OpCall "double"
        ]
  
  case executeProgram initialDict program [] of
    Left err -> putStrLn $ "Ошибка: " ++ formatExecutionError err
    Right (resultStack, _) -> putStrLn $ "Результат: " ++ show resultStack