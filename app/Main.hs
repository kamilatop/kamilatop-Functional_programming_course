module Main where
  
import StackCommentChecker (checkStackComments, StackCommentMode(..), StackCommentError(..))
import BaseStructureFix (Operation(..), formatExecutionError, Stack)
import ControlFlow (doLoop)
import System.IO (putStrLn)

main :: IO ()
main = do
  -- Изначальное состояние стека
  let stack = [0, 0]

  -- Список операций
  let ops   = [OpInt 1, OpAdd]

  -- Проверка комментария перед выполнением
  let comment = "( a b -- c )"
  let mode = StrictMode

  case checkStackComments mode comment 2 1 of
    Left (InvalidSyntax msg) -> putStrLn $ "Ошибка синтаксиса комментария: " ++ msg
    Left (Mismatch expectedIn expectedOut actualIn actualOut) ->
      putStrLn $ "Несоответствие: ожидалось (" ++ show expectedIn ++ " -- " ++ show expectedOut 
               ++ "), но получено (" ++ show actualIn ++ " -- " ++ show actualOut ++ ")"
    Right _ -> do
      case doLoop 0 3 ops stack of
        Right resultStack -> putStrLn $ "Результат: " ++ show resultStack
        Left err          -> putStrLn $ "Ошибка: " ++ formatExecutionError err"
