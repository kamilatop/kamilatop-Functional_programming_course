module Main where

import Test.Hspec
import BaseStructureFix
import Comparisons
import ControlFlow  
import IOHandler (emit, printTop)
import StackCommentChecker (checkStackComments, StackCommentMode(..), StackCommentError(..))
import qualified Data.Either as E

main :: IO ()
main = hspec $ do
  describe "Операции со стеком" $ do
    it "Работа push и pop" $ do
      pop (push 42 emptyStack) `shouldBe` Right (42, emptyStack)

  describe "Операции сравнения" $ do
    it "Проверка равенства" $ do
      compareOp "=" [2,2] `shouldBe` Right [-1]
      compareOp "=" [2,3] `shouldBe` Right [0]

  describe "Управление потоком" $ do
    it "Ветвление ifThenElse" $ do
      ifThenElse [-1] [OpInt 1] [OpInt 2] `shouldBe` Right [1]
      ifThenElse [0]  [OpInt 1] [OpInt 2] `shouldBe` Right [2]

    it "Цикл DO I LOOP" $ do
      doLoop 0 3 [OpInt 1, OpAdd] [0,0] `shouldBe` Right [3,0]

    it "Цикл BEGIN ... UNTIL" $ do
      let condition stack = case stack of
                             [] -> False
                             (x:_) -> x == 10
      beginUntil [OpInt 1, OpAdd] [0] condition `shouldBe` Right [10]  

  describe "Операции ввода-вывода" $ do
    it "Вывод символа" $ do
      emit 'A' [] `shouldBe` ([], "A")
      printTop [10] `shouldBe` Right ([], "10 ")

  describe "Проверка комментариев про стек" $ do
    it "Корректный комментарий (2 входа, 1 выход) - режим StrictMode" $ do
      checkStackComments StrictMode "( a b -- c )" 2 1 `shouldBe` Right ()
    
    it "Несоответствие (ожидалось 2 -- 1, но получено 1 -- 1) - режим StrictMode" $ do
      checkStackComments StrictMode "( a b -- c )" 1 1 `shouldBe` Left (Mismatch 2 1 1 1)

    it "Неверный синтаксис - режим StrictMode" $ do
      checkStackComments StrictMode "( a b c d )" 2 1 `shouldBe` Left (InvalidSyntax "Некорректный синтаксис комментария стека")

    it "Несоответствие (ожидалось 3 -- 1, но получено 2 -- 1) - режим WarningMode (должно проходить)" $ do
      checkStackComments WarningMode "( a b c -- d )" 2 1 `shouldBe` Right ()
    
    it "Неверный синтаксис - режим WarningMode (должно проходить без ошибки)" $ do
      checkStackComments WarningMode "( a b c d )" 2 1 `shouldBe` Right ()

  describe "Операции с массивами" $ do
    it "Создание массива" $ do
      executeProgram [OpCreateArray 10] emptyStack `shouldBe` Right []

    it "Запись и чтение из массива" $ do
      let program = [OpCreateArray 10, OpInt 3, OpInt 42, OpArrayStore, OpInt 3, OpArrayFetch]
      executeProgram program emptyStack `shouldBe` Right [42]