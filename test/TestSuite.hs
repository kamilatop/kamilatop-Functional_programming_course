module Main where

import Test.Hspec
import BaseStructureFix
import Comparisons
import ControlFlow
import IOHandler (emit, printTop)
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

  describe "Операции ввода-вывода" $ do
    it "Вывод символа" $ do
      emit 'A' [] `shouldBe` ([], "A")
      printTop [10] `shouldBe` Right ([], "10 ")
