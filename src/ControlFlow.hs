module ControlFlow where

import BaseStructureFix
import qualified Data.Map as Map

executeProgram :: Dictionary -> [Operation] -> Stack -> Either ExecutionError (Stack, Dictionary)
executeProgram dict [] st = Right (st, dict)
executeProgram dict (op:ops) st = 
  case op of
    OpColon name -> 
      let (body, remainingOps) = span (/= OpSemicolon) ops
      in executeProgram (Map.insert name body dict) remainingOps st
    OpSemicolon ->
      Right (st, dict)
    OpCall name ->
      case Map.lookup name dict of
        Nothing -> Left (WordNotDefined name)
        Just wordOps -> do
          (newSt, newDict) <- executeProgram dict wordOps st
          executeProgram newDict ops newSt
    _ -> 
      case executeOperation op st of
        Left err -> Left err
        Right newSt -> executeProgram dict ops newSt

executeOperation :: Operation -> Stack -> Either ExecutionError Stack
executeOperation (OpInt n) st = Right (push n st)
executeOperation OpAdd st = 
  case pop st of
    Left err -> Left err
    Right (a, st1) ->
      case pop st1 of
        Left err -> Left err
        Right (b, st2) -> Right (push (b + a) st2)
executeOperation OpSub st = 
  case pop st of
    Left err -> Left err
    Right (a, st1) ->
      case pop st1 of
        Left err -> Left err
        Right (b, st2) -> Right (push (b - a) st2)
executeOperation OpMul st = 
  case pop st of
    Left err -> Left err
    Right (a, st1) ->
      case pop st1 of
        Left err -> Left err
        Right (b, st2) -> Right (push (b * a) st2)
executeOperation OpDiv st = 
  case pop st of
    Left err -> Left err
    Right (a, st1) ->
      case pop st1 of
        Left err -> Left err
        Right (b, st2) -> 
          if a == 0 
            then Left DivisionByZero
            else Right (push (b `div` a) st2)
executeOperation _ st = Left (UnknownOperation "Unsupported operation")