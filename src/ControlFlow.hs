module ControlFlow where

import BaseStructureFix
import qualified Data.Map as Map

ifThenElse :: Stack -> [Operation] -> [Operation] -> Either ExecutionError Stack
ifThenElse (cond:restStack) thenBranch elseBranch =
  if cond /= 0
    then executeProgram thenBranch restStack
    else executeProgram elseBranch restStack
ifThenElse _ _ _ = Left StackUnderflow

doLoop :: Int -> Int -> [Operation] -> Stack -> Either ExecutionError Stack
doLoop start end ops st
  | start >= end = Right st
  | otherwise = 
      case executeProgram ops st of
        Left err      -> Left err
        Right newSt   -> doLoop (start + 1) end ops newSt

beginUntil :: [Operation] -> Stack -> (Stack -> Bool) -> Either ExecutionError Stack
beginUntil ops stack condition = 
  let loop stack' = 
        if condition stack' 
          then Right stack'
          else 
            case executeProgram ops stack' of
              Left err      -> Left err
              Right newSt   -> loop newSt
  in loop stack

executeProgram :: [Operation] -> Stack -> Either ExecutionError Stack
executeProgram [] st = Right st
executeProgram (op:ops) st = 
  case op of
    OpInt n -> executeProgram ops (push n st)
    OpAdd -> 
      case pop st of
        Left err         -> Left err
        Right (a, st1) ->
          case pop st1 of
            Left err         -> Left err
            Right (b, st2) -> executeProgram ops (push (b + a) st2)
    OpSub -> 
      case pop st of
        Left err         -> Left err
        Right (a, st1) ->
          case pop st1 of
            Left err         -> Left err
            Right (b, st2) -> executeProgram ops (push (b - a) st2)
    OpMul -> 
      case pop st of
        Left err         -> Left err
        Right (a, st1) ->
          case pop st1 of
            Left err         -> Left err
            Right (b, st2) -> executeProgram ops (push (b * a) st2)
    OpDiv -> 
      case pop st of
        Left err         -> Left err
        Right (a, st1) ->
          case pop st1 of
            Left err         -> Left err
            Right (b, st2) -> 
              if a == 0 
                then Left DivisionByZero
                else executeProgram ops (push (b `div` a) st2)
    OpCreateArray size -> 
      case createArray size Map.empty of
        Left err -> Left err
        Right arrays -> executeProgram ops st
    OpArrayStore -> 
      case pop st of
        Left err -> Left err
        Right (value, st1) ->
          case pop st1 of
            Left err -> Left err
            Right (elementIndex, st2) ->
              case pop st2 of
                Left err -> Left err
                Right (arrayIndex, st3) ->
                  case arrayStore arrayIndex elementIndex value Map.empty of
                    Left err -> Left err
                    Right arrays -> executeProgram ops st3
    OpArrayFetch -> 
      case pop st of
        Left err -> Left err
        Right (elementIndex, st1) ->
          case pop st1 of
            Left err -> Left err
            Right (arrayIndex, st2) ->
              case arrayFetch arrayIndex elementIndex Map.empty of
                Left err -> Left err
                Right value -> executeProgram ops (push value st2)
    OpArrayModify -> 
      case pop st of
        Left err -> Left err
        Right (elementIndex, st1) ->
          case pop st1 of
            Left err -> Left err
            Right (arrayIndex, st2) ->
              case arrayModify arrayIndex elementIndex (+1) Map.empty of
                Left err -> Left err
                Right arrays -> executeProgram ops st2
    _ -> Left (UnknownOperation "Unsupported operation")