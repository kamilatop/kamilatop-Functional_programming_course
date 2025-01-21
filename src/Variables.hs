module Variables where

import Control.Monad.State

-- | Тип для хранения переменных (имя/значение)
type VariableStore = [(String, Int)]

-- | Добавить переменную (имя и значение) в хранилище
addVariable :: String -> Int -> State VariableStore ()
addVariable name value = modify ((name, value) :)

-- | Получить значение переменной
getVariable :: String -> State VariableStore (Maybe Int)
getVariable name = do
  store <- get
  return (lookup name store)
