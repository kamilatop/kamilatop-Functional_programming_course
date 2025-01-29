

# Основные модули

1. **BaseStructureFix.hs**  
   Этот модуль содержит базовые структуры данных, такие как:
   - Тип `Operation`, который определяет возможные операции (например, `OpInt`, `OpAdd`, `OpSub` и т.д.).
   - Тип `ExecutionError`, который описывает возможные ошибки выполнения (например, `StackUnderflow`, `DivisionByZero`).
   - Тип `Stack`, который представляет стек как список целых чисел.
   - Функции для работы со стеком (`push`, `pop`).

2. **ControlFlow.hs**  
   Этот модуль отвечает за управление потоком выполнения программы:
   - Функция `ifThenElse` для реализации условных операторов.
   - Функция `doLoop` для реализации циклов.
   - Функция `executeProgram`, которая выполняет список операций на стеке.

3. **Comparisons.hs**  
   Этот модуль содержит функции для выполнения операций сравнения (например, "=", "<", ">").

4. **IOHandler.hs**  
   Этот модуль предоставляет функции для ввода и вывода данных, такие как `emit` (вывод символа) и `printTop` (вывод верхнего элемента стека).

5. **Variables.hs**  
   Этот модуль реализует хранение и управление переменными с использованием монады `State`.

---

### Основные функции

- **push**: Добавляет элемент в стек.
- **pop**: Извлекает элемент из стека.
- **executeProgram**: Выполняет список операций на стеке.
- **ifThenElse**: Реализует условные операторы.
- **doLoop**: Реализует циклы.

---

# Тестирование

### Модуль TestSuite.hs

Модуль `TestSuite.hs` содержит тесты для проверки корректности работы основных функций проекта. Тесты написаны с использованием библиотеки `hspec`.

#### Примеры тестов:

1. **Операции со стеком**:
   - Проверка функций `push` и `pop`.
   ```haskell
   describe "Операции со стеком" $ do
     it "Работа push и pop" $ do
       pop (push 42 emptyStack) `shouldBe` Right (42, emptyStack)
    ```
2. **Операции сравнения**:
    - Проверка функции compareOp для операций "=", "<", ">".
```haskell
describe "Операции сравнения" $ do
  it "Проверка равенства" $ do
    compareOp "=" [2,2] `shouldBe` Right [-1]
    compareOp "=" [2,3] `shouldBe` Right [0]

  it "Проверка операции 'меньше'" $ do
    compareOp "<" [1,2] `shouldBe` Right [-1]
    compareOp "<" [2,1] `shouldBe` Right [0]

  it "Проверка операции 'больше'" $ do
    compareOp ">" [2,1] `shouldBe` Right [-1]
    compareOp ">" [1,2] `shouldBe` Right [0]
```
3. **Управление потоком:**:
    - Проверка функций ifThenElse и doLoop.
```haskell
describe "Управление потоком" $ do
  it "Ветвление ifThenElse" $ do
    ifThenElse [-1] [OpInt 1] [OpInt 2] `shouldBe` Right [1]
    ifThenElse [0]  [OpInt 1] [OpInt 2] `shouldBe` Right [2]

  it "Цикл DO I LOOP" $ do
    doLoop 0 3 [OpInt 1, OpAdd] [0,0] `shouldBe` Right [3,0]
```

4. **Операции ввода-вывода**:
    - Проверка функций emit и printTop.
```haskell
describe "Операции ввода-вывода" $ do
  it "Вывод символа" $ do
    emit 'A' [] `shouldBe` ([], "A")

  it "Вывод верхнего элемента стека" $ do
    printTop [10] `shouldBe` Right ([], "10 ")
```
Заключение по тестированию
Тесты охватывают основные функции проекта, такие как операции со стеком, сравнения, управление потоком и ввод-вывод. Это позволяет убедиться в корректности и надежности работы программы. Все тесты написаны с использованием библиотеки hspec, что обеспечивает удобство и читаемость тестового кода.


# Комбинирование монад

## Использование монад

В проекте активно используются монады для управления состоянием и обработки ошибок:

- **Монада Either**: Используется для обработки ошибок выполнения (например, `StackUnderflow`).
- **Монада State**: Используется в модуле `Variables.hs` для управления состоянием переменных.

### Пример использования монады State
```haskell
addVariable :: String -> Int -> State VariableStore ()
addVariable name value = modify ((name, value) :)
```
### Пример использования монады Either

```haskell
pop :: Stack -> Either ExecutionError (Int, Stack)
pop [] = Left StackUnderflow
pop (x:xs) = Right (x, xs)
```


# Парсинг

## Модуль `ParserEngine.hs`

Этот модуль отвечает за парсинг строк в операции. Используется библиотека `megaparsec` для обработки входных данных.

### Пример функции парсинга

```haskell
parseOperation :: String -> Either LexicalError Operation
parseOperation "+"  = Right OpAdd
parseOperation "-"  = Right OpSub
parseOperation "*"  = Right OpMul
parseOperation "/"  = Right OpDiv
parseOperation str
  | all (`elem` "0123456789") str = Right (OpInt (read str))
  | otherwise = Left (LexicalError ("Неизвестный токен: " ++ str))
```
## Модуль `CommentParser.hs`

Этот модуль отвечает за пропуск комментариев в коде. Комментарии обрабатываются в формате `(* ... *)`.
```
## Пример функции

```haskell
skipComment :: Parser ()
skipComment = do
  _ <- string "(*"
  _ <- manyTill anySingle (string "*)")
  return ()
```
## Используемые библиотеки для парсинга

- **megaparsec**: Мощная библиотека для парсинга текста.
- **text**: Для работы с текстовыми данными.

## Заключение по парсингу

Парсинг является важной частью проекта, позволяя преобразовывать входные данные в структуры, которые могут быть обработаны интерпретатором.


# Используемые библиотеки

В проекте используются следующие библиотеки:

- **base**: Базовая библиотека Haskell.
- **containers**: Для работы с коллекциями данных.
- **transformers**: Для работы с монадами.
- **megaparsec**: Для парсинга.
- **hspec**: Для написания тестов.
- **mtl**: Для работы с монадами и трансформерами.

### Зависимости проекта (build-depends)

```yaml
build-depends:
  base >=4.7 && <5,
  containers,
  transformers,
  megaparsec,
  hspec,
  mtl
```

# Результаты тестирования
Для запуска тестов используется команда `cabal test`. Пример вывода:

```bash
PS C:\Users\kamilatop\Desktop\Functional_Programming> cabal test
Build profile: -w ghc-9.10.1 -O1
In order, the following will be built (use -v for more details):
 - colon-0.1.0.0 (test:colon-test) (first run)
Preprocessing test suite 'colon-test' for colon-0.1.0.0...
Building test suite 'colon-test' for colon-0.1.0.0...
Running 1 test suites...
Test suite colon-test: RUNNING...

Операции со стеком
  Работа push и pop [v]
Операции сравнения
  Проверка равенства [v]
Управление потоком
  Ветвление ifThenElse [v]
  Цикл DO I LOOP [v]
Операции ввода-вывода
  Вывод символа [v]

Finished in 0.0069 seconds
5 examples, 0 failures
Test suite colon-test: PASS
Test suite logged to:
C:\Users\kamilatop\Desktop\Functional_Programming\dist-newstyle\build\x86_64-windows\ghc-9.10.1\colon-0.1.0.0\t\colon-test\test\colon-0.1.0.0-colon-test.log
1 of 1 test suites (1 of 1 test cases) passed.
PS C:\Users\kamilatop\Desktop\Functional_Programming>
```
## Результат выполнения программы
```bash
PS C:\Users\kamilatop\Desktop\Functional_Programming> cabal run
Build profile: -w ghc-9.10.1 -O1
In order, the following will be built (use -v for more details):
 - colon-0.1.0.0 (exe:colon-exe) (file C:\Users\kamilatop\Desktop\Functional_Programming\dist-newstyle\build\x86_64-windows\ghc-9.10.1\colon-0.1.0.0\cache\build changed)
Preprocessing executable 'colon-exe' for colon-0.1.0.0...
Building executable 'colon-exe' for colon-0.1.0.0...
[1 of 1] Compiling Main             ( app\Main.hs, C:\Users\kamilatop\Desktop\Functional_Programming\dist-newstyle\build\x86_64-windows\ghc-9.10.1\colon-0.1.0.0\x\colon-exe\build\colon-exe\colon-exe-tmp\Main.o ) [BaseStructureFix changed]
[2 of 2] Linking C:\\Users\\kamilatop\\Desktop\\Functional_Programming\\dist-newstyle\\build\\x86_64-windows\\ghc-9.10.1\\colon-0.1.0.0\\x\\colon-exe\\build\\colon-exe\\colon-exe.exe [Objects changed]
Результат: [3,0]
PS C:\Users\kamilatop\Desktop\Functional_Programming>
```

### Описание процесса

1. **Сборка проекта**:
   - Используется профиль сборки: `-w ghc-9.10.1 -O1`.
   - Собраны необходимые компоненты, включая исполняемый файл `colon-exe`.

2. **Компиляция**:
   - Компиляция модуля `Main` из файла `app\Main.hs`.
   - Создан объектный файл `Main.o`.

3. **Линковка**:
   - Исполняемый файл `colon-exe.exe` успешно создан.

4. **Результат выполнения**:
   - Программа завершила выполнение с результатом: `[3, 0]`.



### Цикл BEGIN ... UNTIL
