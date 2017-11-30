import System.IO

-- Парсер математических выражений
-- Распознает бинарные операторы (+, -, *, /) и скобки
-- Выражение читается из файла "file.txt"
-- Выражение должно быть записано в одну строку

-- функция считывания строки из файла
--hReadLine h = read <$> hGetLine h

getStringFromFile :: IO String
getStringFromFile = readFile "file.txt"


-- проверяет, является ли символ оператором. 
-- возвращает True если это так, в противном случае False
isOperators :: (Eq a) => a -> [a] -> Bool
isOperators _ [] = False
isOperators y (x:xs) =
    if (y == x) then
        True
    else
        isOperators y xs


-- возвращает приоритет оператора, типа Int
getPriority :: [(Char, Int)] -> Char -> Int
getPriority [x] _ = snd x
getPriority (x:xs) op =
    if (op == fst x) then
        snd x
    else
        getPriority xs op


-- добавить элемент в конец стэк.
-- стек представлен списком символов или чисел типа Double
push :: [a] -> a -> [a]
push s x = s ++ [x]


-- yдалить последний элемент из стэка.
pup :: [a] -> [a]
pup [] = []
pup x = init x


-- добавление символа в конец строки
append :: Char -> [Char] -> [Char]
append c s = s ++ [c]


-- возвращает и удаляет два элемента из стэка
two_last_el :: [a] -> (a, a, [a])
two_last_el stack = (last stack, last (init stack), init (init stack))


-- проверка на закрывающую скобку
isClosingParenthesis :: Char -> Bool
isClosingParenthesis x = 
    if (x == ')') then
        True
    else
        False


-- выталкивает из стэка операторы с большим приоритетом, чем у текущего
-- или записывает его в стэк
operatorCheck :: Char -> [Char] -> [Char] -> ([Char], [Char])
operatorCheck op [] strRPE = ((push [] op), strRPE)
operatorCheck op stack strRPE = do
    if (((getPriority priorities (last stack)) > (getPriority priorities op)) && (length stack > 0) && (getPriority priorities op /= 0)) then do
        operatorCheck op (init stack) (append (last stack) strRPE)
    else
        ((push stack op), strRPE)


-- набор операторов и приоритетов
operators = ['+', '-', '*', '/', '(']
priorities = [('+', 1), ('-', 2), ('/', 3), ('*', 4), ('(', 0)]


-- выталкивание операторов если встретилась закрывающая скобка
-- и удаление открывающей скобки из стека
-- возвращает тапл из двух символьных списков (строк)
ejection :: [Char] -> [Char] -> ([Char], [Char])
ejection [] strRPE = ([], strRPE)
ejection stack strRPE = 
    if (getPriority priorities (last stack) /= 0) then
        ejection (init stack) (append (last stack) strRPE)
    else 
        ((init stack), strRPE)


-- перевод входящей строки в обратную польскую нотацию
-- принимает исходную строку, стек (пустой) и конечную строку (пустой)
getReversePolishEntry :: [Char] -> [Char] -> [Char] -> [Char]
getReversePolishEntry [] [] strRPE = strRPE

getReversePolishEntry [] stack strRPE = do
    snd $ ejection stack strRPE

getReversePolishEntry (x:xs) stack strRPE = do
    let (s, str) = if (isOperators x operators) then 
                       operatorCheck x stack strRPE
                   else if (isClosingParenthesis x) then
                            ejection stack strRPE
                        else (stack, append x strRPE)
    getReversePolishEntry xs s str


-- возвращяает результат применения оператора к операндам
getValue :: Char -> [Double] -> [Double]
getValue op stack = 
    case op of
        '+' -> push s (z + y)
        '-' -> push s (z - y)
        '*' -> push s (z * y)
        '/' -> push s (z / y)
      where
        (y, z, s) = two_last_el stack


-- вычисление выражения в ОПН,
-- используется строка в ОПН и стек из Double,
-- возвращает Double
calc :: [Char] -> [Double] -> Double
calc [] stack = head stack
calc (x:xs) stack = do
    let s = if (isOperators x operators) then do
                getValue x stack
            else
                push stack (read [x] :: Double)
    calc xs s


--main :: IO ()
main = do
    input <- readFile "file.txt"
    putStrLn $ show $ calc (getReversePolishEntry input "" "") []
