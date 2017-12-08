import System.IO
import Text.Printf --для вывода с округлением
import Data.Char (ord) --для создания белого списка символов

-- Парсер математических выражений
-- Распознает бинарные операторы (+, -, *, /) и скобки
-- Выражение читается из файла "file.txt"
-- Выражение должно быть записано в одну строку
-- При неправильной расстановке скобок выводиться ошибка
-- При наличии в выражении символов кроме чисел, точки, склбок и операторов +, -, *, / выводиться оибка
-- При неправильной расстановке операторов (Например "2++1") выводиться ошибка 


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


-- выталкивает из стэка операторы с большим либо таким же приоритетом, чем у текущего
-- или записывает его в стэк
operatorCheck :: Char -> [Char] -> [Char] -> ([Char], [Char])
operatorCheck op [] strRPE = ((push [] op), strRPE)
operatorCheck op stack strRPE = do
    if (((getPriority priorities (last stack)) >= (getPriority priorities op)) && (length stack > 0) && (getPriority priorities op /= 0)) then do
        operatorCheck op (init stack) ((append (last stack) strRPE) ++ " ")
    else
        ((push stack op), strRPE)


-- набор операторов и приоритетов
operators = ['+', '-', '*', '/', '(']
priorities = [('+', 1), ('-', 1), ('/', 2), ('*', 2), ('(', 0)]


-- выталкивание операторов если встретилась закрывающая скобка
-- и удаление открывающей скобки из стека
-- возвращает тапл из двух символьных списков (строк)
ejection :: [Char] -> [Char] -> ([Char], [Char])
ejection [] strRPE = ([], strRPE)
ejection stack strRPE = 
    if (getPriority priorities (last stack) /= 0) then
        ejection (init stack) ((append (last stack) strRPE) ++ " ")
    else 
        ((init stack), strRPE)

inTuple :: ([a], [a]) -> [a] -> ([a], [a], [a])
inTuple x y = do
    (b, c, y)
      where 
        b = fst x
        c = snd x

inTuple2 :: [Char] -> ([Char], [Char]) -> ([Char], [Char], [Char])
inTuple2 x y = do
    (x, c ++ " ", b)
      where 
        b = fst y
        c = snd y

-- перевод входящей строки в обратную польскую нотацию
-- принимает исходную строку, стек (пустой) и конечную строку (пустой)
getReversePolishEntry :: [Char] -> [Char] -> [Char] -> [Char]
getReversePolishEntry [] [] strRPE = strRPE

getReversePolishEntry [] stack strRPE = do
    snd $ ejection stack strRPE

getReversePolishEntry (x:xs) stack strRPE = do
    let (s, str, ar) = if (isOperators x operators) then 
                           inTuple (operatorCheck x stack strRPE) xs
                       else if (isClosingParenthesis x) then
                                inTuple (ejection stack strRPE) xs
                            else do
                                inTuple2 stack (checkNumber xs (append x strRPE))
    getReversePolishEntry ar s str


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


--проверка корректности расстановки операторов
validation :: [Char] -> Int -> Bool
validation [] n = if (n == 1) then True else False
validation (x:xs) n = 
    if ((i <= -1) || (i >= 2)) then False else validation inStr i
    where 
        (inStr, i) = if ((isOperators x operators) && (x /='(')) then (xs, n - 1)
                     else if ((x /= '(') && (x /= ')')) then do
                          let (inStr, munb) = checkNumber xs ""
                          (inStr, n + 1)
                     else (xs, n)


--поиск чисел из несколькиз цыфр
checkNumber :: [Char] -> [Char] -> ([Char], [Char])
checkNumber [] strRPE = ([], strRPE)
checkNumber (x:xs) strRPE = 
    -- x уже проверено и это число
    -- необходимо просмотреть строку дальше на предмет чисел
    if ((not (isOperators x operators)) && (x /= '(') && (x /= ')') && (x /= ' ')) then do
        checkNumber xs (append x strRPE)
    else
        if (x == ' ') then 
            (xs, strRPE)
        else
            (([x] ++ xs), strRPE)

-- вычисление выражения в ОПН,
-- используется строка в ОПН и стек из Double,
-- возвращает Double
calc :: [Char] -> [Double] -> Double
calc [] stack = head stack
--calc [] stack = (fromIntegral f / 100)
--    where
--        f = round ((head stack) * 100)
calc (x:xs) stack = do
    let (s, ar) = if (isOperators x operators) then do
                      ((getValue x stack), xs)
                  else do
                      if (x == ' ') then 
                          (stack, xs)
                      else
                          ((push stack (read numb :: Double)), array) --Double
                            where
                              (array, numb) = checkNumber xs [x] 
    calc ar s


-- проверка на корректность расстановки скобок
checkSequenceParentheses :: [Char] -> Int -> Bool
checkSequenceParentheses [] n = if (n == 0) then True else False
checkSequenceParentheses (x:xs) n = 
    if ((x == ')') && (n == 0)) then
        False
    else do
        let b = if (x == '(') then n + 1
                else if (x == ')') then n - 1
                     else n
        checkSequenceParentheses xs b

--проверка на наличие букв и посторонних символов
isNotLetter :: [Char] -> Bool
isNotLetter [] = True
isNotLetter (x:xs) =
    --`elem` "qwertyuiopasdfghjklzxcvbnm[]{};',йцукенгшщзхъфывапролджэячсмитьбюQWERTYUIOPASDFGHJKLZXCVBNMЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖЭЯЧСМИТЬБЮ"
    if (((((ord x) < 40) && ((ord x) > 43)) && ((ord x) < 45)) || ((ord x) > 57)) then
        False
    else 
        isNotLetter xs


--main :: IO ()
main = do
    input <- readFile "file.txt"
    if not (checkSequenceParentheses input 0) then
        putStrLn ("Некорректно расставлены скобки")
    else if not (isNotLetter input) then
        putStrLn ("Введен недопустимый символ")
    else if not (validation input 0) then
        putStrLn ("Некорректно расставлены знаки")
    else do
        let f = getReversePolishEntry input "" ""
        --printf "%.2f\n" (calc f [])
        putStrLn $ show $ calc f []
          --where
          --  f = getReversePolishEntry input "" ""
