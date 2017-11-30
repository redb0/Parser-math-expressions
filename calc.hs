import System.IO


-- Проверяет, является ли символ оператором.
--ПЕРЕДЕЛАЛ
isOperators :: Char -> Bool
isOperators op 
    | op `elem` ['+', '-', '*', '/', '('] = True
    | otherwise = False


-- Возвращает приоритет оператора.
--ПЕРЕДЕЛАЛ
getPriority :: Char -> Int
getPriority op =
    case op of 
        '(' -> 0
        '+' -> 1
        '-' -> 2
        '/' -> 3
        '*' -> 4


-- добавить элемент в стэк.
push :: [a] -> a -> [a]
push s x = s ++ [x]

-- Удалить последний элемент из стэка.
pup :: [a] -> [a]
pup [] = []
pup x = init x


getArgumets :: [a] -> (a, a, [a])
getArgumets stack = (last stack, last (init stack), init (init stack))


--ПЕРЕДЕЛАЛ
isClosingParenthesis :: Char -> Bool
isClosingParenthesis ')' = True
isClosingParenthesis x = False


-- Выталкивает из стэка операторы с высоким приоритетом
operatorCheck :: Char -> [Char] -> [Char] -> ([Char], [Char])
operatorCheck op [] strRPE = ((push [] op), strRPE)
operatorCheck op stack strRPE = do
    if (((getPriority (last stack)) > (getPriority op)) && (length stack > 0)) then do
        operatorCheck op (init stack) (push strRPE (last stack))
    else
        ((push stack op), strRPE)


--выталкивание операторов если встретилась закрывающая скобка
ejection :: [Char] -> [Char] -> ([Char], [Char])
ejection [] strRPE = ([], strRPE)
ejection stack strRPE = 
    if (getPriority (last stack) /= 0) then
        ejection (init stack) (push strRPE (last stack))
    else 
        ((init stack), strRPE)


stringInRPE :: [Char] -> [Char] -> [Char] -> [Char]
stringInRPE [] [] strRPE = strRPE
stringInRPE [] stack strRPE = do snd $ ejection stack strRPE
stringInRPE (x:xs) stack strRPE = do
    let (s, str) = if (isOperators x) then 
                       operatorCheck x stack strRPE
                   else if (isClosingParenthesis x) then
                            ejection stack strRPE
                        else (stack, push strRPE x)
    stringInRPE xs s str


getResultOperation :: Char -> [Double] -> [Double]
getResultOperation op stack = 
    case op of
        '+' -> push s (y + z)
        '-' -> push s (y - z)
        '*' -> push s (y * z)
        '/' -> push s (y / z)
      where
        (y, z, s) = getArgumets stack


calculation :: [Char] -> [Double] -> Double
calculation [] stack = head stack
calculation (x:xs) stack = do
    let s = if (isOperators x) then do
                getResultOperation x stack
            else
                push stack (read [x] :: Double)
    calculation xs s


--ПЕРЕДЕЛАЛ
main = do
    putStrLn "Введите математическое выражение: "
    str <- getLine
    putStrLn $ show $ calculation (stringInRPE str "" "") []
