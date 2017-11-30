import System.IO


-- проверка на оператор
isOperators :: Char -> Bool
isOperators op 
    | op `elem` ['+', '-', '*', '/', '('] = True
    | otherwise = False


-- приоритеты
getPriority :: Char -> Int
getPriority op =
    case op of 
        '(' -> 0
        '+' -> 1
        '-' -> 2
        '/' -> 3
        '*' -> 4


push :: [a] -> a -> [a]
push s x = s ++ [x]


pup :: [a] -> [a]
pup [] = []
pup x = init x


-- получить аргументы для операции
getArgumets :: [a] -> (a, a, [a])
getArgumets stack = (last stack, last (init stack), init (init stack))


isClosingBracket :: Char -> Bool
isClosingBracket ')' = True
isClosingBracket x = False


--выталкивание из стэка операторов с высоким приоритетом
operatorCheck :: Char -> [Char] -> [Char] -> ([Char], [Char])
operatorCheck op [] strRPE = ((push [] op), strRPE)
operatorCheck op stack strRPE = do
    if (((getPriority (last stack)) > (getPriority op)) && (length stack > 0)) then do
        operatorCheck op (init stack) (push strRPE (last stack))
    else
        ((push stack op), strRPE)


--выталкивание операторов если встретился ')'
ejection :: [Char] -> [Char] -> ([Char], [Char])
ejection [] strRPE = ([], strRPE)
ejection stack strRPE = 
    if (getPriority (last stack) /= 0) then
        ejection (init stack) (push strRPE (last stack))
    else 
        ((init stack), strRPE)


-- конвертация в обратную польскую запись
stringInRPE :: [Char] -> [Char] -> [Char] -> [Char]
stringInRPE [] [] strRPE = strRPE
stringInRPE [] stack strRPE = do snd $ ejection stack strRPE
stringInRPE (x:xs) stack strRPE = do
    let (s, str) = if (isOperators x) then 
                       operatorCheck x stack strRPE
                   else if (isClosingBracket x) then
                            ejection stack strRPE
                        else (stack, push strRPE x)
    stringInRPE xs s str


-- результат применения оператора
getResultOperation :: Char -> [Double] -> [Double]
getResultOperation op stack = 
    case op of
        '+' -> push s (x + y)
        '-' -> push s (x - y)
        '*' -> push s (x * y)
        '/' -> push s (x / y)
      where
        (x, y, s) = getArgumets stack


-- вычисление выражение
calculation :: [Char] -> [Double] -> Double
calculation [] stack = head stack
calculation (x:xs) stack = do
    let s = if (isOperators x) then do
                getResultOperation x stack
            else
                push stack (read [x] :: Double)
    calculation xs s


main = do
    putStrLn "Введите математическое выражение: "
    str <- getLine
    putStrLn $ show $ calculation (stringInRPE str "" "") []
