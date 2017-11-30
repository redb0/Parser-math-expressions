elem :: Eq a => a -> [a] -> Bool

elem x = foldr ((||) . (x ==)) False

--charFound c s = elem c s