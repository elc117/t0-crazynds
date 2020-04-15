
ends :: [Int] -> [Int]
ends list = head list : last list : []
ends2 :: [Int] -> [Int]
ends2 list = [head list ,last list]


deduzame :: [Integer] -> [Integer]
deduzame [] = []
deduzame (x:xs) = (2 * x) : deduzame (xs)

deduzame2 :: [Integer] -> [Integer]
deduzame2 [] = []
deduzame2 (x:xs) 
 | x > 2 = x : deduzame2 xs
 | otherwise = deduzame2 xs

geraTabela :: Int -> [(Int,Int)]
geraTabela 1 = [(1,1)]
geraTabela n = (n,n^2) : geraTabela (n-1)

contido :: Char -> [Char] -> Bool
contido c [] = False
contido c (x:xs)
 | c==x = True
 | otherwise = contido c xs

translate :: [(Float,Float)] -> [(Float,Float)]
translate [] = []
translate (x:xs) = (fst x + 2 , snd x + 2) : translate xs

countLongs :: [String] -> Int
countLongs [] = 0
countLongs (x:xs)
 | length x > 5 = 1 + countLongs xs
 | otherwise = countLongs xs

onlyLongs :: [String] -> [String]
onlyLongs [] = []
onlyLongs (x:xs)
 | length x > 5 = x : onlyLongs xs
 | otherwise = onlyLongs xs

