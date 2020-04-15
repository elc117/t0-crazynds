

isBin :: String -> Bool
isBin [] = False
isBin (x:xs)
 | (x =='1' || x=='0' )&& length xs == 0 = True
 | (x =='1' || x=='0' ) = isBin xs
 |otherwise = False
 
isBin2 :: String -> Bool
isBin2 [] = False
isBin2 list = and [ x=='0' || x=='1' | x <- list ]

auxBin2Dec :: [Int] -> Int -> Int
auxBin2Dec [] x = 0
auxBin2Dec (x:xs) n = x*(2^n) + auxBin2Dec xs (n-1)

bin2dec :: [Int] -> Int
bin2dec [] = undefined
bin2dec bits = auxBin2Dec bits ((length bits)-1)

bin2dec2 :: [Int] -> Int
bin2dec2 [] = undefined
bin2dec2 bits = sum [ 2^(length bits-1-x) | x <- [0..length bits-1] , bits !! x == 1]

dec2bin :: Int -> [Int]
dec2bin 0 = [0]
dec2bin 1 = [1]
dec2bin val =
  let power = head [ x-1 | x <- [0..], 2^x > val ]
      maxVal = 2^power
      list = dec2bin (val-maxVal)
   in 1 : [ 0 | x <- [1..power-length list] ] ++ list 
   
isHex :: String -> Bool
isHex [] = False
isHex (x:xs)
 | (elem x "1234567890ABCDEF")&& length xs == 0 = True
 | (elem x "1234567890ABCDEF") = isHex xs
 |otherwise = False