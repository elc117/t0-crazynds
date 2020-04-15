add10toAll :: [Int] -> [Int]
add10toAll list = [x+10 | x <- list]

multN :: Int -> [Int] -> [Int]
multN n list = [x*n | x <- list]

applyExpr :: [Int] -> [Int]
applyExpr list = [3*x+2 | x <- list]

addSuffix :: String -> [String] -> [String]
addSuffix suf strs = [x++suf | x <- strs]

selectgt5 :: [Int] -> [Int]
selectgt5 list = [ x | x<- list , x>5]

sumOdds :: [Int] -> Int
sumOdds list = sum [ x | x <- list , x`mod`2==1]

selectExpr :: [Int] -> [Int]
selectExpr list = [x | x <- list , x`mod`2==0 , x<50, x>20]

countShorts :: [String] -> Int
countShorts list = length [x | x <- list , length x <5 ]

calcExpr :: [Float] -> [Float]
calcExpr list = [ x^2/2 | x <-list , x^2/2>10 ]

trSpaces :: String -> String
trSpaces str = [ if x==' ' then '-' else x | x <- str]

selectSnd :: [(Int,Int)] -> [Int]
selectSnd list = [ snd x | x <-list]

dotProd :: [Int] -> [Int] -> Int
dotProd l1 l2 = sum [fst x * snd x | x <- zip l1 l2]

genRects :: Int -> (Int,Int) -> [(Float,Float,Float,Float)]
genRects n (px,py) = 
 let size = 5.5
     posx = intToFloat px
     posy = intToFloat py
  in [ ( posy + (intToFloat x )*size  ,posy, 5.5 , 5.5) | x <- [0..n-1] ]

