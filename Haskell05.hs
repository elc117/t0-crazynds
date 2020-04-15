import Data.Char
import Text.Printf


func1 :: [Int] -> Bool
func1 lst = expr1 > expr2
  where expr1 = head lst + 20
        expr2 = last lst * 2
		
func2 :: String -> Bool
func2 str = 
  let digits = map digitToInt str
      sumdigits = sum digits
   in sumdigits > 50


testaString0 :: IO()
testaString0 = do
 str <- getLine
 putStrLn $ "Ola seu "++str

testaString :: IO()
testaString = do
 str <- getLine
 putStrLn . show $ (length str)>5
 


 
isCpfOk :: [Int] -> Bool
isCpfOk cpf = 
  let -- calcula primeiro digito
      digitos1 = take 9 cpf
      expr1 = (sum $ zipWith (*) digitos1 [10,9..2]) `mod` 11
      dv1 = if expr1 < 2 then 0 else 11-expr1

      -- calcula segundo digito
      digitos2 = digitos1 ++ [dv1]
      expr2 = (sum $ zipWith (*) digitos2 [11,10..2]) `mod` 11
      dv2 = if expr2 < 2 then 0 else 11-expr2
   in dv1 == cpf !! 9 && dv2 == cpf !! 10

isCpf :: IO()
isCpf = do
 cpf <- getLine
 let digitos = (map digitToInt cpf)
     result = isCpfOk digitos
 putStrLn (show result)
 
type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type Color     = (Int,Int,Int)
type RectComplete = (Rect,Color)




-------------------------------------------------------------------------------
-- Strings SVG
-------------------------------------------------------------------------------

svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 

svgEnd :: String
svgEnd = "</svg>"

svgRect :: Rect -> Color -> String 
svgRect ((x,y),w,h) color = 
  printf "<rect x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='%s' />\n" x y w h style
  where style = svgStyle color
  
svgStyle :: Color -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d); mix-blend-mode: screen;" r g b

intToFloat :: Int -> Float
intToFloat i = fromInteger $ toInteger i

createRec :: Int -> RectComplete
createRec val = (((x,y),h,w),(r,g,b))
  where x = fromInteger $ toInteger ((val*11)`mod`800) 
        y = fromInteger $ toInteger ((val*117)`mod`800) 
        h = fromInteger $ toInteger ((val*22)`mod`230) 
        w = fromInteger $ toInteger ((val*7)`mod`175) 
        r = (val*67)`mod`256
        g = (val*33)`mod`256
        b = (val*15)`mod`256

criaExemplo :: IO ()
criaExemplo = do
  writeFile "rects.svg" $ svgstrs
  where svgstrs = svgBegin w h ++ svgfigs ++ svgEnd
        svgfigs = concat (map (\r -> svgRect (fst r) (snd r)) rets )
        rets = map createRec $ map (*573) [3,7..220]
        (w,h) = (1000,1000) -- width, height da imagem SVG


