import Control.Monad

main = do
    putStr "> "
    input <- getLine
    unless(input=="q") $ do
        let n = rpn input
        print n
        main


rpn :: String -> Double
rpn [] = 0
rpn x = head $ foldl rpn' [] $ words x

rpn' :: [Double] -> String -> [Double]
rpn' (x:y:ys) "*" = (x*y):ys
rpn' (x:y:ys) "-" = (x-y):ys
rpn' (x:y:ys) "+" = (x+y):ys
rpn' (x:y:ys) "/" = (y/x):ys
rpn' (x:y:ys) "^" = (y**x):ysa
rpn' (x:xs) "ln" = log x:xs
rpn' (x:xs) "!" = fac x:xs
rpn' (x:xs) "cos" = cos x:xs
rpn' (x:xs) "sin" = sin x:xs
rpn' (x:xs) "tan" = tan x:xs
rpn' (x:xs) "acos" = acos x:xs
rpn' (x:xs) "asin" = asin x:xs
rpn' (x:xs) "atan" = atan x:xs
rpn' (x:xs) "asinh" = asinh x:xs
rpn' (x:xs) "atanh" = atanh x:xs
rpn' (x:xs) "acosh" = acosh x:xs
rpn' (x:xs) "log" = log x:xs
rpn' (x:xs) "sqrt" = sqrt x:xs
rpn' ys n = (read n):ys


fac :: Double -> Double
fac 0 = 1
fac n = n * fac (n-1)