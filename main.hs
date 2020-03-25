import Control.Monad

main = do
    input <- getLine
    unless(input=="q") $ do
        let n = rpn input
        if 
        print n
        else
        main

parseText :: String -> Bool
parseText [] = []
parseText 

rpn :: String -> Double
rpn [] = 0
rpn x = head $ foldl rpn' [] $ words x

rpn' :: [Double] -> String -> [Double]
rpn' (x:y:ys) "*" = (x*y):ys
rpn' (x:y:ys) "-" = (x-y):ys
rpn' (x:y:ys) "+" = (x+y):ys
rpn' (x:y:ys) "/" = (y/x):ys
rpn' (x:y:ys) "^" = (y**x):ys
rpn' (x:xs) "ln" = log x:xs
rpn' ys n = (read n):ys


-- Couldn't match expected type ‘IO a0’ with actual type ‘Double’