module Hallo where

gcd' a b 
	| b == 0 	= a
	| otherwise = gcd' b (mod a b)


lcm' a b = (a * b) `div` gcd' a b


isPrime num = helper num 2
    where
      helper num x
            | x == num 			= True
            | 0 == num `mod` x	= False
            | otherwise 		= helper num (x + 1)


main = getLine >>= wrapper >>= putStrLn

average xs = sum xs / n
	where n = fromIntegral $ length xs

-- words: "1 2 3" -> [1, 2, 3]
-- read converts String -> int (int выводится компилятором)
-- map f []. f == read, [] == words

-- read from right to left
wrapper = return . show . average . map read . words



-- basics
quadratic​ a b c = (x1, x2)
  ​where​ d=b^ ​2​ - ​4​ *a*c
        sd = sqrt d
        x1=(-b-sd)/(​2​ *a)
        x2=(-b+sd)/(​2​ *a)

quadratic​2 a b c =
​  let​ d=b^ ​2​ - ​4​ *a*c
      sd = sqrt d
      x1=(-b-sd)/(​2​ *a)
      x2=(-b+sd)/(​2​ *a)
  ​in​ (x1, x2)

