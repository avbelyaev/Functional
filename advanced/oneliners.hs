module Oneliners where

-- какие-то однострочники, которые могут спросить на экзамене

reverseList [] = []
reverseList (x:xs) = (reverseList xs) ++ [x]

myInit [] = []
myInit lst = reverseList $ tail $ reverseList lst


myLast lst = head $ reverseList lst

tailHelper _ [] = []
tailHelper n lst =
	if 0 == n
	then tailHelper 1 lst
	else if n == length lst
		 then []
		 else [(lst !! n)] ++ tailHelper (n + 1) lst
		 

myTail lst = tailHelper 0 lst


myTail2 (x:xs) = xs


myRef n lst = 
	if 0 == n
	then head lst
	else myRef (n - 1) (tail lst)


myFilter _ [] = []
myFilter predicate lst =
	if predicate $ head lst
	then [(head lst)] ++ myFilter predicate (tail lst)
	else myFilter predicate (tail lst)

myFoldr f z []     = z 
myFoldr f z (x:xs) = f x (foldr f z xs)

myFoldl f z []     = z                  
myFoldl f z (x:xs) = foldl f (f z x) xs

prod 0 x = 0
prod x 0 = 0
prod x y = x*y



main = do
    print "start"

    let rev = reverseList [1, 2, 3, 4]
    print rev
    -- 4,3,2,1

    let t1 = myInit [1, 2, 3, 4]
    print t1
    -- 1,2,3

    let t2 = myLast t1
    print t2
    -- 4

    let t3 = myTail [1, 2, 3, 4]
    print t3
    -- 2,3,4

    let t4 = myTail2 [1, 2, 3, 4]
    print t4
    -- 2,3,4

    let t5 = myRef 3 [1, 2, 3, 4, 5]
    print t5
    -- 4

    let t6 = myFilter (\x -> (x `mod` 2) == 0) [1, 2, 3, 4, 5]
    print t6
    -- 2,4

    let t7 = myFoldl prod 1 [1, 2, 3, 4]
    print t7
    -- 24

    let t7 = myFoldr prod 1 [1, 2, 3, 4]
    print t7
    -- 24

