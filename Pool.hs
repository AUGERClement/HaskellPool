mySucc :: Int -> Int
mySucc x = x + 1

myIsNeg :: Int -> Bool
myIsNeg x
    | (x < 0) = True
    | otherwise = False

myAbs :: Int -> Int
myAbs x
    | myIsNeg x = (-1) * x
    | otherwise = x

myMin :: Int -> Int -> Int
myMin x y
    | (x < y) = x
    | (x > y) = y
    | otherwise = x

myMax :: Int -> Int -> Int
myMax x y
    | (x < y) = y
    | (x > y) = x
    | otherwise = x

myTuple :: a -> b -> (a, b)
myTuple x y = (x, y)

myTruple :: a -> b -> c -> (a, b, c)
myTruple x y z = (x, y, z)

myFst :: (a, b) -> a
myFst (x, y) = x

mySnd :: (a, b) -> b
mySnd (x, y) = y

mySwap :: (a, b) -> (b, a)
mySwap (a, b) = (b, a)

myHead :: [a] -> a
myHead [] = error "Empty list"
myHead (x:_) = x

myTail :: [a] -> [a]
myTail [] = error "Empty List"
myTail (x:xs) = xs

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myNth :: [a] -> Int -> a
myNth [] index = error "Empty List"
myNth (x:xs) 0 = x 
myNth (x:xs) index
    | index < 0 = error "Negative Index"
    | index > myLength (x:xs) = error "Index greater than list size"
    | otherwise = myNth xs $ index - 1

myTake :: Int -> [a] -> [a]
myTake n [] = []
myTake 0 _ = []
myTake n (x:xs)
    | (n < 0) = error "Negative N"
    | otherwise = x:myTake (n - 1) xs

myDrop :: Int -> [a] -> [a]
myDrop n [] = []
myDrop 0 xs = xs
myDrop n (x:xs)
    | (n < 0) = error "Negative N"
    | otherwise = myDrop (n - 1) xs

myAppend :: [a] -> [a] -> [a]
myAppend [] ys = ys
myAppend xs [] = xs
myAppend (x:xs) ys = x:(myAppend xs ys)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myAppend (myReverse xs) [x]

myInit :: [a] -> [a]
myInit [] = error "Empty List"
myInit xs =  reverse $ tail $ reverse xs

myLast :: [a] -> a
myLast [] = error "Empty List"
myLast (x:xs)
    | null xs = x
    | otherwise = myLast xs

myZip :: [a] -> [b] -> [(a, b)]
myZip [] ys = []
myZip xs [] = []
myZip (x:xs) (y:ys) = (x, y):myZip xs ys

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip xs = (myMap myFst xs, myMap mySnd xs)

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = f x:myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (x:xs) 
    | f x = x:myFilter f xs
    | otherwise = myFilter f xs

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f acc [] = acc
myFoldr f acc xs = myFoldr f (f (myLast xs) acc) $ myInit xs

mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan f [] = ([], [])
mySpan f xs = ((myFilter f xs), (myFilter (not.f) xs))

myQuickSort :: (a -> a -> Bool) -> [a] -> [a]
myQuickSort f [] = []
myQuickSort f (x:xs) = myAppend smaller $ myAppend [x] bigger
    where
        cmp = f x --Partial application
        smaller = myQuickSort f $ myFilter cmp xs
        bigger = myQuickSort f $ myFilter (not.cmp) xs