myElem :: Eq a => a -> [a] -> Bool
myElem elem [] = False
myElem elem (x:xs)
    | elem == x = True
    | otherwise = myElem elem xs

safeDiv :: Int -> Int -> Maybe Int
safeDiv x 0 = Nothing
safeDiv x y = Just (div x y)

safeNth :: [a] -> Int -> Maybe a
safeNth [] index = Nothing
safeNth (x:xs) 0 = Just x
safeNth (x:xs) index
    | index < 0 = Nothing
    | index > length (x:xs) = Nothing
    | otherwise = safeNth xs $ index - 1

safeSucc :: Maybe Int -> Maybe Int
safeSucc Nothing = Nothing
safeSucc x = fmap succ x

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup elem [] = Nothing
myLookup elem (x:xs)
    | elem == fst x = Just $ snd x
    | otherwise = myLookup elem xs

maybeDo :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo f Nothing _ = Nothing
maybeDo f _ Nothing = Nothing
maybeDo f x y = Just (f) <*> x <*> y

myIsDigit :: Char -> Bool
myIsDigit x 
    | x `elem` "0123456789" = True
    | otherwise = False

readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt xs = case all myIsDigit xs of
    True -> Just (read xs :: Int)
    False -> Nothing

getLineLength :: IO Int
getLineLength = getLine >>= return . length -- (\x -> return (length x))
getLineLength = fmap length getLine
getLineLength = length <$> getLine

printAndGetLength :: String -> IO Int
printAndGetLength xs = do
    putStrLn xs
    return (length xs)

printBox :: Int -> IO ()
printBox 0 = return ()
printBox x
    | x < 0 = return ()
    | otherwise = printSquare x

printSquare :: Int -> IO ()
printSquare x = do
    putStrLn ('+':edgeBox x)
    putIntermediatesLines $ getIntermediateLines (x - 2)
    putStrLn ('+':edgeBox x)

putIntermediatesLines :: [String] -> IO ()
putIntermediatesLines [] = return ()
putIntermediatesLines (x:xs) = do
    putStrLn x
    putIntermediatesLines xs

--Use a acc ?
getIntermediateLines :: Int -> [String]
getIntermediateLines 0 = []
getIntermediateLines x = ('|':(buildLine x)):getIntermediateLines (x - 1)

buildLine :: Int -> String
buildLine 0 = []
buildLine 1 = []
buildLine 2 = ['|']
buildLine x = ' ':buildLine (x - 1)

edgeBox :: Int -> String
edgeBox 1 = []
edgeBox 2 = ['+']
edgeBox x = '-':edgeBox (x - 1)