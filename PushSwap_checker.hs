import System.Environment
import Text.Read
import Data.Maybe
import Data.List
import Data.Char
import System.Exit

data ListID = A | B

type Lists = ([Int], [Int])

data Action = Sa | Sb | Sc
            | Pa | Pb
            | Ra | Rb | Rr
            | Rra | Rrb | Rrr deriving (Show, Eq, Read)

main :: IO ()
main = do
    args <- getArgs
    actions <- getLine
    let ints = getJusts $ strsToInts args
    let acts = getJusts $ strsToActs $ words actions
    case ints of
        Nothing -> exitWith $ ExitFailure 84
        Just x -> case acts of
            Nothing -> exitWith $ ExitFailure 84
            Just y -> printBool $ checkPushSwap $ transformLists (x,[]) y


--Only IO () called in the monad 
printBool :: Bool -> IO ()
printBool True = putStrLn "OK"
printBool False = putStrLn "KO"

--Fold de execAction
checkPushSwap :: Lists -> Bool
checkPushSwap (xs, []) = xs == sort xs
checkPushSwap (xs, ys) = False

--Fold de execAction to apply multiples transfo
transformLists :: Lists -> [Action] -> Lists
transformLists lists [] = lists
transformLists lists acts = foldl execAction lists acts

execAction :: Lists -> Action -> Lists
execAction (xs, ys) Sa = (swap xs, ys)
execAction (xs, ys) Sb = (xs, swap ys)
execAction (xs, ys) Sc = (swap xs, swap ys)
execAction lists Pa = pick lists A
execAction lists Pb = pick lists B
execAction (xs, ys) Ra = (rotate xs, ys)
execAction (xs, ys) Rb = (xs, rotate ys)
execAction (xs, ys) Rr = (rotate xs, rotate ys)
execAction (xs, ys) Rra = (rrotate xs, ys)
execAction (xs, ys) Rrb = (xs, rrotate ys)
execAction (xs, ys) Rrr = (rrotate xs, rrotate ys)

strToAct :: String -> Maybe Action
strToAct [] = Nothing
strToAct (x:xs) = readMaybe $ (toUpper x):xs

strsToActs :: [String] -> [Maybe Action]
strsToActs [] = []
strsToActs xs = map strToAct xs

strsToInts :: [String] -> [Maybe Int]
strsToInts [] = []
strsToInts xs = map readMaybe xs

getJusts :: [Maybe a] -> Maybe [a]
getJusts [] = Just []
getJusts xs
    | length xs == length res = Just (res)
    | otherwise = Nothing
    where res = catMaybes xs

-- actions for pushSwap

swap :: [Int] -> [Int]
swap [] = []
swap (x:[]) = [x]
swap (x:y:xs) = y:x:xs

pick :: Lists -> ListID -> Lists
pick (as, []) A = (as, [])
pick ([], bs) B = ([], bs)
pick (as, (b:bs)) A = ((b:as), bs)
pick ((a:as), bs) B = (as, (a:bs))

rotate :: [Int] -> [Int]
rotate [] = []
rotate (x:xs) = reverse $ x:(reverse xs)

rrotate :: [Int] -> [Int]
rrotate [] = []
rrotate xs = (last xs):(init xs)