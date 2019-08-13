import Text.Printf (printf)
import Data.List
import Data.Maybe

---------------------------------------------------------------------
-- Digit
---------------------------------------------------------------------
newtype Digit = D { unD :: [Bool] }
  deriving Eq

showHT :: Bool -> String
showHT True  = "__"
showHT False = "  "

showHM :: Bool -> String
showHM True  = "--"
showHM False = "  "

showHB :: Bool -> String
showHB True  = "‾‾"
showHB False = "  "

showV :: Bool -> String
showV True  = "|"
showV False = " "

instance Show Digit where
  show (D xs) = showBools xs

showBools :: [Bool] -> String
showBools xs  = printf " %s \n"   (showHT a)
             <> printf "%s  %s\n" (showV b) (showV c)
             <> printf " %s \n"   (showHM d)
             <> printf "%s  %s\n" (showV e) (showV f)
             <> printf " %s \n"   (showHB g)
   where
    [a,b,c,d,e,f,g] = take 7 xs

mkDigit :: [Int] -> Digit
mkDigit is = D [ x `elem` is | x <- [0..6]]

--   _    0
--  | |  1 2
--   -    3
--  | |  4 5
--   _    6

digitToD :: Int -> Digit
digitToD 0  = mkDigit [0,1,2,  4,5,6]
digitToD 1  = mkDigit [    2,    5  ]
digitToD 2  = mkDigit [0,  2,3,4,  6]
digitToD 3  = mkDigit [0,  2,3,  5,6]
digitToD 4  = mkDigit [1,  2,3,  5  ]
digitToD 5  = mkDigit [0,1,  3,  5,6]
digitToD 6  = mkDigit [0,1,  3,4,5,6]
digitToD 7  = mkDigit [0,2,      5  ]
digitToD 8  = mkDigit [0,1,2,3,4,5,6]
digitToD 9  = mkDigit [0,1,2,3,  5,6]
digitToD 11 = mkDigit [1,2,    4,5  ]
digitToD _  = undefined

strToD :: String -> Digit
strToD x
  | x `elem` xs = digitToD $ read x
  | otherwise = undefined
 where
  xs = show <$> (11:[0..9] :: [Int])

isValidDigit :: [Bool] -> Bool
isValidDigit xs = xs `elem` ys
 where
  ys = unD . digitToD <$> (11:[0..9])

getDititInt :: Digit -> Maybe Int
getDititInt d = lookup d vlds
 where
  is = 11:[0..9]
  vlds = map (\x -> (digitToD x, x)) is

---------------------------------------------------------------------
-- Number
---------------------------------------------------------------------
newtype Numbr = N {unN :: [Digit]}
 deriving Eq

instance Show Numbr where
  show (N xs) =
   let
    x = replicate 5 ""
    ls = lines . show
    pp i j = i ++ " " ++ j
   in
    unlines $ foldr (zipWith pp . ls) x xs

mkNumbr :: Int -> Numbr
mkNumbr = N . fmap strToD . go . show
 where
  go :: String -> [String]
  go = foldr f []
  f '1' ("1":ys) = "11" : ys
  f x   xs       = [x] : xs

isValidNum :: Numbr -> Bool
isValidNum (N xs) = all (isValidDigit . unD) xs

getNumInt :: Numbr -> Maybe Int
getNumInt (N xs)
  = fmap (read . concatMap show)
  $ sequence$ getDititInt <$> xs

diffNum :: Numbr -> Numbr -> String
diffNum a b = zipWith f (show a) (show b)
 where
  f '\n' _ = '\n'
  f ' ' _  = ' '
  f x y | x == y = x
        | otherwise = '*'

---------------------------------------------------------------------
-- move the sitcks
---------------------------------------------------------------------

-- FIXME:: use lens ~. for update selected element (or not)
-- update value at specific index
updAt :: Int -> a -> [a] -> [a]
updAt i v ys = take i ys ++ v : drop (i+1) ys

-- remove specific stick from a digit, unchanged if already none
rmeStick :: Int -> Digit -> Digit
rmeStick j (D ys) = D $ updAt j False ys

-- add specific stick from a digit, unchanged if already exists
addStick :: Int -> Digit -> Digit
addStick j (D ys) = D $ updAt j True ys

-- possible outcomes of removing one stick from a Digit
rem1Sd :: Digit -> [Digit]
rem1Sd d@(D xs) = [rmeStick i d | i <- ixs]
 where
  ixs = elemIndices True xs  -- indices of sticks

-- possible outcomes of adding one stick to a Digit
add1Sd :: Digit -> [Digit]
add1Sd d@(D xs)
  | null ixs  = [d]                       -- 8 have noting to added
  | otherwise = [addStick i d | i <- ixs]
 where
  ixs = elemIndices False xs              -- indices of no sticks

-- possible outcomes of add/remove one stick to/from a Number
mod1Sn :: (Digit -> [Digit]) -> Numbr -> [Numbr]
mod1Sn f (N xs) = do
  x <- xs
  y <- f x
  let i = elemIndex x xs
  let os = updAt (fromJust i) y xs
  pure (N os)

-- possible outcomes of one stick removed from a Number
rem1Sn :: Numbr -> [Numbr]
rem1Sn = mod1Sn rem1Sd

-- possible outcomes of one stick added to a Number
add1Sn :: Numbr -> [Numbr]
add1Sn = mod1Sn add1Sd

-- FIXME: implement swap (add . rem /=? rem .add =/? swap)

---------------------------------------------------------------------
-- That specific number to flip for
---------------------------------------------------------------------
thatNum :: Numbr
thatNum = mkNumbr 5008

---------------------------------------------------------------------
-- IOs
---------------------------------------------------------------------
main :: IO ()
main = do
  print $ fromJust $ getNumInt thatNum
  print thatNum

  let m =  maximum <$> nub $ filter isJust
        $ map getNumInt
        -- take 2 sticks out and put 2 sticks back
        $ pure thatNum >>= rem1Sn >>= rem1Sn >>= add1Sn >>= add1Sn
  print $ fromJust m
  let mx = mkNumbr $ fromJust m
  print mx

  putStrLn $ diffNum thatNum mx
  putStrLn $ diffNum mx thatNum
