-- AFBF + CGHB + DAFG + AEAB = BCDC
--

import Control.Monad
import Data.List     (nub)
import Text.Printf   (printf)

-- all possible 10^8 combinations [0,0...] to [9,9...]
choices :: [[Int]]
choices = replicateM 8 [0..9]

-- no duplicated value in the list
-- i.e. A - H have distinct value
distincts :: [[Int]]
distincts = do
  ls <- choices
  guard $ nodub ls
  pure ls
 where
  nodub :: Eq a => [a] -> Bool
  nodub xs = nub xs == xs

conv :: [Int] -> Int
conv = read . (=<<) show

_afbf,_cghb,_dafg,_aeab,_bcdc :: [Int] -> Int
_afbf [a,b,_,_,_,f,_,_] = conv [a,f,b,f]
_afbf _ = undefined
_cghb [_,b,c,_,_,_,g,h] = conv [c,g,h,b]
_cghb _ = undefined
_dafg [a,_,_,d,_,f,g,_] = conv [d,a,f,g]
_dafg _ = undefined
_aeab [a,b,_,_,e,_,_,_] = conv [a,e,a,b]
_aeab _ = undefined
_bcdc [_,b,c,d,_,_,_,_] = conv [b,c,d,c]
_bcdc _ = undefined

-- AFBF + CGHB + DAFG + AEAB = BCDC
gen :: [[Int]]
gen = do
  ls <- distincts
  let [afbf,cghb,dafg,aeab,bcdc]
       = fmap ($ ls) [_afbf,_cghb,_dafg,_aeab,_bcdc]
  guard $ afbf + cghb + dafg + aeab == bcdc
  pure ls

-- no leading zero
gen4 :: [[Int]]
gen4 = do
  ls@[a,_,c,d,_,_,_,_] <- gen
  guard $ a /= 0
  guard $ c /= 0
  guard $ d /= 0
  pure ls

pp :: [Int] -> String
pp ls@[a,b,c,d,e,f,g,h]
  = printf "A %i, B %i, C %i, D %i, E %i, F %i, G %i, H %i\n"
      a b c d e f g h
 <> printf "AFBF + CGHB + DAFG + AEAB = BCDC\n"
 <> printf "%i + %i + %i + %i = %i\n"
      afbf cghb dafg aeab bcdc
 where
  [afbf,cghb,dafg,aeab,bcdc]
       = fmap ($ ls) [_afbf,_cghb,_dafg,_aeab,_bcdc]

pp _ = undefined

main :: IO ()
main = mapM_ (putStrLn . pp) gen4
